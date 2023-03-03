module Handler.EventSeries where

import Import hiding (concat, length, zip, undefined, trace, (.), (++))
import Data.Time
import Data.Time.Calendar
import Data.Either
import qualified Text.Read as TR
import qualified Data.Text as T
import qualified Data.List as L
import Safe
import Debug.Trace

sequenceAL :: Applicative f => [f a] -> f [a]
sequenceAL [] = pure []
sequenceAL (fe:fes) = (:) <$> fe <*> sequenceAL fes

uniques :: (Ord a) => [a] -> [a]
uniques = L.concat . ((L.take 1) <$>) . L.group . L.sort

zipSelf :: [a] -> [(a, a)]
zipSelf l = zip l l

nextFirstOrDefault :: (Ord a) => [a] -> a -> a -> a
nextFirstOrDefault [] _ defaultVal = defaultVal
nextFirstOrDefault values@(v:_) reference _ =
  case L.find (>= reference) values of
    Nothing -> v
    (Just val) -> val

packRefEither :: (String, Either String a) -> (Text, Either Text a)
packRefEither (s, (Right v)) = (T.pack s, Right v)
packRefEither (s, (Left m)) = (T.pack s, Left $ T.pack m)

expandRefLeft :: (Text, Either Text a) -> Either Text a
expandRefLeft (_, r@(Right _)) = r
expandRefLeft (t, (Left m)) = Left $ m <> T.pack ": \xab" <> t <> T.pack "\xbb"

readEither :: Read a => Text -> Either Text a
readEither t =
  let s = T.unpack t
      v = TR.readEither s
      convert (Right r) = Right r
      convert (Left l) = Left $ T.pack l
  in convert v

{-

`SeriesSelector` and the following parser implement a sort of simplified cron syntax.  This syntax skips the minute and hour fields, currently doesn't allow ranges, and requires the DayOfWeek field to be encoded Data.Time.Calendar.DayOfWeek values (e.g. "Thursday").  As in cron, specifying the DayOfWeek field is additive to what is specified in the DayOfMonth field.

-}

data SeriesSelector = SeriesSelector [MonthOfYear] [DayOfMonth] [DayOfWeek]
  deriving (Show)

parseSeriesSelector :: Text -> Either Text SeriesSelector
parseSeriesSelector encoded = case components of
  encodedDaysOfMonth : encodedMonths : encodedDaysOfWeek : [] ->
    let comma = T.pack ","
        asterisk = T.pack "*"
        slash = T.pack "/"
        monthsLT = T.splitOn comma encodedMonths
        monthsEL
          | encodedMonths == asterisk = Right [1..12]
          | otherwise = readAllEither monthsLT
        daysOfMonthLT = T.splitOn comma encodedDaysOfMonth
        daysOfMonthEL
          | encodedDaysOfMonth == slash = Right []
          | otherwise = readAllEither daysOfMonthLT
        daysOfWeekLT = T.splitOn comma encodedDaysOfWeek
        daysOfWeekEL
          | encodedDaysOfWeek == slash = Right []
          | otherwise = readAllEither daysOfWeekLT
    in result monthsEL daysOfMonthEL daysOfWeekEL
      where result (Left l) _ _ = Left l
            result _ (Left l) _ = Left l
            result _ _ (Left l) = Left l
            result (Right m) (Right d0) (Right d1) =
              Right $ SeriesSelector m d0 d1
  _ -> Left $ T.pack $ "Error: " ++ show (length components)
              ++ " components in series selector."
  where components = T.words encoded
        readAllEither l = uniques <$> (sequenceAL $ expandRefLeft
                                      <$> (readEither <$>) <$> zipSelf l)

increments
  :: [Int] -- | The starting points for each interwoven series; make sure this is not an empty list!
  -> Int -- | How much to add to each starting point each time
  -> [Int] -- | An infinite series of Ints formed by incrementing the starting points
increments starts augment =
  concat $ iterate ((augment +) <$>) starts

repeatDaySeriesAnchored
  :: Day -- | The first day of the series
  -> Text -- | The pattern of repeating events for the series.  E.g. "xxDDxxxxxD" means that including the first day of the series, this event series has a ten-day repeating cycle where events happen on the third, fourth, and tenth days of the cycle. "D"s represent event days, any other character represents a non-event day.  If the pattern contains no "D"s, then one will be appended!
  -> Day -- | The reference point past which to look for new events in the series (e.g. today)
  -> [Day] -- | Infinite sequence of Days following the reference day and matching the pattern.
repeatDaySeriesAnchored first pattern reference =
  let pattern'
        | T.elem 'D' pattern = pattern
        | otherwise = T.snoc pattern 'D'
      breakpoint = mod (fromIntegral $ diffDays reference first)
                       $ T.length pattern'
      adjustedPattern = T.append (T.drop breakpoint pattern')
                                 $ T.take breakpoint pattern'
      offsets = (T.length . fst) <$> T.breakOnAll (T.pack "D") adjustedPattern
  in (((flip addDays) reference) . toInteger)
       <$> increments offsets (T.length pattern')

-- maybe call this daysOfMonthForDayOfWeek ?
-- maybe drop the referenceDay argument?
dayOfWeekLaterInMonth :: DayOfWeek -> Day -> [DayOfMonth]
dayOfWeekLaterInMonth dayOfWeek referenceDay =
  let resultDay = firstDayOfWeekOnAfter dayOfWeek referenceDay
      (_, refmonth, refday) = toGregorian referenceDay
      (resyear, resmonth, resday) = toGregorian resultDay
      resultL
        | resmonth /= refmonth = []
        | (gregorianMonthLength resyear resmonth) == resday = []
        | otherwise = resday : (dayOfWeekLaterInMonth dayOfWeek
                               $ addDays 1
                               $ fromGregorian resyear resmonth resday)
  in resultL

repeatDaySeries' :: [MonthOfYear] -> [DayOfMonth] -> [DayOfWeek]
                    -> Day -> [Day]
repeatDaySeries' monthL dayOfMonthL dayOfWeekL referenceDay =
  let (refyear, refmonth, refday) = toGregorian referenceDay
      resmonth = case L.find (>= refmonth) monthL of
        (Just m) -> m
        Nothing -> headDef refmonth monthL
      resyear
        | resmonth < refmonth = refyear + 1
        | otherwise = refyear
      searchday
        | resmonth == refmonth = refday
        | otherwise = 1
      dayOfWeekDayL = L.concat $ ((flip dayOfWeekLaterInMonth)
                                 $ fromGregorian resyear resmonth searchday)
                                 <$> dayOfWeekL
      possibleDayL = uniques $ dayOfMonthL ++ dayOfWeekDayL
      tryNextMonth = repeatDaySeries' monthL dayOfMonthL dayOfWeekL
                     $ addGregorianMonthsClip 1
                     $ fromGregorian resyear resmonth 1
  in case L.find (>= searchday) possibleDayL of
       (Just md) -> case fromGregorianValid resyear resmonth md of
         (Just day) -> day : repeatDaySeries' monthL dayOfMonthL dayOfWeekL
                             (addDays 1 day)
         Nothing -> tryNextMonth
       Nothing -> tryNextMonth

repeatDaySeries
  :: Text -- | The encoded SeriesSelector
  -> Day -- | Show Days in the series on or following this Day
  -> Either Text [Day] -- | Either an infinite sequence of Days following the reference day according to the SeriesSelector (on the right side), otherwise an error message describing something like a parsing failure (on the left side).
repeatDaySeries selector referenceDay =
  let eitherSeries = parseSeriesSelector selector
  in case eitherSeries of
    (Left t) -> Left t
    (Right (SeriesSelector monthL dayOfMonthL dayOfWeekL)) ->
      Right $ repeatDaySeries' (sort monthL) dayOfMonthL dayOfWeekL referenceDay

getEventSeriesR :: EventSeriesId -> Handler Html
getEventSeriesR eventSeriesId = error "Not yet implemented: getEventSeriesR"

postEventSeriesR :: EventSeriesId -> Handler Html
postEventSeriesR eventSeriesId = error "Not yet implemented: postEventSeriesR"
