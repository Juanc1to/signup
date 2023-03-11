{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Event where

import Import
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time
import qualified Text.Read as R

roleDetailsForm :: LocalTime -> RoleDetails -> RoleId -> [Participant] -> Widget
roleDetailsForm eventStart roleDetails roleId participants = do
  let roleIdVal = either (flip const $ T.empty) id
                  $ maybe (Left "empty list") fromPersistValueText
                  $ headMay $ keyToValues roleId
      signupId = "signup" <> roleIdVal
      bottomLineId = "bottomline" <> roleIdVal
      formatTime' = formatTime defaultTimeLocale
      fullTimeFormatString = "on %A, %B %-e at %-l:%M%P (%H%M)" -- or just `rfc822DateFormat`?
      roleStartMinutes = roleDetailsOffsetTime roleDetails
      roleStart = addLocalTime
                  (60 * (fromIntegral roleStartMinutes) :: NominalDiffTime)
                  eventStart
      roleStartS = formatTime' fullTimeFormatString roleStart
      roleDurationMinutes = roleDetailsDuration roleDetails
      roleEnd = addLocalTime
                (60 * (fromIntegral $ roleStartMinutes + roleDurationMinutes)
                      :: NominalDiffTime)
                eventStart
      timeOfDayFormatString
        | localDay roleStart == localDay roleEnd = "at %-l:%M%P (%H%M)"
        | otherwise = fullTimeFormatString
      roleEndS = formatTime' timeOfDayFormatString roleEnd
      participantSummary p9t = participantFirstName p9t
        <> maybe "" ((maybe "" (\ (c, _) -> " " <> singleton c <> "."))
                    . T.uncons) (participantLastName p9t)
        <> maybe "" (":" <>) (participantFeelings p9t)
      participantInfoL = (\ p9t ->
          (p9t
          , participantSummary p9t
          , maybe "" (singleton . fst) (T.uncons $ participantFirstName p9t)
            -- <> maybe "" ((fst <$>) . (>>= T.uncons)) (participantLastName p9t)
            <> maybe T.empty (singleton . fst) (participantLastName p9t >>= T.uncons)
          )) <$> participants
  [whamlet|
    <div .details>
      <h3>#{roleDetailsDescription roleDetails}
      <p>Starting #{roleStartS}, ending #{roleEndS}
      <p>Looking for #{show $ roleDetailsDesiredNrParticipants roleDetails} people.
      $if null participants
        <p>No one currently signed up.
      $else
        <p .participants>Currently signed up:
          $forall (participant, summary, initials) <- participantInfoL
            <span
              :maybe False id $ participantWillingToBottomline participant:.bottomline
              title=#{summary} .participant>#{initials}
    <div .signup>
      <p>
        <input type=checkbox id=#{signupId} name=signup value=#{roleIdVal}>
        <label for=#{signupId}>Signup for this role
      <p>
        <input type=checkbox id=#{bottomLineId}
               name=bottomline value=#{roleIdVal}>
        <label for=#{bottomLineId}>Are you willing to bottomline this role?
  |]

eventRoleAndParticipants :: MonadIO m => EventId -> Entity RoleDetails
  -> ReaderT SqlBackend m (Entity Role, Entity RoleDetails, [Participant])
                          -- -> ReaderT SqlBackend m (Maybe (Entity Role))
eventRoleAndParticipants eventId roleDetails = do
  maybeRole <- selectFirst [RoleRoleDetailsId ==. entityKey roleDetails
                           ,RoleEventId ==. eventId] []
  case maybeRole of
    (Just r@(Entity roleId _)) -> do
      participantL <- selectList [ParticipantRoleId ==. roleId] []
      return (r, roleDetails, entityVal <$> participantL)
    Nothing -> do
      let role = Role (entityKey roleDetails) eventId
      roleId <- insert role
      return (Entity roleId role, roleDetails, [])

getEventR :: EventId -> Handler Html
getEventR eventId = do
  event <- runDB $ get404 eventId
  let eventSeriesId = eventEventSeriesId event
  eventSeries <- runDB $ get404 eventSeriesId
  roleDetailsL <- runDB $ selectList
    [RoleDetailsEventSeriesId ==. eventSeriesId] [Asc RoleDetailsOffsetTime]
  eventRoleAndParticipantsL <- runDB . sequence
    $ eventRoleAndParticipants eventId <$> roleDetailsL
  let primeStartTime = LocalTime (eventEventDay event)
                                 $ eventSeriesPrimeStartTime eventSeries
      events = []
      formSection =
        [whamlet|
          <form method=post action=@{EventR eventId}>
            <div .roles>
              $forall ((Entity roleId _), (Entity _ roleDetails), participants) <- eventRoleAndParticipantsL
                ^{roleDetailsForm primeStartTime roleDetails roleId participants}
            <div #participant>
              <label for=firstName>Your first name, handle, or nickname
              <input id=firstName name=firstName>
              <label for=lastName>Your last name
              <input id=lastName name=lastName>
              <label for=feelings>
                How are you feeling about signing up for these roles?
              <input id=feelings name=feelings>
              <label for=email>Your email address
              <input id=email name=email>
              <label for=phone>Your phone number
              <input id=phone name=phone>
              <button>Sign up
        |]
  defaultLayout $ do
    setTitle . toHtml $ T.pack "Signup for this event"
    $(widgetFile "series")

keyvals :: (Eq a) => [(a, b)] -> a -> [b]
keyvals pairs key = snd <$> filter ((== key) . fst) pairs

onlyrights :: [Either a b] -> [b]
onlyrights [] = []
onlyrights ((Left _) : es) = onlyrights es
onlyrights ((Right r) : es) = r : onlyrights es

sqlKeysFromEncodings :: (PersistEntity record) => [Text] -> [Key record]
sqlKeysFromEncodings postVals =
  let int64s = onlyrights $ ((singleton . PersistInt64) <$>)
                          . R.readEither . T.unpack <$> postVals
  in onlyrights $ keyFromValues <$> int64s

data SignupPerson = SignupPerson { firstName :: Text
                                 , lastName :: Maybe Text
                                 , feelings :: Maybe Text
                                 , email :: Maybe Text
                                 , phone :: Maybe Text
                                 }

addParticipant :: MonadIO m => SignupPerson -> S.Set RoleId -> RoleId
                            -> ReaderT SqlBackend m (Key Participant)
addParticipant person bottomlineRoleIds roleId = do
  let participant = Participant roleId (firstName person) (lastName person)
                                (email person) (phone person)
                                (Just $ S.member roleId bottomlineRoleIds)
                                (feelings person)
  insert participant

postEventR :: EventId -> Handler Html
postEventR eventId = do
  signupPerson <- runInputPost $ SignupPerson
    <$> ireq textField "firstName"
    <*> iopt textField "lastName"
    <*> iopt textField "feelings"
    <*> iopt textField "email"
    <*> iopt textField "phone"
  signupParams <- (flip keyvals "signup") <$> getPostParams
  bottomlineParams <- (flip keyvals "bottomline") <$> getPostParams
  let signupRoleKeys = sqlKeysFromEncodings signupParams :: [Key Role]
  let bottomlineRoleKeys = sqlKeysFromEncodings bottomlineParams :: [Key Role]
  _ <- runDB . sequence $ addParticipant signupPerson
                                         (S.fromList bottomlineRoleKeys)
                        <$> signupRoleKeys
  redirect $ EventR eventId
