{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Event where

import Import
import qualified Data.Text as T
import Data.Time
import qualified Text.Read as R

roleDetailsForm :: LocalTime -> RoleDetails -> RoleId -> [Participant] -> Widget
roleDetailsForm eventStart roleDetails roleId participants = do
  let roleIdVal = either (flip const $ "") id $ maybe (Left "empty list") fromPersistValueText $ headMay $ keyToValues roleId
      ident = "id" <> roleIdVal
  [whamlet|
    <div style="width: 20%; float: right;">
      <input type=checkbox id=#{ident} name=signup value=#{roleIdVal}>
      <label for=#{ident}>Signup for this role
    <p>#{roleDetailsDescription roleDetails}
    <p>Looking for #{show $ roleDetailsDesiredNrParticipants roleDetails} people.
    $if null participants
      <p>No one currently signed up.
    $else
      <p>Currently signed up:
        $forall participant <- participants
          <span title=#{maybe "" id $ participantFeelings participant}
            .participant>#{participantName participant}
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
            $forall ((Entity roleId _), (Entity _ roleDetails), participants) <- eventRoleAndParticipantsL
              <div>^{roleDetailsForm primeStartTime roleDetails roleId participants}
            <p>
              <label for=name>Your name
              <input id=name name=name>
            <p>
              <label for=feelings>
                How are you feeling about signing up for these roles?
              <input id=feelings name=feelings>
            <p>
              <label for=email>Your email address
              <input id=email name=email>
            <p>
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

data SignupPerson = SignupPerson { name :: Text
                                 , feelings :: Maybe Text
                                 , email :: Maybe Text
                                 , phone :: Maybe Text
                                 }

addParticipant :: MonadIO m => SignupPerson -> RoleId
                            -> ReaderT SqlBackend m (Key Participant)
addParticipant person roleId = do
  let participant = Participant roleId (name person) (email person)
                                       (phone person) Nothing (feelings person)
  insert participant

postEventR :: EventId -> Handler Html
postEventR eventId = do
  signupPerson <- runInputPost $ SignupPerson
    <$> ireq textField "name"
    <*> iopt textField "feelings"
    <*> iopt textField "email"
    <*> iopt textField "phone"
  signupParams <- (flip keyvals "signup") <$> getPostParams
  let signupRoleKeys = sqlKeysFromEncodings signupParams :: [Key Role]
  _ <- runDB . sequence $ addParticipant signupPerson <$> signupRoleKeys
  redirect $ EventR eventId
