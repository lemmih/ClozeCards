{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module CLI.Users (importUsers, importResponses) where

import           Control.Monad
import           Data.Aeson                 as Aeson
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy       as LB
import qualified Data.Csv as Csv
import           Data.Maybe
import           Data.Text                  (Text)
import           Data.Time (UTCTime)
import qualified Data.Vector                as V
import qualified Database.PostgreSQL.Simple as PSQL

data User = User
  { userId            :: Text
  , userEmail         :: Maybe Email
  , userWeeklyReports :: Bool
  , userNewsletter    :: Bool
  , userName          :: Text
  , userPassword      :: Maybe Text
  , userCreatedAt     :: UTCTime
  } deriving (Show)

data Email = Email
  { emailAddress  :: Text
  , emailVerified :: Bool
  } deriving (Show)

instance FromJSON User where
  parseJSON = withObject "User" $ \o -> do
    profile <- o.:"profile"
    Object services <- o.:"services"
    Object password <- services.:?"password" .!= object []
    Object createdAt <- o.:"createdAt"
    emails <- o.:?"emails" .!= []
    User
      <$> o.:"_id"
      <*> pure (listToMaybe emails)
      <*> profile.:?"weeklyReports" .!= True
      <*> profile.:?"newsletter" .!= True
      <*> profile.:?"name" .!= ""
      <*> password.:?"bcrypt"
      <*> createdAt.:"$date"

instance FromJSON Email where
  parseJSON = withObject "Email" $ \o -> do
    Email
      <$> o.:"address"
      <*> o.:"verified"

data LegacyResponse = LegacyResponse
  { legacyKey         :: Text
  , legacyValue       :: Text
  , legacyShownAnswer :: Bool
  } deriving (Show)

instance FromJSON LegacyResponse where
  parseJSON = withObject "Response" $ \o ->
    LegacyResponse
      <$> o.:"key"
      <*> o.:"value"
      <*> o.:"shownAnswer"

loadUsers :: FilePath -> IO [User]
loadUsers path = do
  inp <- LB.readFile path
  case Aeson.eitherDecode inp of
    Left msg -> do
      error $ "Failed to parse: " ++ msg
    Right users ->
      pure users

importUsers :: PSQL.Connection -> FilePath -> IO ()
importUsers conn path = do
  users <- loadUsers path
  forM_ (users::[User]) $ \User{..} ->
    case (userEmail, userPassword) of
      (Just (Email address verified), Just password) ->
        B.putStrLn =<< PSQL.formatQuery conn
          "INSERT INTO users(email, name, hash, weeklyReports, newsletter, verified, created_at)\
          \           VALUES(?,     ?,    ?,    ?,            ?,          ?,         ?)\
          \ ON CONFLICT DO NOTHING;"
          (address, userName, password, userWeeklyReports, userNewsletter, verified, userCreatedAt)
      _ -> return ()

-- Aeson understand the time format we get from postgres.
parseTime :: Text -> UTCTime
parseTime txt =
  case fromJSON (String txt) of
    Error msg    -> error msg
    Success time -> time

instance Csv.FromField Bool where
  parseField "t" = pure True
  parseField "f" = pure False
  parseField _   = fail "Invalid bool"

-- {"key": "敢", "type: ""MandarinTextAnswer"", "value": "gǎn, "shownAnswer: false}
importResponses :: PSQL.Connection -> FilePath -> FilePath -> IO ()
importResponses conn usersFile responsesFile = do
  users <- loadUsers usersFile
  inp <- LB.readFile responsesFile
  case Csv.decode Csv.NoHeader inp of
    Left msg -> error msg
    Right rows ->
      V.forM_ rows $ \(_rid::Text, _sid::Text, uid::Text, content, at::Text, completed::Bool) ->
        case join $ listToMaybe [ userEmail u | u <- users, userId u == uid ] of
          Nothing -> return ()
          Just email ->
            case Aeson.eitherDecode content of
              Left msg       -> return ()
              Right LegacyResponse{..} ->
                B.putStrLn =<< PSQL.formatQuery conn
                  "INSERT INTO responses(user_id, word, created_at, completed, value, shown_answer, factor)\
                  \ SELECT id, ?, ?, ?, ?, ?, 3\
                  \  FROM users WHERE email = ?;"
                  (legacyKey, parseTime at, completed, legacyValue, legacyShownAnswer, emailAddress email)
