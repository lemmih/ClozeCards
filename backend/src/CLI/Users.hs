{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module CLI.Users (importUsers) where

import qualified Database.PostgreSQL.Simple as PSQL
import Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Char8 as B
import Data.Text (Text)
import Data.Time
import Control.Monad
import Data.Maybe
import Helpers

data User = User
  { userEmail :: Maybe Email
  , userWeeklyReports :: Bool
  , userNewsletter :: Bool
  , userName :: Text
  , userPassword :: Maybe Text
  , userCreatedAt :: UTCTime
  } deriving (Show)

data Email = Email
  { emailAddress :: Text
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
      <$> pure (listToMaybe emails)
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

importUsers :: PSQL.Connection -> FilePath -> IO ()
importUsers conn path = do
  inp <- LB.readFile path
  case Aeson.eitherDecode inp of
    Left msg -> do
      errorLog $ "Failed to parse: " ++ msg
    Right users ->
      forM_ (users::[User]) $ \User{..} ->
        case (userEmail, userPassword) of
          (Just (Email address verified), Just password) ->
            B.putStrLn =<< PSQL.formatQuery conn
              "INSERT INTO users(email, name, hash, weeklyReports, newsletter, verified, created_at)\
              \           VALUES(?,     ?,    ?,    ?,            ?,          ?,         ?)\
              \ ON CONFLICT DO NOTHING;"
              (address, userName, password, userWeeklyReports, userNewsletter, verified, userCreatedAt)
          _ -> return ()
