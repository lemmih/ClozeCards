{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module DB.Users where

import           Crypto.BCrypt
import           Crypto.Hash.SHA256
import qualified Data.ByteString            as B
import qualified Data.Text                  as T
import qualified Data.Text.Encoding                  as T
import           Data.UUID.V4               as UUID
import           Database.PostgreSQL.Simple
import Data.ByteString.Base16

import           DB.Instances               ()
import           DB.Misc
import           Types


createUser :: Connection -> IO User
createUser conn = do
  Only userId <- querySingle conn "INSERT INTO users (id) VALUES (default) RETURNING id" ()
  return User
    { getUserId = userId
    , getUserEmail = Nothing
    , getUserName  = T.empty
    , getUserHash  = B.empty }

fetchUser :: Connection -> UserId -> IO (Maybe User)
fetchUser conn userId =
  queryMaybe conn "SELECT id, email, name, hash FROM users WHERE id=?" (Only userId)

fetchUserByEmail :: Connection -> Email -> IO (Maybe User)
fetchUserByEmail conn email =
  queryMaybe conn "SELECT id, email, name, hash FROM users WHERE email=?" (Only email)

-- FIXME: Use 'ON CONFLICT'
createToken :: Connection -> UserId -> IO Token
createToken conn userId = do
  execute conn "DELETE FROM tokens WHERE user_id = ?" (Only userId)
  token <- UUID.nextRandom
  execute conn "INSERT INTO tokens values (?, ?)" (userId, token)
  return token

tokenLogin :: Connection -> UserId -> Token -> IO (Maybe User)
tokenLogin conn userId token = do
  rows <- query conn "SELECT id, email, name, hash FROM tokens, users WHERE id=user_id AND user_id=? AND token=?" (userId, token)
  case rows of
    [user] -> return $ Just user
    _      -> return Nothing

passwdLogin :: Connection -> Email -> Password -> IO (Maybe User)
passwdLogin conn email passwd = do
  mbUser <- fetchUserByEmail conn email
  case mbUser of
    Just user | validatePassword (getUserHash user) (encode $ hash $ T.encodeUtf8 passwd) ->
      pure (Just user)
    _ -> pure Nothing
-- tokens
