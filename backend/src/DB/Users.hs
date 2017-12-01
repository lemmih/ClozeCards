{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module DB.Users where

import           Crypto.BCrypt
import qualified Data.ByteString            as B
import qualified Data.Set                   as S
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import           Data.UUID.V4               as UUID
import           Database.PostgreSQL.Simple

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
    , getUserHash  = B.empty
    , getUserFavorites = S.empty }

registerUser :: Connection -> UserId -> Email -> Password -> IO (Maybe User)
registerUser conn userId email password = do
  hash <- hashPasswordUsingPolicy fastBcryptHashingPolicy (T.encodeUtf8 password)
  changed <- execute conn "UPDATE users SET email = ?, hash = ? WHERE id = ? AND NOT EXISTS (SELECT id FROM users WHERE email=?)"
    (email, hash, userId, email)
  if changed == 0
    then return Nothing
    else fetchUser conn userId

fetchUser :: Connection -> UserId -> IO (Maybe User)
fetchUser conn userId = queryMaybe conn
  "SELECT id, email, name, hash, \
  \ ARRAY(SELECT deck_id FROM favorites WHERE user_id = id)\
  \ FROM users WHERE id=?" (Only userId)

fetchUserByEmail :: Connection -> Email -> IO (Maybe User)
fetchUserByEmail conn email = queryMaybe conn
  "SELECT id, email, name, hash, \
  \ ARRAY(SELECT deck_id FROM favorites WHERE user_id = id)\
  \ FROM users WHERE email=?" (Only email)

createToken :: Connection -> UserId -> IO Token
createToken conn userId = do
  token <- UUID.nextRandom
  execute conn "INSERT INTO tokens values (?, ?)" (userId, token)
  return token

tokenLogin :: Connection -> UserId -> Token -> IO (Maybe User)
tokenLogin conn userId token = do
  rows <- query conn "SELECT id, email, name, hash,\
                     \ ARRAY(SELECT deck_id FROM favorites WHERE user_id = id)\
                     \ FROM tokens, users\
                     \ WHERE id=user_id AND user_id=? AND token=?" (userId, token)
  case rows of
    [user] -> return $ Just user
    _      -> return Nothing

passwdLogin :: Connection -> Email -> Password -> IO (Maybe User)
passwdLogin conn email passwd = do
  mbUser <- fetchUserByEmail conn email
  case mbUser of
    Just user | validatePassword (getUserHash user) (T.encodeUtf8 passwd) ->
      pure (Just user)
    _ -> pure Nothing
-- tokens
