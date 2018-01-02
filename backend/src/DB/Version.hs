{-# LANGUAGE OverloadedStrings #-}
module DB.Version
  ( checkVersion
  , setVersion
  ) where

import           Control.Monad
import           Data.Text                              (Text)
import           Database.PostgreSQL.Simple

checkVersion :: Connection -> Text -> String -> IO Bool
checkVersion conn key value = do
  rows <- query conn "SELECT true FROM versions WHERE key = ? AND version = ?"
            (key, value)
  case rows of
    [Only val] -> pure val
    _          -> pure False

setVersion :: Connection -> Text -> String -> IO ()
setVersion conn key value = do
  void $ execute conn "DELETE FROM versions WHERE key = ?" (Only key)
  void $ execute conn "INSERT INTO versions VALUES (?,?)" (key,value)
