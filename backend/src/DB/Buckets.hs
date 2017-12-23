{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module DB.Buckets where

import Data.Time
import Data.Text (Text)
import           Control.Monad
import           Database.PostgreSQL.Simple

import           DB.Instances               ()
import           DB.Misc

pushBucketSum :: Connection -> Text -> Text -> UTCTime -> Int -> IO ()
pushBucketSum conn key tag at value = void $ execute conn
  "INSERT INTO buckets (key, tag, at, value) VALUES (?, ?, ?, ?)\
  \ ON CONFLICT (key, tag, at) DO UPDATE\
  \ SET value = buckets.value + EXCLUDED.value"
  (key, tag, at, value)

pushBucketLast :: Connection -> Text -> Text -> UTCTime -> Int -> IO ()
pushBucketLast conn key tag at value = void $ execute conn
  "INSERT INTO buckets (key, tag, at, value) VALUES (?, ?, ?, ?)\
  \ ON CONFLICT (key, tag, at) DO UPDATE\
  \ SET value = EXCLUDED.value"
  (key, tag, at, value)

fetchBucketSumSnapshot :: Connection -> Text -> Text -> UTCTime -> IO Int
fetchBucketSumSnapshot conn key tag since = fromOnly <$> querySingle conn
  "SELECT SUM(value) FROM buckets WHERE key = ? AND tag = ? AND at > ?"
  (key, tag, since)

fetchBucketLastSnapshot :: Connection -> Text -> Text -> UTCTime -> IO Int
fetchBucketLastSnapshot conn key tag since = do
  mbRow <- queryMaybe conn
            "SELECT value FROM buckets WHERE key = ? AND tag = ? AND at > ?\
            \ ORDER BY at desc LIMIT 1"
            (key, tag, since)
  case mbRow of
    Nothing       -> return 0
    Just (Only n) -> return n

fetchBuckets :: Connection -> Text -> Text -> UTCTime -> IO [(UTCTime, Int)]
fetchBuckets conn key tag since = query conn
  "SELECT at, value FROM buckets WHERE key = ? AND tag = ? AND at > ?"
  (key, tag, since)

fetchBucketSumByTag :: Connection -> Text -> UTCTime -> IO [(Text, Int)]
fetchBucketSumByTag conn key since = query conn
  "SELECT tag, SUM(value) FROM buckets WHERE key = ? AND at > ?\
  \ GROUP BY tag"
  (key, since)
