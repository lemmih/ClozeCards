{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Buckets
  ( pushToBucket -- :: Connection -> Bucket a -> a -> Int -> IO ()
  , readBuckets -- :: Connection -> Bucket a -> a -> IO [(UTCTime, Int)]
  , readBucketSnapshot -- :: Connection -> Bucket a -> a -> IO Int
  , readBucketByTag -- :: Connection -> Bucket a -> IO [(a, Int)]
  , highscoreHourly -- :: Bucket UserId
  , highscoreDaily -- :: Bucket UserId
  , responsesHourly -- :: Bucket ()
  , responsesDaily -- :: Bucket ()
  , knownDaily -- :: Bucket (UserId, DeckId)
  ) where

import Data.Time
import qualified Data.UUID as UUID
import qualified Data.Text as T
import Data.Text (Text)

import DB
import Types
import           Database.PostgreSQL.Simple

{-
# Number of words
KnownWords
 - Indexed by day

# Similar to KnownWords but doesn't count skipped words.
LearnedWords
  - Index by day
-}

{-
Buckets

Key | Value | At

data Bucket = Bucket
  { bucketKey :: Text
  , bucket}



key | tag | user_id | deck_id | at | value


Use cases:
- High score for the last 24 hours.
  key = "highscore"
  tag = "hourly"
  user_id = ...
  deck_id = None
  at = hour
  value = int
- High score for the last 7 days.
  key = "highscore"
  tag = "daily"
  user_id = ...
  deck_id = None
  at = day
  value = int
- High score for the last 30 days.
- High score is points per user.
- Total responses for the last 24 hours.
- Total responses for the last 7 days.
- Total responses for the last 30 days.
- Total responses for all time.
  key = "responses"
  tag = "all"
  user_id = None
  deck_id = None
  at = day
  value = int
- Words known per user per course per day.
  key = "known"
  tag = None
  user_id = ...
  deck_id = ...
  at = day
  value = int


-}

data Bucket a = Bucket
  { bucketKey    :: Text
  , bucketTag    :: a -> [Text]
  , bucketUntag  :: [Text] -> a
  , bucketAt     :: UTCTime -> UTCTime
  , bucketAcc    :: Accumulate
  , bucketCutoff :: NominalDiffTime
  }

mkBucketTag :: [Text] -> Text
mkBucketTag = T.intercalate "_"
fromBucketTag :: Text -> [Text]
fromBucketTag = T.splitOn "_"

data Accumulate = Sum | Last

pushToBucket :: Connection -> Bucket a -> a -> Int -> IO ()
pushToBucket conn Bucket{..} tag val = do
    now <- getCurrentTime
    pushBucket conn bucketKey (mkBucketTag $ bucketTag tag) (bucketAt now) val
  where
    pushBucket =
      case bucketAcc of
        Sum -> pushBucketSum
        Last -> pushBucketLast

-- known words: get N readings
-- high score: get N readings, sum by user_id
-- responses: get N readings, sum all
-- []
-- readBucketOverTime knownDairly (lemmih, hsk1) = [(day 1, ...)]
-- readBucketOverTime responsesDaily () = [(day 1, ...), (day 2, ...)]
readBuckets :: Connection -> Bucket a -> a -> IO [(UTCTime, Int)]
readBuckets conn Bucket{..} tag = do
  now <- getCurrentTime
  let since = addUTCTime (negate bucketCutoff) now
  fetchBuckets conn bucketKey (mkBucketTag $ bucketTag tag) since

-- readBucketSnapshot knownDairly (lemmih, hsk1) = known words for lemmih in hsk1.
-- readBucketSnapshot highscoreHourly lemmih = high score for lemmih.
readBucketSnapshot :: Connection -> Bucket a -> a -> IO Int
readBucketSnapshot conn Bucket{..} tag = do
  now <- getCurrentTime
  let since = addUTCTime (negate bucketCutoff) now
  fetchBucketSumSnapshot conn bucketKey (mkBucketTag $ bucketTag tag) since

-- readBucketByTag highscoreHourly = [(UserId, Score)]
readBucketByTag :: Connection -> Bucket a -> IO [(a, Int)]
readBucketByTag conn Bucket{..} = do
  now <- getCurrentTime
  let since = addUTCTime (negate bucketCutoff) now
  rows <- fetchBucketSumByTag conn bucketKey since
  return [ (bucketUntag $ fromBucketTag tag, val) | (tag, val) <- rows ]

highscoreHourly :: Bucket UserId
highscoreHourly = Bucket "highscore-hourly" toTag fromTag crimpHour Sum (1*day)
  where
    toTag uid = [T.pack $ show uid]
    fromTag [txt] = read (T.unpack txt)
    fromTag _ = error "highscoreHourly: invalid tag"

highscoreDaily :: Bucket UserId
highscoreDaily = Bucket "highscore-daily" toTag fromTag crimpDay Sum (30*day)
  where
    toTag uid = [T.pack $ show uid]
    fromTag [txt] = read (T.unpack txt)
    fromTag _ = error "highscoreDaily: invalid tag"

responsesHourly :: Bucket ()
responsesHourly = Bucket "responses-hourly" toTag fromTag crimpHour Sum (1*day)
  where
    toTag () = []
    fromTag _ = ()

responsesDaily :: Bucket ()
responsesDaily = Bucket "responses-daily" toTag fromTag crimpDay Sum (30*day)
  where
    toTag () = []
    fromTag _ = ()

knownDaily :: Bucket (UserId, DeckId)
knownDaily = Bucket "known" toTag fromTag crimpDay Last (1*year)
  where
    toTag (uid, deckId) = [T.pack $ show uid, UUID.toText deckId]
    fromTag [uid, deckTxt]
      | Just deckId <- UUID.fromText deckTxt = (read $ T.unpack uid, deckId)
    fromTag _ = error "knownDaily: Invalid tag"

second :: NominalDiffTime
second = 1
minute :: NominalDiffTime
minute = 60 * second
hour :: NominalDiffTime
hour = 60 * minute
day :: NominalDiffTime
day = 24 * hour
year :: NominalDiffTime
year = 365 * day

crimpHour :: UTCTime -> UTCTime
crimpHour (UTCTime day diff) =
  let TimeOfDay{..} = timeToTimeOfDay diff
      diff' = timeOfDayToTime TimeOfDay{ todHour = todHour, todMin = 0, todSec = 0}
  in UTCTime day diff'

crimpDay :: UTCTime -> UTCTime
crimpDay (UTCTime day _diff) =
  let diff' = timeOfDayToTime TimeOfDay{ todHour = 0, todMin = 0, todSec = 0}
  in UTCTime day diff'
