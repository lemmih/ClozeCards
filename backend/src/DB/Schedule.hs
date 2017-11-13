{-# LANGUAGE OverloadedStrings #-}
module DB.Schedule where

import           Control.Monad
import           Data.Text                              (Text)
import Data.Time
import           Database.PostgreSQL.Simple

import           Types


updScheduleAndClearDirty :: Connection -> UTCTime -> IO (Int, Int)
updScheduleAndClearDirty conn now = do
  new <- execute conn
    "INSERT INTO schedule (\
    \   SELECT user_id, sentence_id, review_at, seen_at\
    \     FROM dirty_schedule\
    \    WHERE created_at < ?)\
    \ ON CONFLICT(user_id, sentence_id) DO UPDATE\
    \ SET review_at = EXCLUDED.review_at"
    (Only now)
  del <- execute conn
    "DELETE FROM dirty_sentences WHERE created_at < ?" (Only now)
  return (fromIntegral new, fromIntegral del)


setSchedule :: Connection -> UserId -> SentenceId -> UTCTime -> UTCTime -> IO ()
setSchedule conn userId sentenceId seenAt reviewAt = void $ execute conn
  "INSERT INTO schedule (user_id, sentence_id, review_at, seen_at)\
  \              VALUES (?,       ?,           ?,         ?)\
  \ ON CONFLICT(user_id, sentence_id) DO UPDATE\
  \ SET seen_at = EXCLUDED.seen_at,\
  \     review_at = EXCLUDED.review_at"
  (userId, sentenceId, reviewAt, seenAt)

-- We might have, say, 10 sentences to review at time X.
-- Once we pick one of those words, we should not review any of the
-- other sentences until we can recomputed the scheduled review time.
-- This function pushes the scheduled review time into the future.
clearSchedule :: Connection -> UserId -> UTCTime -> IO ()
clearSchedule conn userId reviewAt = void $ execute conn
  "UPDATE schedule SET review_at = now() + interval '1 hour'\
  \ WHERE user_id = ? AND review_at = ?" (userId, reviewAt)

-- This can take a long time. Some words are used in 10,000+ sentences.
markAllSentencesDirty :: Connection -> UserId -> Text -> IO ()
markAllSentencesDirty conn userId word = void $ execute conn
  "INSERT INTO dirty_sentences (\
  \  SELECT sentence_id, ? FROM sentence_words WHERE word = ?)\
  \ ON CONFLICT (sentence_id, user_id) DO UPDATE\
  \    SET created_at = EXCLUDED.created_at"
  (userId, word)

markSeenSentencesDirty :: Connection -> UserId -> Text -> IO ()
markSeenSentencesDirty conn userId word = void $ execute conn
  "INSERT INTO dirty_sentences (\
  \  SELECT schedule.sentence_id, schedule.user_id\
  \    FROM sentence_words, schedule\
  \   WHERE schedule.user_id = ? AND word = ? AND sentence_words.sentence_id = schedule.sentence_id)\
  \ ON CONFLICT (sentence_id, user_id) DO UPDATE\
  \    SET created_at = EXCLUDED.created_at"
  (userId, word)
