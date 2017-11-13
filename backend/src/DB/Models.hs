{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module DB.Models where

import           Control.Monad
import           Data.Text                              (Text)
import           Database.PostgreSQL.Simple

import           Types
import           DB.Instances ()
import           DB.Misc

fetchModel :: Connection -> UserId -> Text -> IO (Maybe Model)
fetchModel conn userId word =
  queryMaybe conn "SELECT user_id, word, stability, review_at, created_at\
                  \  FROM models WHERE user_id = ? AND word = ?"
    (userId, word)

fetchSentenceModels :: Connection -> UserId -> SentenceId -> IO [(Text, Maybe Model)]
fetchSentenceModels conn userId sentenceId = do
  rows <- query conn "SELECT sentence_words.word, models.*\
                     \  FROM sentence_words\
                     \ LEFT JOIN models\
                     \    ON sentence_words.word = models.word AND\
                     \       models.user_id = ?\
                     \ WHERE sentence_id = ?"
                     (userId, sentenceId)
  return [ (key, mbModel) | (Only key :. mbModel) <- rows ]

createModel :: Connection -> Model -> IO ()
createModel conn Model{..} = void $ execute conn
  "INSERT INTO models VALUES (?, ?, ?, ?,?)\
  \ ON CONFLICT(user_id, word) DO UPDATE\
  \ SET stability = EXCLUDED.stability,\
  \     review_at = EXCLUDED.review_at,\
  \     created_at = EXCLUDED.created_at"
  (modelUserId, modelWord, modelStability, modelReviewAt, modelCreatedAt)
