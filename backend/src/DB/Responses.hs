{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module DB.Responses where

import           Control.Monad
import           Database.PostgreSQL.Simple

import           DB.Instances               ()
import           Types


createResponse :: Connection -> Response -> IO ()
createResponse conn Response{..} = void $ execute conn
  "INSERT INTO responses VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
    ( responseUserId, responseWord, responseSentenceId, responseCreatedAt
    , responseCompleted, responseValue, responseShownAnswer, responseFactor )

foldResponses :: Connection -> UserId -> a -> (a -> Response -> IO a) -> IO a
foldResponses conn userId = fold conn
  "SELECT user_id, word, sentence_id, created_at, completed, value, shown_answer, factor\
  \  FROM responses\
  \ WHERE user_id = ?\
  \ ORDER BY word, created_at asc" (Only userId)
