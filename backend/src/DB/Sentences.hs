{-# LANGUAGE OverloadedStrings #-}
module DB.Sentences where

import           Control.Monad
import           Data.Text                  (Text)
import           Database.PostgreSQL.Simple

import           Types


forEachSentence :: Connection -> (SentenceId -> Text -> IO ()) -> IO ()
forEachSentence conn action = do
  forEach_ conn "SELECT id, simplified FROM sentences" $ \(sId, text) ->
    action sId text

setSentenceWords :: Connection -> SentenceId -> [Text] -> IO ()
setSentenceWords conn sid ws = do
  void $ execute conn "DELETE FROM sentence_words WHERE sentence_id = ?"
    (Only sid)
  void $ executeMany conn "INSERT INTO sentence_words VALUES (?,?) ON CONFLICT DO NOTHING"
    [ (sid, w) | w <- ws ]

fetchSentences :: Connection -> IO [(SentenceId, Text)]
fetchSentences conn =
  query conn "SELECT id, simplified FROM sentences" ()
