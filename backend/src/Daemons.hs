{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp  #-}
module Daemons where

import           Control.Monad
import           Data.Chinese.CCDict
import qualified Data.Map                               as Map
import qualified Data.Set                               as Set
import           Data.Time
import           Data.Version
import           Database.PostgreSQL.Simple             (withTransaction)
import           Database.PostgreSQL.Simple.Transaction

import           Broadcast
import           Buckets                                (readBucketByTag)
import qualified Buckets
import           DB
import           Helpers
import           Logic
import           Types
import qualified Tiling

-- High scores are incrementally updated for active users but needs to be
-- periodically updated for inactive users. Ie. when time passes, high scores
-- may need to be decreased.
updHighscores bc db = do
  daily <- readBucketByTag db Buckets.highscoreHourly
  weekly <- readBucketByTag db Buckets.highscoreDaily
  broadcast bc $ SetHighscore (Highscore daily) (Highscore weekly)

-- Sentence to Words daemon.
-- When cndict version changes, recalculate words for each stencil.
-- schedule will have to be recomputed as well
updSentenceWords conn = do
  goodVersion <- checkVersion conn "cndict" (showVersion version)
  unless goodVersion $ timeIt "Sentence Words" $ withTransaction conn $ do
    noticeLog "Recalculating sentence words..."
    forEachSentence conn $ \sid sentence ->
      setSentenceWords conn sid [ entryOriginal e | e <- Tiling.textEntries sentence ]
    setVersion conn "cndict" (showVersion version)
    noticeLog "Recalculating sentence words... DONE!"

updSchedule conn = do
  now <- getCurrentTime
  withTransactionSerializable conn $ do
    (ms, (new, del)) <- timed $ updScheduleAndClearDirty conn now
    when (new > 0 || del > 0) $
      infoLog $ "Schedule: Finished in " ++ show ms ++ " ms, " ++ show new ++ " new, " ++ show del ++ " deleted."

updDirtyUsers conn = do
  mbDirtyUser <- withTransaction conn $ fetchDirtyUser conn
  case mbDirtyUser of
    Nothing -> return ()
    Just dirtyUser -> withTransaction conn $
      timeIt "Recalculating models & schedule" $ do
        markUserClean conn dirtyUser
        mbLastModel <- foldResponses conn dirtyUser Nothing (worker dirtyUser)
        case mbLastModel of
          Nothing -> return ()
          Just lastModel ->
            createModel conn lastModel
        markUserSchedule conn dirtyUser
  where
    worker userId Nothing response = do
      let newStability | responseShownAnswer response = minStability
                       | otherwise                    = knownWordStability
          newReviewAt  = updateReviewAt response newStability
          newModel = Model
            { modelUserId    = userId
            , modelWord      = responseWord response
            , modelStability = round newStability
            , modelCreatedAt = responseCreatedAt response
            , modelReviewAt  = newReviewAt
            }
      return $ Just newModel
    worker userId (Just model) response
      | modelWord model /= responseWord response = do
        createModel conn model
        let newStability | responseShownAnswer response = minStability
                         | otherwise                    = knownWordStability
            newReviewAt  = updateReviewAt response newStability
            newModel = Model
              { modelUserId    = userId
              , modelWord      = responseWord response
              , modelStability = round newStability
              , modelCreatedAt = responseCreatedAt response
              , modelReviewAt  = newReviewAt
              }
        return $ Just newModel
      | otherwise =
        return $ Just $ updateModel response model


-- Deck sentences
-- When decks are dirty, get the chinese text, find stencils, set HSK level and difficulty level.
updDirtyDecks conn = do
  dirty <- fetchDirtyDecks conn
  unless (null dirty) $ do
    noticeLog "Recalculating deck sentences..."
    allSentences <- fetchSentences conn
    let db = Map.fromListWith (++)
              [ (entryOriginal e, [(sid, entries)])
              | (sid, sentence) <- allSentences
              , let entries = Tiling.textEntries sentence
              , e <- entries ]
    forM_ dirty $ \deck -> do
      setDeckFlags conn (deckId deck) False True
      timeIt "Dirty deck" $ withTransaction conn $ do
        cs <- fetchContent conn (deckContentId deck)
        let ws = contentEntries cs
            sentences = Tiling.tileSentences db ws
        setDeckSentences conn (deckId deck) sentences
        -- setDeckTags conn (deckId deck) newTagsHere
        setDeckFlags conn (deckId deck) False False
    noticeLog "Recalculating deck sentences... DONE!"

contentEntries :: ContentState -> [Entry]
contentEntries = setNub Set.empty . concatMap blockEntries . contentStateBlocks
  where
    setNub _seen [] = []
    setNub seen (x:xs)
      | x `Set.member` seen = setNub seen xs
      | otherwise           = x : setNub (Set.insert x seen) xs
    blockEntries = Tiling.textEntries . contentBlockText
