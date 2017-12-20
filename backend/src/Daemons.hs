{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp  #-}
module Daemons where

import           Control.Monad
import           Data.Chinese.CCDict
import           Data.Chinese.Segmentation
import           Data.List
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Maybe
import           Data.Ord
import qualified Data.Set                   as Set
import           Data.Text                  (Text)
import           Data.Time
import           Data.Version
import           Database.PostgreSQL.Simple (withTransaction)
import           Database.PostgreSQL.Simple.Transaction

import           DB
import           Logic
import           Helpers
import           Types


-- Sentence to Words daemon.
-- When cndict version changes, recalculate words for each stencil.
-- schedule will have to be recomputed as well
updSentenceWords conn = do
  goodVersion <- checkVersion conn "cndict" (showVersion version)
  unless goodVersion $ timeIt "Sentence Words" $ withTransaction conn $ do
    noticeLog "Recalculating sentence words..."
    forEachSentence conn $ \sid sentence ->
      setSentenceWords conn sid [ entryOriginal e | e <- textEntries sentence ]
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
              , let entries = textEntries sentence
              , e <- entries ]
    forM_ dirty $ \deck -> do
      setDeckFlags conn (deckId deck) False True
      timeIt "Dirty deck" $ withTransaction conn $ do
        cs <- fetchContent conn (deckContentId deck)
        let ws = contentEntries cs
            sentences = tileSentences db ws
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
    blockEntries = textEntries . contentBlockText

tileSentences :: Map Text [(SentenceId, [Entry])] -> [Entry] -> [(Text, Maybe (SentenceId, Int))]
tileSentences db ws =
    [ (entryOriginal w, findSentence db context n w)
    | w <- ws
    | n <- [0..] ]
  where
    context = Map.fromList
      [ (entryOriginal w, n)
      | w <- ws
      | n <- [0..] ]

--
-- Find the sentence with the lowest cost according to these rules:
--   1. Words that have already been introduced are free.
--   2. Words that will be introduced later have a cost multiplier that
--      grows exponentially with distance until it reaches a max of 90%
--   3. Words have a fixed cost (1e-5).
--   4. Sentence cost is the sum of word costs and the max of recip word freq.
--
findSentence :: Map Text [(SentenceId, [Entry])]
             -> Map Text Int
             -> Int
             -> Entry
             -> Maybe (SentenceId, Int)
findSentence db context nth entry = do
  sentences <- Map.lookup (entryOriginal entry) db
  -- Cost is the primary sorting criteria.
  -- If two sentence have the same cost, take the longest one.
  let withCost =
        [ (sid, (cost, negate(length ws)))
        | (sid, ws) <- sentences
        , let cost = sentenceCost context nth ws ]
  (sid, (cost, _len)) <- listToMaybe $ sortBy (comparing snd) withCost
  guard (cost < costLimit)
  return (sid, round (cost / costLimit * 100))

sentenceCost :: Map Text Int -> Int -> [Entry] -> Double
sentenceCost context nth ws =
    sum wordCosts + maximum freqCosts
  where
    wordCosts = [ wordCost * multiplier w | w <- ws ]
    freqCosts = [ recip (max 1 (fromIntegral (entryWordFrequency w))) * multiplier w
                | w <- ws ]
    multiplier w =
      case Map.lookup (entryOriginal w) context of
        Nothing  -> 1
        Just idx -> costMultiplier nth idx

-- Parameters:
lookAhead :: Int
lookAhead = 10

cutoff :: Int
cutoff = 90

maxCost :: Double
maxCost = 0.9

wordCost :: Double
wordCost = 1e-5

costLimit :: Double
costLimit = 0.05

-- Distance           Cost-multiplier
-- 0-lookAhead        0%
-- lookAhead-cutoff   0% growing to maxCost
-- cutoff-infinity    maxCost
--
-- With default parameters:
-- Distance           Cost-multiplier
-- 0                  0%
-- 10                 0%
-- 30                 7%
-- 50                 18%
-- 70                 36%
-- 90                 67%
-- 95                 78%
-- 99                 87%
-- 100                90%
costMultiplier :: Int -> Int -> Double
costMultiplier self remote =
    min maxCost $ max 0 $
    (exp (diff/factor)-1)/(steepness-1)*maxCost
  where
    diff = fromIntegral (remote - self - lookAhead)
    steepness = 10
    factor = fromIntegral cutoff/(log steepness)

textEntries :: Text -> [Entry]
textEntries txt = [ e | KnownWord e <- tokenizer txt ]
