{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE RecordWildCards  #-}
module Tiling where

import           Data.Chinese.CCDict
import           Data.Chinese.Segmentation
import           Data.List
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Maybe
import           Data.Text                 (Text)
import qualified Data.Text                 as T

import           Types

{- Goal: Find sentences that cover words in word list.
Exclude sentences that are too expensive.

Parameters:
  word cost: 0.1
  look ahead: 10
  cutoff: 90
  max cost: 0.9
  freqLimitFactor: 2.5
-}

type SentenceDB = Map Text [(SentenceId, [Entry])]
type Context = Map Text Int

data Tile = Tile
  { tileSentenceId :: SentenceId
  , tileWords      :: [(Text, Double, Double)]
  } deriving (Eq)

instance Ord Tile where
  a `compare` b = key a `compare` key b
    where
      key tile = (tileCost tile, length (tileWords tile))

tileCost :: Tile -> Double
tileCost Tile{..} = sum [ cost*multiplier | (_, cost, multiplier) <- tileWords ]

tileSentences :: SentenceDB -> [Entry] -> [(Text, [Tile])]
tileSentences db ws =
    [ (entryOriginal w, findSentence db (mkContext ws) minFreq n w)
    | w <- ws
    | n <- [0..] ]
  where
    medianFreq = median (map entryWordFrequency ws)
    minFreq = medianFreq `div` medianFreqFactor

mkContext :: [Entry] -> Context
mkContext ws = Map.fromList
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
findSentence :: SentenceDB -> Context -> Int -> Int -> Entry -> [Tile]
findSentence db context minFreq nth entry = fromMaybe [] $ do
  sentences <- Map.lookup (entryOriginal entry) db
  -- Cost is the primary sorting criteria.
  -- If two sentence have the same cost, take the longest one.
  let withCost =
        [ Tile sid cost
        | (sid, ws) <- sentences
        , let cost = sentenceCost context minFreq nth ws ]
  return $ sort withCost

sentenceCost :: Map Text Int -> Int -> Int -> [Entry] -> [(Text, Double, Double)]
sentenceCost context minFreq nth ws =
    freqCosts
    -- maximumBy cmp freqCosts : wordCosts
  where
    cmp (_,c1,m1) (_,c2,m2) = compare (c1*m1) (c2*m2)
    wordCosts = [ (entryOriginal w, wordCost, multiplier w) | w <- ws ]
    freqCosts = [ (entryOriginal w, fromIntegral minFreq / fromIntegral freq, multiplier w)
                | w <- ws
                , let freq = enhancedWordFrequency w
                , freq> 0 ]
    multiplier w =
      case Map.lookup (entryOriginal w) context of
        Nothing  -> 1
        Just idx -> costMultiplier nth idx

enhancedWordFrequency :: Entry -> Int
enhancedWordFrequency e
  | entryWordFrequency e > 0 = entryWordFrequency e
  | otherwise =
    case freqByWord of
      [] -> 0
      x:xs -> foldl' min x xs
    where
      freqByWord =
        [ entryWordFrequency entry
        | entry <- mapMaybe lookupMatch (T.chunksOf 1 (entryOriginal e))
        , entryWordFrequency entry > 0 ]


-- Parameters:
medianFreqFactor :: Int
medianFreqFactor = 10

lookAhead :: Int
lookAhead = 10

cutoff :: Int
cutoff = 90

maxCost :: Double
maxCost = 0.9

wordCost :: Double
wordCost = 0.1

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
    factor = fromIntegral cutoff / log steepness

median :: Ord a => [a] -> a
median lst = head (drop (length lst `div` 2) (sort lst))

textEntries :: Text -> [Entry]
textEntries txt = [ e | KnownWord e <- tokenizer txt ]
