{-# LANGUAGE OverloadedStrings #-}
module CLI.TenThousand (tenThousand) where

import           Control.Monad
import           Data.Chinese.CCDict
import           Data.Chinese.Segmentation
import           Data.List
import qualified Data.Map                   as Map
import           Data.Ord
import qualified Data.Text                  as T
import qualified Database.PostgreSQL.Simple as PSQL

import           DB.Sentences

import qualified Tiling

-- Find the cheapest sentence x 10,000.
--
tenThousand :: PSQL.Connection -> IO ()
tenThousand conn = do
  sentences <- fetchSentencePairs conn
  let db =  [ (simplified, english, entries)
            | (simplified, english) <- sentences
            , let entries = Tiling.textEntries simplified ]
      cheapest = findCheapest 1000 Map.empty db
  forM_ (zip [1..] cheapest) $ \(n,(simplified, english, newWords)) -> do
    let tokens = tokenizer simplified
        txt = [ case token of
                  UnknownWord txt -> txt
                  KnownWord e | entryOriginal e `elem` newWords -> "<b>" <> entryOriginal e <> "</b>"
                              | otherwise -> entryOriginal e
              | token <- tokens ]
    putStrLn $ show n ++ ". " ++ T.unpack (T.concat txt)
    putStrLn $ show n ++ ". " ++ T.unpack english

findCheapest 0 _seenWords _sentences = []
findCheapest count seenWords [] = []
findCheapest count seenWords sentences =
    (simplified, english, newWords) : findCheapest (count-1) seenWords' (delete sentence sentences)
  where
    newWords = filter (`Map.notMember` seenWords) $ map entryOriginal entries
    seenWords' = Map.union seenWords $ Map.fromList (zip newWords [0,0..])
    (_cost, sentence@(simplified, english, entries)) = minimumBy (comparing fst) graded
    graded = [ (cost, sentence)
             | sentence@(simplified, english, entries) <- sentences
             , let wordCost = Tiling.sentenceCost seenWords 0 0 entries
                   cost = sum [ a*b | (_, a, b) <- wordCost ]]
