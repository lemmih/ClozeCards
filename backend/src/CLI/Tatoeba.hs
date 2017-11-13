{-# LANGUAGE OverloadedStrings #-}
module CLI.Tatoeba (tatoeba) where

import           Control.Monad
import qualified Data.ByteString.Char8      as B
import           Data.Chinese.Segmentation
import qualified Data.IntMap                as M
import qualified Data.IntSet                as S
import qualified Data.Map                   as Map
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T
import qualified Data.Text.Read             as T
import qualified Data.Vector                as V
import qualified Database.PostgreSQL.Simple as PSQL

readInt :: T.Text -> Int
readInt txt =
  case T.decimal txt of
    Left err        -> error err
    Right (i, rest) | T.null rest -> i
    Right (i, rest) -> error $ "Junk at the end of number: " ++ show (i,rest)

chineseKey = T.filter (`notElem` punctuation)
  where
    punctuation = ".,'\"!。，‘’！、（）：" :: String

tatoeba :: PSQL.Connection -> FilePath -> FilePath -> IO ()
tatoeba conn sentenceFile linkFile = do
  ls <- T.lines <$> T.readFile sentenceFile
  let sentences =
        [ (readInt key, lang, T.intercalate "\t" txt)
        | (key:lang:txt) <- map (T.splitOn "\t") ls ]
  let chineseSentences = M.fromList
        [ (idx, sentence::T.Text)
        | (idx, "cmn", sentence) <- sentences ]
  let englishSentences = M.fromList
        [ (idx, sentence)
        | (idx, "eng", sentence) <- sentences ]

  -- putStrLn $ "Total Sentenes: " ++ show (length sentences)
  -- putStrLn $ "Chinese sentences: " ++ show (length chineseSentences)
  -- putStrLn $ "English sentences: " ++ show (length englishSentences)

  ls <- T.lines <$> T.readFile linkFile
  let linkMap = M.fromListWith S.union
        [ (keyA, S.singleton keyB)
        | [keyA, keyB] <- map (map readInt . T.splitOn "\t") ls
        , keyA `M.member` chineseSentences
        , keyB `M.member` englishSentences ]
      dupLinks = length
        [ ()
        | [keyA, keyB] <- map (map readInt . T.splitOn "\t") ls
        , keyA `M.member` chineseSentences
        , keyB `M.member` chineseSentences ]

  let lowestKey a@(keyA, _, _) b@(keyB, _, _)
        | keyA < keyB = a
        | otherwise   = b
  let stencils = Map.fromListWith lowestKey
        [ (chineseKey cmn, (keyA, cmn, eng))
        | (keyA, keysB) <- M.toList linkMap
        , let cmn = toSimplified (chineseSentences M.! keyA)
        , let eng = map (englishSentences M.!) (S.toList keysB) ]

  -- putStrLn $ "Dup links: " ++ show dupLinks
  -- putStrLn $ "Duplicate sentences: " ++ show (M.size linkMap - Map.size stencils)
  -- putStrLn $ "Translated sentences: " ++ show (Map.size stencils)
  forM_ (Map.toList stencils) $ \(key, (idx, cmn, eng)) ->
    B.putStrLn =<< PSQL.formatQuery conn "INSERT INTO sentences (id, simplified, english) VALUES (?, ?, ?) ON CONFLICT DO NOTHING;"
       (idx, cmn, V.fromList eng)
