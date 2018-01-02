module CLI.Tile (debugTiling) where

import           Control.Monad
import           Data.Chinese.CCDict
import           Data.List
import qualified Data.Map                   as Map
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T
import qualified Database.PostgreSQL.Simple as PSQL
import           Text.Printf

import           DB
import           Tiling

debugTiling :: PSQL.Connection -> FilePath -> IO ()
debugTiling conn wordFile = do
  allSentences <- fetchSentences conn
  let db = Map.fromListWith (++)
            [ (entryOriginal e, [(sid, entries)])
            | (sid, sentence) <- allSentences
            , let entries = textEntries sentence
            , e <- entries ]
  inp <- T.readFile wordFile
  let ws = nub $ textEntries inp
      sentences = tileSentences db ws
      context = mkContext ws
  forM_ (zip [0..] sentences) $ \(n, (word, tiles)) ->
    case tiles of
      [] -> do
        T.putStr word
        putStrLn "\tBlank"
      tiles@(tile1: _) -> do
        let Just sentence = lookup (tileSentenceId tile1) allSentences
        T.putStrLn sentence
        T.putStr word
        forM_ (take 3 tiles) $ \tile ->
          printf "\t%.1f%%" (tileCost tile * 100)
        putStrLn ""
        forM_ (tileWords tile1) $ \(word, a, b) ->
          when (a*b > 0.01) $
            printf "  %s: %.0f * %.0f = %.0f\n" (T.unpack word) (a*100) (b*100) (a*b*100)
