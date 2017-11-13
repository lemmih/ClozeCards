{-# LANGUAGE OverloadedStrings #-}
module CLI.Audio (fetchAudio) where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import qualified Database.PostgreSQL.Simple as PSQL
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.IO
import           System.Process

requireTool :: String -> IO FilePath
requireTool name = do
  mbTool <- findExecutable name
  case mbTool of
    Nothing -> do
      hPutStrLn stderr $ name ++ " required and not found."
      exitWith (ExitFailure 1)
    Just tool -> pure tool

fetchAudio :: PSQL.Connection -> FilePath -> IO ()
fetchAudio conn dest = do
  gtts <- requireTool "gtts-cli"
  id3  <- requireTool "id3tool"

  files <- getDirectoryContents dest
  let fileIds = [ i | file <- files, (i,"") <- reads (takeBaseName file) ] :: [Int]
  rows <- PSQL.query conn "SELECT id, simplified FROM sentences where coalesce(id not in ?, TRUE)" (PSQL.Only $ PSQL.In fileIds)
  let nMax = length rows
  forM_ (zip [1..] rows) $ \(n, (sId, sentence)) -> do
    quit <- hReady stdin
    when quit $ exitWith ExitSuccess
    putStrLn $ "Need voice for: " ++ show n ++ "/" ++ show nMax ++ " " ++ sentence
    let output = dest </> show (sId::Int) <.> "mp3"
    uninterruptibleMask_ $ do
      callProcess gtts ["-l", "zh-cn", "-o", output, sentence]
      callProcess id3 ["-n", sentence, output]
    threadDelay (10^5*5)
