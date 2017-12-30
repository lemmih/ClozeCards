module Main (main) where

import           Control.Monad
import           Data.Maybe
import           System.Directory
import           System.FilePath

srcDirectory :: FilePath
srcDirectory = "sentences-unlinked"

dstDirectory :: FilePath
dstDirectory = "sentences"

main :: IO ()
main = do
  files <- getDirectoryContents srcDirectory
  let recordings = catMaybes
        [ do tag <- listToMaybe (words file)
             (ident, "") <- listToMaybe (reads tag)
             return (ident::Int, file)
        | file <- files]
  forM_ recordings $ \(tag, file) -> do
    print tag
    copyFile (srcDirectory </> file) (dstDirectory </> show tag <.> "mp3")
