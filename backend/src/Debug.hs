module Debug where

import           System.Console.ANSI

import Helpers
import Types

class Show a => Interesting a where
  isInteresting :: a -> Bool

debugLog :: Interesting a => String -> a -> IO ()
debugLog = debugLog' Yellow

debugLog' :: Interesting a => Color -> String -> a -> IO ()
debugLog' color tag a
  | isInteresting a = do
    setSGR [SetColor Foreground Vivid color]
    safePutStrLn (tag ++ pp a)
    setSGR [Reset]
  | otherwise       = do
    setSGR [SetColor Foreground Vivid color]
    safePutStrLn $ head $ lines (tag ++ pp a)
    setSGR [Reset]














instance Interesting ClientMessage where
  isInteresting _ = False

instance Interesting ServerMessage where
  -- isInteresting SetActiveUser{} = True
  isInteresting _ = False

instance Interesting Handshake where
  isInteresting _ = True
