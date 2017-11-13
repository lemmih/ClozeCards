module Helpers where

import           Control.Concurrent
import qualified Data.Text.Lazy      as TL
import           Data.Time
import           System.Console.ANSI
import           System.IO
import           System.IO.Unsafe
import           Text.Pretty.Simple

{-# NOINLINE lock #-}
lock :: MVar ()
lock = unsafePerformIO $ newMVar ()

safePutStrLn :: String -> IO ()
safePutStrLn msg = withMVar lock $ \_ -> putStrLn msg

timeIt :: String -> IO a -> IO a
timeIt tag action = do
  (ms, ret) <- timed action
  noticeLog $ tag ++ ": Finished in " ++ show ms ++ " ms"
  return ret

-- Timing in ms
timed :: IO a -> IO (Int, a)
timed action = do
  start <- getCurrentTime
  ret <- action
  end <- getCurrentTime
  return (round (diffUTCTime end start * 100), ret)

infoLog msg = do
  setSGR [SetColor Foreground Vivid Blue]
  safePutStrLn msg
  setSGR [Reset]

noticeLog msg = do
  setSGR [SetColor Foreground Vivid Yellow]
  safePutStrLn msg
  setSGR [Reset]

errorLog msg = do
  setSGR [SetColor Foreground Vivid Red]
  withMVar lock $ \_ -> hPutStrLn stderr msg
  setSGR [Reset]

pp a = init $ unlines $ take limit $ lines $ TL.unpack (pShowOpt opt a)
  where
    limit = 15
    opt = defaultOutputOptionsDarkBg{outputOptionsIndentAmount=1}
