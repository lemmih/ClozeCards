module Worker where

import           Control.Concurrent as C
import           Control.Exception
import           Control.Monad      (forever)
import           Data.Time          (UTCTime, getCurrentTime, addUTCTime)

data Worker = Worker (MVar [(ThreadId, MVar ())])

new :: IO Worker
new = fmap Worker (newMVar [])

forkIO :: Worker -> IO () -> IO ()
forkIO (Worker mvar) action = do
    flag <- newEmptyMVar
    tid <- C.forkIO (action `finally` putMVar flag ())
    modifyMVar_ mvar (\lst -> return $ (tid, flag) : lst)

killAll :: Worker -> IO ()
killAll (Worker mvar) = do
    lst <- takeMVar mvar
    mapM_ killThread (map fst lst)
    mapM_ takeMVar (map snd lst)

timeLoop :: (UTCTime -> UTCTime -> IO ()) -> IO ()
timeLoop fn = do
  let loop lastTime = do
        now <- getCurrentTime
        fn lastTime now
        loop now
  forever $ loop =<< addUTCTime (-60) <$> getCurrentTime
