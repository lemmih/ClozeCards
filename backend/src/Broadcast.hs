module Broadcast
  ( Broadcast
  , newBroadcastState
  , insertConnection
  , deleteConnection
  , broadcast
  ) where

import           Control.Exception
import           Control.Concurrent
import           Control.Monad
import qualified Data.Aeson         as Aeson
import           Data.IORef
import           Data.Map           (Map)
import qualified Data.Map           as Map
import qualified Network.WebSockets as WS

import           Types
import           Helpers

type Env = Map UserId WS.Connection
newtype Broadcast = State (IORef Env)

newBroadcastState :: IO Broadcast
newBroadcastState = State <$> newIORef Map.empty

insertConnection :: Broadcast -> UserId -> WS.Connection -> IO ()
insertConnection (State ref) uid conn = do
  modifyIORef ref $ Map.insert uid conn
  void $ forkIO $ broadcast (State ref) $ SetOnline [uid] []

deleteConnection :: Broadcast -> UserId -> IO ()
deleteConnection (State ref) uid = do
  modifyIORef ref $ Map.delete uid
  void $ forkIO $ broadcast (State ref) $ SetOnline [] [uid]

broadcast :: Aeson.ToJSON a => Broadcast -> a -> IO ()
broadcast (State ref) msg = do
  conns <- Map.toList <$> readIORef ref
  forM_ conns $ \(uid, conn) ->
    logExceptions "broadcast" $
      WS.sendTextData conn (Aeson.encode msg)
      `onException` modifyIORef ref (Map.delete uid)
