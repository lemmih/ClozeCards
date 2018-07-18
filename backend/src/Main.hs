{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Control.Concurrent
import           Control.Exception
import           Control.Lens               ((&), (.~))
import           Control.Monad
import           Control.Monad.Trans
import           Data.Aeson                 (Value (..), object, (.=))
import qualified Data.Aeson                 as Aeson
import qualified Data.ByteString.Char8      as B8
import qualified Data.HashMap.Strict        as HM
import           Data.Pool
import qualified Data.Text                  as T
import qualified Data.UUID                  as UUID
import qualified Data.UUID.V4               as UUID
import qualified Data.Vector                as V
import           System.Environment
import           System.IO
import           System.IO.Error

import           Happstack.Server
import qualified Network.AWS                as AWS
import qualified Network.AWS.S3             as AWS
import qualified Network.WebSockets         as WS
import           WebSockets

import qualified Database.PostgreSQL.Simple as PSQL

import           Data.Chinese.CCDict
import           Data.Chinese.Segmentation

import           Broadcast
import           CLI.Audio
import           CLI.Tatoeba
import           CLI.TenThousand
import           CLI.Tile
import           CLI.Users
import           Client
import qualified Daemons
import           DB
import           Helpers                    (logExceptions)
import qualified Worker

instance ToMessage Aeson.Value where
  toContentType _ = "text/json"
  toMessage       = Aeson.encode

oneSecond :: Int
oneSecond = 10^6

oneMinute :: Int
oneMinute = oneSecond * 60

oneHour :: Int
oneHour = oneMinute * 60

mkDatabasePool :: IO (Pool PSQL.Connection)
mkDatabasePool = do
  dbAddr <- getEnv "SQL_DB" `catchIOError` \_ -> return "dbname=ClozeCards"
  createPool (PSQL.connectPostgreSQL (B8.pack dbAddr)) PSQL.close
    1 -- One stripe.
    (60*60) -- Keep connections open for an hour.
    5 -- Max five connections per stripe.

main :: IO ()
main = do
    pool <- mkDatabasePool
    runDB pool $ \_ -> return ()
    args <- getArgs
    case args of
      [] -> do
        bc <- newBroadcastState
        group <- Worker.new
        Worker.forkIO group $ forever $ do
          logExceptions "updSentenceWords" $
            runDBUnsafe pool Daemons.updSentenceWords
          threadDelay oneSecond
        Worker.forkIO group $ forever $ do
          logExceptions "updDirtyDecks" $
            runDBUnsafe pool Daemons.updDirtyDecks
          threadDelay oneSecond
        Worker.forkIO group $ forever $ do
          logExceptions "updSchedule" $
            runDBUnsafe pool Daemons.updSchedule
          threadDelay oneSecond
        Worker.forkIO group $ forever $ do
          logExceptions "updDirtyUsers" $
            runDBUnsafe pool Daemons.updDirtyUsers
          threadDelay (oneSecond * 10)
        Worker.forkIO group $ forever $ do
          logExceptions "updHighscores" $
            runDBUnsafe pool $ Daemons.updHighscores bc
          threadDelay oneHour
        simpleHTTP nullConf (dir "api" $ msum
          [dir "status" $ do
            method GET
            ok $ toResponse ("OK"::String)
          ,dir "blob" $ msum
            [dir "upload" $ do
              let maxDiskSize = 1024*1024*50
                  maxRAMSize = 1024*4
              decodeBody $ defaultBodyPolicy "/tmp/" maxDiskSize maxRAMSize maxRAMSize
              (tmpPath, remoteName, contentType) <- lookFile "file"
              blobId <- liftIO UUID.nextRandom
              liftIO $ do
                lgr <- AWS.newLogger AWS.Error stdout
                env <- AWS.newEnv AWS.Discover
                body <- AWS.chunkedFile AWS.defaultChunkSize tmpPath
                let key = AWS.ObjectKey $ UUID.toText blobId
                AWS.runResourceT $ AWS.runAWS (env & AWS.envLogger .~ lgr) $
                  AWS.within AWS.Ohio $
                    AWS.send $ AWS.putObject "clozecards-blobs" key body
              let url = "https://clozecards-blobs.s3.amazonaws.com/" <> UUID.toText blobId
              ok $ toResponse url
            ]
          ,dir "segmentation" $ do
            method POST
            blocks <- jsonBody :: ServerPart Aeson.Object
            ok $ toResponse $ Aeson.Object (HM.map segmentate blocks)
          ,dir "ws" $ runWebSocketsHappstack $ \pc -> do
            conn <- WS.acceptRequest pc
            handleNewWS bc pool conn
          ]) `finally` Worker.killAll group
      ["tatoeba", sentences, links] -> runDB pool $ \conn -> tatoeba conn sentences links
      ["audio", dest] -> runDB pool $ \conn -> fetchAudio conn dest
      ["users", userFile] -> runDB pool $ \conn -> importUsers conn userFile
      ["responses", userFile, responsesFile] -> runDB pool $ \conn -> importResponses conn userFile responsesFile
      ["tenthousand"] -> runDB pool $ \conn -> tenThousand conn
      ["tile", wordFile] -> runDB pool $ \conn -> debugTiling conn wordFile
      _ -> putStrLn "Usag: prog tatoeba sentences links"
  where
    jsonBody :: Aeson.FromJSON a => ServerPart a
    jsonBody = do
      rq <- askRq
      mbBS <- fmap unBody <$> takeRequestBody rq
      case mbBS of
        Nothing -> mzero
        Just bs ->
          case Aeson.decode bs of
            Nothing    -> liftIO (putStrLn "failed to parse") >> mzero
            Just value -> return value

segmentate :: Value -> Value
segmentate (String str) =
    Aeson.toJSON (worker 0 (tokenizer str))
  where
    worker pos [] = []
    worker pos (x:xs) =
      case x of
        KnownWord e -> let end = pos + T.length (entryOriginal e) in
          object [ "start" .= pos
                 , "end"   .= end
                 ] : worker end xs
        UnknownWord txt ->
          worker (pos + T.length txt) xs

segmentate x = Array V.empty
