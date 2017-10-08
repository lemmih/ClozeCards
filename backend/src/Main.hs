{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Control.Monad.Trans
import qualified Data.Text as T
import qualified Data.Aeson                 as Aeson
import Data.Aeson (Value(..), (.=), object)
import qualified Data.ByteString.Char8      as B8
import           Data.Pool
import           System.Environment
import           System.IO.Error
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V

import           Happstack.Server
import qualified Happstack.Server           as Happstack

import qualified Database.PostgreSQL.Simple as PSQL

import           Data.Chinese.CCDict
import           Data.Chinese.Segmentation

instance ToMessage Aeson.Value where
  toContentType _ = "text/json"
  toMessage       = Aeson.encode


mkDatabasePool :: IO (Pool PSQL.Connection)
mkDatabasePool = do
  dbAddr <- getEnv "SQL_DB" `catchIOError` \_ -> return "host=localhost user=lemmih"
  createPool (PSQL.connectPostgreSQL (B8.pack dbAddr)) PSQL.close
    1 -- One stripe.
    (60*60) -- Keep connections open for an hour.
    5 -- Max five connections per stripe.


main :: IO ()
main = do
    pool <- mkDatabasePool
    simpleHTTP nullConf (msum
      [dir "status" $ do
        method GET
        ok $ toResponse ("OK"::String)
      ,dir "segmentation" $ do
        method OPTIONS
        ok $ setHeader "Access-Control-Allow-Origin" "*"
           $ setHeader "Access-Control-Allow-Headers" "Content-type"
              $ toResponse ()
      ,dir "segmentation" $ do
        method POST
        blocks <- jsonBody :: ServerPart Aeson.Object
        ok $ setHeader "Access-Control-Allow-Origin" "*"
           $ toResponse $ Aeson.Object (HM.map segmentate blocks)
      ])
  where
    jsonBody :: Aeson.FromJSON a => ServerPart a
    jsonBody = do
      -- liftIO $ putStrLn "jsonBody"
      rq <- askRq
      mbBS <- fmap unBody <$> takeRequestBody rq
      case mbBS of
        Nothing -> mzero
        Just bs -> do
          -- liftIO $ L.putStrLn bs
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
