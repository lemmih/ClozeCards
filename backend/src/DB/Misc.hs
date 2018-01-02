{-# LANGUAGE OverloadedStrings #-}
module DB.Misc where

import           Data.Pool
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.Transaction

querySingle :: (Show a, ToRow a, FromRow r) => Connection -> Query -> a -> IO r
querySingle conn q a = do
  rows <- query conn q a
  case rows of
    [row] -> pure row
    []    -> error $ "Bad single query, no results: " ++ show q ++ " " ++ show a
    _     -> error $ "Bad single query, many results: " ++ show q

queryMaybe :: (ToRow a, FromRow r) => Connection -> Query -> a -> IO (Maybe r)
queryMaybe conn q a = do
  rows <- query conn q a
  case rows of
    [row] -> pure (Just row)
    []    -> pure Nothing
    _     -> error $ "Bad maybe query, many results: " ++ show q


runDB :: Pool Connection -> (Connection -> IO a) -> IO a
runDB pool action =
  withResource pool $ \conn ->
    withTransactionSerializable conn (action conn)

runDBUnsafe :: Pool Connection -> (Connection -> IO a) -> IO a
runDBUnsafe pool action =
  withResource pool $ \conn ->
    action conn
