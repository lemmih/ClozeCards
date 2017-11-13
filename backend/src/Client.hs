{-# LANGUAGE RecordWildCards #-}
module Client (handleNewWS) where

import           Control.Monad
import qualified Data.Aeson                 as Aeson
import qualified Data.ByteString.Lazy.Char8 as BL8
import           System.Console.ANSI
import           System.IO

import qualified Network.WebSockets         as WS

import           Cards
import           DB
import           Debug
import           Logic
import           Types

handleNewWS pool conn = do
  handshake <- receiveJSON conn
  case handshake of
    WithoutToken -> do
      userId <- runDB pool $ \db -> do
        user <- createUser db
        token <- createToken db (getUserId user)
        sendJSON conn $ SetActiveUser user token
        return (getUserId user)
      handleClient pool conn userId
    WithToken userId token -> do
      userId' <- runDB pool $ \db -> do
        mbUser <- tokenLogin db userId token
        case mbUser of
          Just user -> do
            sendJSON conn $ SetActiveUser user token
            return userId
          Nothing -> do
            user <- createUser db
            token <- createToken db (getUserId user)
            sendJSON conn $ SetActiveUser user token
            return (getUserId user)
      handleClient pool conn userId'

handleClient pool conn userId = do
  msg <- receiveJSON conn
  case msg of
    ReceiveContent contentId content ->
      runDB pool $ \db ->
        createContent db userId contentId content
    ReceiveDeck deck@Deck{..} ->
      runDB pool $ \db -> do
        slugs <- createSlugs db deckId deckSlugs
        let deck' = deck{ deckSlugs = slugs, deckOwner = userId
                        , deckDirty = True, deckProcessing = True}
        createDeck db deck'
        sendJSON conn $ ReceiveDeck deck'
    FetchDeck slug ->
      runDB pool $ \db -> do
        mbDeck <- deckBySlug db slug
        case mbDeck of
          Nothing   -> sendJSON conn $ UnusedSlug slug
          Just deck -> sendJSON conn $ ReceiveDeck deck
    FetchContent contentId ->
      runDB pool $ \db -> do
        content <- fetchContent db contentId
        sendJSON conn $ ReceiveContent contentId content
    FetchCards deckId style ->
      runDB pool $ \db -> do
        cards <- fetchCards db userId deckId style
        sendJSON conn $ ReceiveCards deckId cards
    ReceiveResponse response -> addResponse pool userId response
    FetchSearchResults _query ordering offset ->
      runDB pool $ \db -> do
        results <- searchDecks db [] [] ordering offset
        forM_ results $ \result -> sendJSON conn $ ReceiveDeck result
        sendJSON conn $ ReceiveSearchResults (map deckId results)
    Login{} -> return ()
    Logout{} -> return ()
  case msg of
    Login email password -> do
      mbUser <- runDB pool $ \db -> passwdLogin db email password
      case mbUser of
        Nothing -> do
          sendJSON conn LoginFailed
          loop
        Just user -> do
          token <- runDB pool $ \db -> createToken db (getUserId user)
          sendJSON conn $ SetActiveUser user token
          handleClient pool conn (getUserId user)
    Logout -> do
      userId <- runDB pool $ \db -> do
        user <- createUser db
        token <- createToken db (getUserId user)
        sendJSON conn $ SetActiveUser user token
        return (getUserId user)
      handleClient pool conn userId
    _ -> loop
  where
    loop = handleClient pool conn userId

sendJSON conn msg = do
  debugLog' Blue "SENDING " msg
  WS.sendTextData conn $ Aeson.encode msg

receiveJSON conn = do
  text <- WS.receiveData conn
  case Aeson.decode text of
    Nothing  -> do
      hPutStrLn stderr "Bad request:"
      BL8.hPutStrLn stderr text
      error "Bad request"
    Just msg -> do
      debugLog' Green "RECEIVED " msg
      return msg
