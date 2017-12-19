{-# LANGUAGE RecordWildCards #-}
module Client (handleNewWS) where

import           Control.Monad
import qualified Data.Aeson                 as Aeson
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.Chinese.Segmentation
import           Data.List
import qualified Data.Text                  as T
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
        sendJSON conn $ ReceiveSearchResults offset (map deckId results)
    FetchNotes userId deckId ->
      runDB pool $ \db -> do
        mbContent <- fetchNote db userId deckId
        sendJSON conn $ ReceiveNotes userId deckId mbContent
    ReceiveNotes _userId _deckId Nothing -> return () -- Delete the entry?
    ReceiveNotes _userId deckId (Just contentId) ->
      runDB pool $ \db -> createNote db userId deckId contentId
    Login{} -> return ()
    Logout{} -> return ()
    Register email password -> do
      mbUser <- runDB pool $ \db -> registerUser db userId email password
      case mbUser of
        Nothing -> sendJSON conn LoginFailed
        Just user -> do
          token <- runDB pool $ \db -> createToken db (getUserId user)
          sendJSON conn $ SetActiveUser user token
    SetFavorite deckId ->
      runDB pool $ \db -> setFavorite db userId deckId
    UnsetFavorite deckId ->
      runDB pool $ \db -> unsetFavorite db userId deckId
    SetVisibility deckId hidden ->
      runDB pool $ \db -> setVisibility db userId deckId hidden
    MarkWords ws asKnown -> do
      known <- runDB pool $ \db -> fetchKnownWords db userId
      forM_ (nub [ entrySimplified e | KnownWord e <- tokenizer ws]) $ \word ->
        when (word `notElem` known || not asKnown) $ do
          let resp = Response
                { responseUserId      = userId
                , responseWord        = word
                , responseSentenceId  = Nothing
                , responseCreatedAt   = error "CreatedAt not set" -- will be set by 'addResponse'
                , responseCompleted   = True
                , responseValue       = word
                , responseShownAnswer = not asKnown
                , responseFactor      = 100000000
                }
          addResponse pool userId resp
    FetchKnownWords -> do
      ws <- runDB pool $ \db -> fetchKnownWords db userId
      sendJSON conn $ ReceiveKnownWords (T.unwords ws)
    FetchHighlight deckId -> do
      (recent, expired, known) <- runDB pool $ \db -> deckHighlights db userId deckId
      sendJSON conn $ ReceiveHighlight
        { highlightRecent  = recent
        , highlightExpired = expired
        , highlightKnown   = known }
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
  case Aeson.eitherDecode text of
    Left err -> do
      hPutStrLn stderr $ "Bad request: " ++ err
      BL8.hPutStrLn stderr text
      error "Bad request"
    Right msg -> do
      debugLog' Green "RECEIVED " msg
      return msg
