{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Types where

import           Data.Aeson
import           Data.ByteString (ByteString)
import           Data.Map        (Map)
import           Data.Set        (Set)
import           Data.Text       (Text, pack)
import           Data.Time
import           Data.UUID

data User = User
  { getUserId        :: UserId
  , getUserEmail     :: Maybe Email
  , getUserName      :: Name
  , getUserHash      :: ByteString
  , getUserFavorites :: Set DeckId
  } deriving (Show)

type UserId = Int
type Email = Text
type Name = Text
type Slug = Text
type Tag = Text
type Password = Text
type Token = UUID
type DeckId = UUID
type ContentId = UUID
type Content = ContentState
type SentenceId = Int

data Handshake =
  WithToken UserId Token |
  WithoutToken
  deriving ( Show )


data Deck = Deck
  { deckId         :: DeckId
  , deckOwner      :: UserId
  , deckType       :: Text
  , deckTitle      :: Text
  , deckTags       :: [Tag]
  , deckAudioUrl   :: Maybe Text -- Maybe URL
  , deckSlugs      :: [Slug]
  , deckNLikes     :: Int
  , deckNComments  :: Int
  , deckContentId  :: ContentId
  , deckCreatedAt  :: UTCTime
  , deckDirty      :: Bool
  , deckProcessing :: Bool
  , deckHidden     :: Bool
  } deriving (Show)


-- ContentState and ContentBlock should match draft-js.
data ContentState = ContentState
  { contentStateBlocks    :: [ContentBlock]
  , contentStateEntityMap :: Map Int DraftEntity
  } deriving (Show)
data ContentBlock = ContentBlock
  { contentBlockKey          :: Text
  , contentBlockData         :: Value
  , contentBlockText         :: Text
  , contentBlockType         :: Text
  , contentBlockDepth        :: Int
  , contentBlockEntities     :: [EntityRange]
  , contentBlockInlineStyles :: [InlineStyle]
  } deriving (Show)
data InlineStyle = InlineStyle
  { styleName   :: Text
  , styleLength :: Int
  , styleOffset :: Int
  } deriving (Show)
data EntityRange = EntityRange
  { entityRangeKey    :: Int
  , entityRangeLength :: Int
  , entityRangeOffset :: Int
  } deriving (Show)
data DraftEntity = DraftEntity
  { entityData       :: Value
  , entityType       :: Text
  , entityMutability :: Mutability
  } deriving (Show)
data Mutability = Mutable | Immutable | Segmented deriving (Show)

data Card = Card
  { cardWord       :: Text
  , cardSentenceId :: SentenceId
  , cardChinese    :: [CardBlock]
  , cardEnglish    :: Text
  , cardNow        :: UTCTime
  } deriving (Show)

data CardBlock
  = ChineseBlock
    { blockSimplified  :: Text
    , blockPinyin      :: Text
    , blockDefinitions :: [Definition]
    , blockAnswers     :: [Text]
    , blockIsGap       :: Bool
    , blockIsNew       :: Bool
    , blockOffset      :: Int
    , blockEnglish     :: Maybe Text
    }
  | EscapedBlock Text
    deriving (Show)

data Definition = Definition
  { definitionPinyin  :: Text
  , definitionEnglish :: [Text]
  } deriving (Show)

data CardTemplate = CardTemplate
  { cardTemplateIndex      :: Int
  , cardTemplateWord       :: Text
  , cardTemplateSentenceId :: SentenceId
  , cardTemplateSimplified :: Text
  , cardTemplateEnglish    :: Text
  , cardTemplateModels     :: [(Text, Maybe UTCTime)]
  } deriving (Show)

data Response = Response
  { responseUserId      :: UserId
  , responseWord        :: Text
  , responseSentenceId  :: Maybe SentenceId
  , responseCreatedAt   :: UTCTime
  , responseCompleted   :: Bool
  , responseValue       :: Text
  , responseShownAnswer :: Bool
  , responseFactor      :: Double
  } deriving (Show)

data Model = Model
  { modelUserId    :: UserId
  , modelWord      :: Text
  , modelStability :: Int
  , modelReviewAt  :: UTCTime
  , modelCreatedAt :: UTCTime
  } deriving (Show)

data Style = Review | Study deriving (Show)

type Offset = Int
type SearchQuery = Text

data DeckOrdering
  = ByLikes
  | ByDate
  | ByTrending
    deriving (Show)

data ClientMessage
  = ReceiveContent ContentId Content
  | ReceiveDeck Deck
  | FetchDeck Slug
  | FetchContent ContentId
  | FetchCards DeckId Style
  | ReceiveResponse Response
  | FetchSearchResults SearchQuery DeckOrdering Offset
  | FetchNotes UserId DeckId
  | ReceiveNotes UserId DeckId (Maybe ContentId)
  | Login Email Password
  | Logout
  | Register Email Password
  | SetFavorite DeckId
  | UnsetFavorite DeckId
  | SetVisibility DeckId Bool
  | MarkWords Text Bool -- True => Mark as known, False => Mark as unknown
  | FetchKnownWords
  | FetchHighlight DeckId
  | DictionaryLookup [Text]
    deriving (Show)

newtype Highscore = Highscore [(UserId, Int)]
  deriving (Show)

data ServerMessage
  = SetActiveUser User Token
  | UnusedSlug Slug
  | ReceiveSearchResults Int [DeckId]
  | ReceiveCards DeckId [Card]
  | ReceiveKnownWords Text
  | ReceiveHighlight
    { highlightRecent :: [Text]
    , highlightExpired :: [Text]
    , highlightKnown   :: [Text] }
  | LoginFailed
  | ReceiveDictionaryResults [(Text, [Definition])]
  | SetHighscore
    { highscoreDaily  :: Highscore
    , highscoreWeekly :: Highscore }
  | UpdateHighscore
    { highscoreDailyDelta  :: Highscore
    , highscoreWeeklyDelta :: Highscore }
    deriving (Show)



----------------------------------------------------------------------------
-- Instances

instance ToJSON User where
  toJSON (User userId email name _hash favorites) = object
    [ "id" .= userId
    , "email" .= email
    , "name" .= name
    , "favorites" .= favorites ]

instance FromJSON Handshake where
  parseJSON = withObject "Handshake" $ \o -> do
    tag <- o .: "tag"
    case tag::String of
      "with-token"    -> WithToken <$> o.:"userId" <*> o.:"token"
      "without-token" -> pure WithoutToken
      _               -> fail "invalid handshake"

instance FromJSON Deck where
  parseJSON = withObject "Deck" $ \o -> Deck
    <$> o.:"id"
    <*> o.:"owner"
    <*> o.:"type"
    <*> o.:"title"
    <*> o.:"tags"
    <*> o.:"audioUrl"
    <*> o.:"slugs"
    <*> o.:"nLikes"
    <*> o.:"nComments"
    <*> o.:"contentId"
    <*> o.:"createdAt"
    <*> o.:"dirty"
    <*> o.:"processing"
    <*> o.:"hidden"

instance ToJSON Deck where
  toJSON Deck{..} = object
      [ "id"        .= deckId
      , "owner"     .= deckOwner
      , "type"      .= deckType
      , "title"     .= deckTitle
      , "tags"      .= deckTags
      , "audioUrl"  .= deckAudioUrl
      , "slugs"     .= deckSlugs
      , "nLikes"    .= deckNLikes
      , "nComments" .= deckNComments
      , "contentId" .= deckContentId
      , "createdAt" .= deckCreatedAt
      , "dirty"     .= deckDirty
      , "processing" .= deckProcessing
      , "hidden"    .= deckHidden ]

instance FromJSON ContentState where
  parseJSON = withObject "ContentState" $ \o ->
    ContentState
      <$> o.:"blocks"
      <*> o.:"entityMap"

instance ToJSON ContentState where
  toJSON ContentState{..} = object
    [ "blocks" .= contentStateBlocks
    , "entityMap" .= contentStateEntityMap ]

instance FromJSON ContentBlock where
  parseJSON = withObject "ContentBlock" $ \o ->
    ContentBlock
      <$> o.:"key"
      <*> o.:"data"
      <*> o.:"text"
      <*> o.:"type"
      <*> o.:"depth"
      <*> o.:"entityRanges"
      <*> o.:"inlineStyleRanges"

instance ToJSON ContentBlock where
  toJSON ContentBlock{..} = object
    [ "key"               .= contentBlockKey
    , "data"              .= contentBlockData
    , "text"              .= contentBlockText
    , "type"              .= contentBlockType
    , "depth"             .= contentBlockDepth
    , "entityRanges"      .= contentBlockEntities
    , "inlineStyleRanges" .= contentBlockInlineStyles
    ]

instance FromJSON InlineStyle where
  parseJSON = withObject "InlineStyle" $ \o ->
    InlineStyle
      <$> o.:"style"
      <*> o.:"length"
      <*> o.:"offset"

instance ToJSON InlineStyle where
  toJSON InlineStyle{..} = object
    [ "style"  .= styleName
    , "length" .= styleLength
    , "offset" .= styleOffset ]

instance FromJSON EntityRange where
  parseJSON = withObject "EntityRange" $ \o ->
    EntityRange
      <$> o.:"key"
      <*> o.:"length"
      <*> o.:"offset"

instance ToJSON EntityRange where
  toJSON EntityRange{..} = object
    [ "key"    .= entityRangeKey
    , "length" .= entityRangeLength
    , "offset" .= entityRangeOffset ]

instance FromJSON DraftEntity where
  parseJSON = withObject "DraftEntity" $ \o ->
    DraftEntity
      <$> o.:"data"
      <*> o.:"type"
      <*> o.:"mutability"

instance ToJSON DraftEntity where
  toJSON DraftEntity{..} = object
    [ "data"       .= entityData
    , "type"       .= entityType
    , "mutability" .= entityMutability ]

instance FromJSON Mutability where
  parseJSON = withText "Mutability" $ \txt ->
    case txt of
      "MUTABLE"   -> pure Mutable
      "IMMUTABLE" -> pure Immutable
      "SEGMENTED" -> pure Segmented
      _           -> fail "Invalid mutability"

instance ToJSON Mutability where
  toJSON Mutable   = String "MUTABLE"
  toJSON Immutable = String "IMMUTABLE"
  toJSON Segmented = String "Segmented"

instance FromJSON Card where
  parseJSON = withObject "Card" $ \o ->
    Card
      <$> o.:"word"
      <*> o.:"sentenceId"
      <*> o.:"chinese"
      <*> o.:"english"
      <*> o.:"now"

instance ToJSON Card where
  toJSON Card{..} = object
    [ "word"       .= cardWord
    , "sentenceId" .= cardSentenceId
    , "chinese"    .= cardChinese
    , "english"    .= cardEnglish
    , "now"        .= cardNow
    ]

instance FromJSON CardBlock where
  parseJSON (String txt) = pure $ EscapedBlock txt
  parseJSON (Object o)   =
    ChineseBlock
      <$> o.:"simplified"
      <*> o.:"pinyin"
      <*> o.:"definitions"
      <*> o.:"answers"
      <*> o.:"isGap"
      <*> o.:"isNew"
      <*> o.:"offset"
      <*> o.:"english"
  parseJSON _ = fail "invalid CardBlock"

instance ToJSON CardBlock where
  toJSON (EscapedBlock txt) = String txt
  toJSON ChineseBlock{..} = object
    [ "simplified"  .= blockSimplified
    , "pinyin"      .= blockPinyin
    , "definitions" .= blockDefinitions
    , "answers"     .= blockAnswers
    , "isGap"       .= blockIsGap
    , "isNew"       .= blockIsNew
    , "offset"      .= blockOffset
    , "english"     .= blockEnglish ]

instance FromJSON Definition where
  parseJSON = withObject "Definition" $ \o ->
    Definition
      <$> o.:"pinyin"
      <*> o.:"english"

instance ToJSON Definition where
  toJSON Definition{..} = object
    [ "pinyin" .= definitionPinyin
    , "english" .= definitionEnglish ]

instance FromJSON Response where
  parseJSON = withObject "Response" $ \o ->
    Response
      <$> o.:"userId"
      <*> o.:"word"
      <*> o.:"sentenceId"
      <*> o.:"createdAt"
      <*> o.:"completed"
      <*> o.:"value"
      <*> o.:"shownAnswer"
      <*> o.:"factor"

instance ToJSON Response where
  toJSON Response{..} = object
    [ "userId" .= responseUserId
    , "word" .= responseWord
    , "sentenceId" .= responseSentenceId
    , "createdAt" .= responseCreatedAt
    , "completed" .= responseCompleted
    , "value" .= responseValue
    , "shownAnswer" .= responseShownAnswer
    , "factor" .= responseFactor ]

instance FromJSON Style where
  parseJSON = withText "Style" $ \txt ->
    case txt of
      "study"  -> pure Study
      "review" -> pure Review
      _        -> fail "invalid study style"

instance ToJSON Style where
  toJSON Review = String "review"
  toJSON Study  = String "study"

instance FromJSON DeckOrdering where
  parseJSON (String "ByLikes")    = pure ByLikes
  parseJSON (String "ByDate")     = pure ByDate
  parseJSON (String "ByTrending") = pure ByTrending
  parseJSON _                     = fail "Invalid DeckOrdering"

instance ToJSON DeckOrdering where
  toJSON ByLikes    = String "ByLikes"
  toJSON ByDate     = String "ByDate"
  toJSON ByTrending = String "ByTrending"

instance FromJSON ClientMessage where
  parseJSON = withObject "ClientMessage" $ \o -> do
    tag <- o .: "type"
    case tag::String of
      "RECEIVE_CONTENT" -> do
        payload <- o .: "payload"
        ReceiveContent
          <$> payload.:"id"
          <*> payload.:"content"
      "RECEIVE_DECK" ->
        ReceiveDeck <$> (parseJSON =<< o.:"payload")
      "FETCH_DECK" ->
        FetchDeck <$> o.:"payload"
      "FETCH_CONTENT" ->
        FetchContent <$> o.:"payload"
      "FETCH_CARDS" -> do
        payload <- o .: "payload"
        FetchCards
          <$> payload.:"deckId"
          <*> payload.:"style"
      "RECEIVE_RESPONSE" ->
        ReceiveResponse <$> o.:"payload"
      "FETCH_SEARCH_RESULTS" -> do
        payload <- o .: "payload"
        FetchSearchResults
          <$> payload.:"query"
          <*> payload.:"order"
          <*> payload.:"offset"
      "FETCH_NOTES" -> do
        payload <- o .: "payload"
        FetchNotes
          <$> payload.:"userId"
          <*> payload.:"deckId"
      "RECEIVE_NOTES" -> do
        payload <- o .: "payload"
        ReceiveNotes
          <$> payload.:"userId"
          <*> payload.:"deckId"
          <*> payload.:"contentId"
      "LOGIN" -> do
        payload <- o .: "payload"
        Login
          <$> payload.:"email"
          <*> payload.:"password"
      "REGISTER" -> do
        payload <- o .: "payload"
        Register
          <$> payload.:"email"
          <*> payload.:"password"
      "LOGOUT" -> pure Logout
      "SET_FAVORITE" -> SetFavorite <$> o.:"payload"
      "UNSET_FAVORITE" -> UnsetFavorite <$> o.:"payload"
      "SET_VISIBILITY" -> do
        payload <- o .: "payload"
        SetVisibility
          <$> payload.:"deckId"
          <*> payload.:"hidden"
      "MARK_WORDS" -> do
        payload <- o .: "payload"
        MarkWords
          <$> payload.:"words"
          <*> payload.:"known"
      "FETCH_KNOWN_WORDS" -> pure FetchKnownWords
      "FETCH_HIGHLIGHT" ->
        FetchHighlight
          <$> o.:"payload"
      "DICTIONARY_LOOKUP" ->
        DictionaryLookup
          <$> o.:"payload"
      _ -> fail "invalid tag"

toAction :: String -> Value -> Value
toAction typeString payload = object
  [ "type"    .= typeString
  , "payload" .= payload ]

instance ToJSON ClientMessage where
  toJSON (ReceiveContent contentId content) =
    toAction "RECEIVE_CONTENT" $ object
      [ "id" .= contentId
      , "content" .= content ]
  toJSON (ReceiveDeck deck) =
    toAction "RECEIVE_DECK" $ toJSON deck
  toJSON (FetchDeck slug) =
    toAction "FETCH_DECK" $ toJSON slug
  toJSON (FetchContent contentId) =
    toAction "FETCH_CONTENT" $ toJSON contentId
  toJSON (FetchCards deckId style) =
    toAction "FETCH_CARDS" $ object
      [ "deckId" .= deckId
      , "style" .= style ]
  toJSON (ReceiveResponse response) =
    toAction "RECEIVE_RESPONSE" $ toJSON response
  toJSON (FetchSearchResults query order offset) =
    toAction "FETCH_SEARCH_RESULTS" $ object
      [ "query"  .= query
      , "order"  .= order
      , "offset" .= offset ]
  toJSON (FetchNotes userId deckId) =
    toAction "FETCH_NOTES" $ object
      [ "userId"  .= userId
      , "deckId"  .= deckId ]
  toJSON (ReceiveNotes userId deckId contentId) =
    toAction "RECEIVE_NOTES" $ object
      [ "userId"    .= userId
      , "deckId"    .= deckId
      , "contentId" .= contentId ]
  toJSON (Login email password) =
    toAction "LOGIN" $ object
      [ "email"    .= email
      , "password" .= password ]
  toJSON (Register email password) =
    toAction "REGISTER" $ object
      [ "email"    .= email
      , "password" .= password ]
  toJSON Logout =
    toAction "LOGOUT" Null
  toJSON (SetFavorite deckId) =
    toAction "SET_FAVORITE" $ toJSON deckId
  toJSON (UnsetFavorite deckId) =
    toAction "UNSET_FAVORITE" $ toJSON deckId
  toJSON (SetVisibility deckId hidden) =
    toAction "SET_VISIBILITY" $ object
      [ "deckId" .= deckId
      , "hidden" .= hidden ]
  toJSON (MarkWords words known) =
    toAction "MARK_WORDS" $ object
      [ "words"  .= words
      , "known" .= known ]
  toJSON FetchKnownWords =
    toAction "FETCH_KNOWN_WORDS" Null
  toJSON (FetchHighlight deckId) =
    toAction "FETCH_HIGHLIGHT" $ toJSON deckId
  toJSON (DictionaryLookup ws) =
    toAction "DICTIONARY_LOOKUP" $ toJSON ws

instance ToJSON Highscore where
  toJSON (Highscore lst) = object
    [ (pack $ show uid) .= score | (uid, score) <- lst ]

instance ToJSON ServerMessage where
  toJSON (SetActiveUser user token) =
    toAction "SET_ACTIVE_USER" $ object
      [ "user"  .= user
      , "token" .= token ]
  toJSON (ReceiveCards deckId cards) =
    toAction "RECEIVE_CARDS" $ object
      [ "deckId" .= deckId
      , "cards" .= cards ]
  toJSON (UnusedSlug slug) =
    toAction "UNUSED_SLUG" $ toJSON slug
  toJSON (ReceiveSearchResults offset results) =
    toAction "RECEIVE_SEARCH_RESULTS" $ object
      [ "offset" .= offset
      , "results" .= results ]
  toJSON (ReceiveKnownWords words) =
    toAction "RECEIVE_KNOWN_WORDS" (toJSON words)
  toJSON ReceiveHighlight{..} =
    toAction "RECEIVE_HIGHLIGHT" $ object
      [ "recent"  .= highlightRecent
      , "expired" .= highlightExpired
      , "known"   .= highlightKnown ]
  toJSON LoginFailed =
    toAction "LOGIN_FAILED" Null
  toJSON (ReceiveDictionaryResults results) =
    toAction "RECEIVE_DICTIONARY_RESULTS" $ object
      [ key .= defs | (key, defs) <- results ]
  toJSON (SetHighscore daily weekly) =
    toAction "SET_HIGHSCORE" $ object
      [ "daily"  .= daily
      , "weekly" .= weekly ]
  toJSON (UpdateHighscore daily weekly) =
    toAction "UPDATE_HIGHSCORE" $ object
      [ "daily"  .= daily
      , "weekly" .= weekly ]
