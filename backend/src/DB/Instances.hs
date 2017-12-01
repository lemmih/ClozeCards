{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module DB.Instances () where

import qualified Data.Aeson                           as Aeson
import qualified Data.Set                             as S
import qualified Data.Vector                          as V
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToField

import           Types

instance FromField ContentState where
  fromField field input = do
    json <- fromField field input
    case Aeson.fromJSON json of
      Aeson.Error msg   -> fail msg
      Aeson.Success val -> pure val

instance ToField ContentState where
  toField = toField . Aeson.toJSON

instance FromRow User where
  fromRow = do
    (userId, email, name, userHash, favorites) <- fromRow
    return $ User userId email name userHash (S.fromList $ V.toList favorites)

instance FromRow Deck where
  fromRow = do
    ((deckId, deckOwner, deckType, deckTitle) :.
     (tags, slugs, deckNLikes, deckNComments) :.
     (deckContentId, deckDirty, deckHidden)) <- fromRow
    let deckTags = V.toList tags
        deckSlugs = V.toList slugs
        deckProcessing = False -- FIXME
    return Deck{..}

instance FromRow CardTemplate where
  fromRow = do
    ( cardTemplateIndex, cardTemplateWord, cardTemplateSentenceId
     ,cardTemplateSimplified, cardTemplateEnglish, ws, models) <- fromRow
    let cardTemplateModels = zip (V.toList ws) (V.toList models)
    return CardTemplate{..}

instance FromRow Model where
  fromRow = do
    (modelUserId, modelWord, modelStability, modelReviewAt, modelCreatedAt) <- fromRow
    return Model{..}

instance FromRow (Maybe Model) where
  fromRow = do
    mbRow <- fromRow
    return $ do
      (modelUserId, modelWord, modelStability, modelReviewAt, modelCreatedAt) <- mbRow
      pure Model{..}
