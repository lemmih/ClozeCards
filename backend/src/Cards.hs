{-# LANGUAGE RecordWildCards #-}
module Cards (fetchCards) where

import           Control.Monad
import           Control.Monad.State
import           Data.Chinese.CCDict
import           Data.Chinese.Pinyin
import           Data.Chinese.Segmentation  as CC
import           Data.Function
import           Data.List
import           Data.Maybe
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Data.Text                  (Text)
import qualified Data.Text as T
import Data.Char
import           Data.Time
import           Database.PostgreSQL.Simple (Connection)

import           DB
import           Types
import Helpers

wantedWords :: Int
wantedWords = 10

instantiate :: UTCTime -> CardTemplate -> State (Set Text) (Maybe Card)
instantiate now CardTemplate{..} = do
  seen <- gets (cardTemplateWord `Set.member`)
  blocks <- mapM (tokenToBlock now cardTemplateWord cardTemplateModels) (tokenizer cardTemplateSimplified)
  if seen
    then pure Nothing
    else pure $ Just Card
          { cardWord = cardTemplateWord
          , cardSentenceId = cardTemplateSentenceId
          , cardChinese = blocks
          , cardEnglish = cardTemplateEnglish
          , cardNow     = now
          }

tokenToBlock :: UTCTime -> Text -> [(Text, Maybe UTCTime)] -> CC.Token -> State (Set Text) CardBlock
tokenToBlock _now _ _ (UnknownWord w) = pure $ EscapedBlock w
tokenToBlock now selectedWord models (KnownWord e) = do
    seen <- gets (entrySimplified e `Set.member`)
    let skip = seen || broken
    modify $ Set.insert (entrySimplified e)
    pure $ ChineseBlock
      { blockSimplified = entrySimplified e
      , blockDefinitions =
          [ Definition (variantPinyin v) (variantDefinitions v)
          | v <- entryVariants e ]
      , blockAnswers = entryAnswers e
      -- Is gap if the model is missing OR if the reviewAt time is in the past.
      , blockIsGap = isSelected || (not skip && maybe True (<now) mbReviewAt)
      , blockIsNew = not skip && isNothing mbReviewAt
      }
  where
    isSelected = selectedWord == entrySimplified e
    broken     = isNothing $ lookup (entrySimplified e) models
    mbReviewAt = join $ lookup (entrySimplified e) models

entryAnswers :: Entry -> [Text]
entryAnswers = nub . concatMap variantAnswer . entryVariants
  where
    variantAnswer Variant{..} = map (T.toLower . T.filter (not.isSpace))
      [ variantSimplified, variantTraditional
      , variantPinyin, clearToneMarks variantPinyin ]

cardify :: UTCTime -> [CardTemplate] -> [Card]
cardify now = limit 0 . catMaybes . flip evalState Set.empty . mapM (instantiate now)
  where
    limit _ [] = []
    limit n _ | n > 10 = []
    limit n (card:cards) = card : limit (n+sum (map cost (cardChinese card))) cards
    cost EscapedBlock{} = 0
    cost ChineseBlock{..}
      | blockIsNew = 3
      | blockIsGap = 1
      | otherwise  = 0

fetchCards :: Connection -> UserId -> DeckId -> Style -> IO [Card]
fetchCards conn userId deckId Review = do
  now <- getCurrentTime
  newStudy <- timeIt "FetchNew" $ fetchStudyCardsNew conn now userId deckId
  let bound = templatesToBound newStudy
  rows <- timeIt "FetchReview" $ fetchReviewCards conn userId deckId bound
  let templates =
        nubBy ((==) `on` cardTemplateWord) $
        nubBy ((==) `on` cardTemplateSentenceId) rows
  putStrLn $ "Review: " ++ show (length templates)
  return $ cardify now templates
fetchCards conn userId deckId Study = do
  now <- getCurrentTime
  newStudy <- timeIt "FetchNew" $ fetchStudyCardsNew conn now userId deckId
  let bound = templatesToBound newStudy
  rows <- timeIt "FetchCards" $ fetchStudyCards conn now userId deckId bound
  putStrLn $ "Bound: " ++ show bound
  putStrLn $ "Review: " ++ show (length rows)
  putStrLn $ "Study: " ++ show (length newStudy)
  let templates = nubBy ((==) `on` cardTemplateSentenceId) (rows++newStudy)
  -- putStrLn $ "Templates: " ++ show (head templates)
  return $ cardify now templates

templatesToBound :: [CardTemplate] -> Maybe Int
templatesToBound []     = Nothing
templatesToBound [x]    = Just (cardTemplateIndex x)
templatesToBound (x:xs)
  | all isJust (map snd (cardTemplateModels x)) = templatesToBound xs
  | otherwise = Just (cardTemplateIndex x)
