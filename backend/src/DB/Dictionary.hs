{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module DB.Dictionary where

import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Database.PostgreSQL.Simple
import Data.Maybe

import           DB.Instances               ()
import           DB.Misc
import           Types

lookupEnglish :: Connection -> UserId -> SentenceId -> Int -> Text -> IO (Maybe Text)
lookupEnglish conn userId sentenceId offset word = fmap fromOnly <$> queryMaybe conn
  "SELECT english FROM sentence_meanings\
  \ WHERE (user_id IS NULL OR user_id = ?) AND\
  \       sentence_id = ? AND\
  \       word_offset = ? AND\
  \       word = ?\
  \ ORDER BY user_id NULLS LAST\
  \ LIMIT 1"
  (userId, sentenceId, offset, word)

annotateCard :: Connection -> UserId -> Card -> IO Card
annotateCard conn userId card = do
    blocks <- worker 0 (cardChinese card)
    return card{cardChinese = blocks}
  where
    sid = cardSentenceId card
    worker offset [] = return []
    worker offset (EscapedBlock txt:xs) = (EscapedBlock txt:)<$>worker (offset + T.length txt) xs
    worker offset (block@ChineseBlock{..}:xs) = do
      mbEnglish <- lookupEnglish conn userId sid offset blockSimplified
      let offset' = offset + T.length blockSimplified
          block' = block{blockOffset = offset}
      case mbEnglish of
        Nothing -> (block':) <$> worker offset' xs
        Just english -> do
          let block'' = block'{ blockEnglish = Just english
                              , blockPinyin = fromMaybe
                                    blockPinyin
                                    (findPinyin blockDefinitions english)
                              }
          (block'':) <$> worker offset' xs

findPinyin :: [Definition] -> Text -> Maybe Text
findPinyin [] _ = Nothing
findPinyin (x:xs) english
  | english `elem` definitionEnglish x = Just (definitionPinyin x)
  | otherwise = findPinyin xs english
