{-# LANGUAGE RecordWildCards #-}
module Logic
  ( answerScore
  , addResponse
  , updateModel
  , computeModel
  , updateReviewAt
  , knownWordStability
  , minStability) where

import           Control.Concurrent
import           Control.Monad
import           Data.Maybe
import           Data.Pool
import qualified Data.Text                  as T
import           Data.Time
import           Database.PostgreSQL.Simple (Connection)
import Data.Chinese.CCDict
import Data.Char

import           DB
import           Helpers
import           Types

{-
EarlyReview: 1 points
SeenAnswer: 1 points
PlainAnswer: 2 points
PinyinAnswer: 3 points
ChineseAnswer: 4 points

-}
-- How many points to award an answer.
answerScore Response{..} earlyReview
  | earlyReview || responseShownAnswer = 1
  | answer `elem` chineseAnswers       = 4
  | answer `elem` pinyinAnswers        = 3
  | otherwise                          = 2
  where
    answer = trim responseValue
    Just e = lookupMatch responseWord
    trim = T.toLower . T.filter (not.isSpace)
    pinyinAnswers = map (trim.variantPinyin) (entryVariants e)
    chineseAnswers = map trim $ concat [ [variantSimplified v, variantTraditional v] | v <- entryVariants e ]


------------------------------------------------------------------------------
-- Parameters
minStability :: NominalDiffTime
minStability = 30 -- minimumDelay between reviews in seconds

maxStability :: NominalDiffTime
maxStability = 60*60*24*365*10

-- Failing to recall a word decreases time to next view while also inserting
-- a recap review after 5 minutes. Passing this recap review does not increase
-- time to next review. Failing the recap DOES decrease the time to next review.
-- Expressed in seconds.
failureRecap :: NominalDiffTime
failureRecap = 5*60

-- If the student successfully recalls a word without having seen it before,
-- we quickly bump up the review time to 24 hours.
knownWordStability :: NominalDiffTime
knownWordStability = 60*60*24




updateModel response model = model
    { modelStability = round newStability
    , modelCreatedAt = responseCreatedAt response
    , modelReviewAt  = newReviewAt }
  where
    newStability = updateStability response model
    newReviewAt  = updateReviewAt response newStability

updateStability Response{..} Model{..}
  | responseShownAnswer && extremeFactor     = minStability
  | not responseShownAnswer && extremeFactor = maxStability
  | not responseShownAnswer && earlyReview = stability + fasttrack
  | not responseShownAnswer && recovery    = stability
  | responseShownAnswer                    = max minStability (stability / factor)
  | otherwise                              = max stability fasttrack * factor
  where
    extremeFactor = responseFactor > 10000
    stability = fromIntegral modelStability
    factor = realToFrac responseFactor
    fasttrack = diffUTCTime responseCreatedAt modelCreatedAt
    earlyReview = responseCreatedAt < modelReviewAt
    recovery = modelReviewAt < addUTCTime stability modelCreatedAt

updateReviewAt Response{..} stability
  | responseShownAnswer = min earlyRepeat desiredRepeat
  | otherwise           = desiredRepeat
  where
    earlyRepeat   = addUTCTime failureRecap responseCreatedAt
    desiredRepeat = addUTCTime stability responseCreatedAt

computeModel :: Maybe Model -> Response -> Model
computeModel mbModel response@Response{..} =
  case mbModel of
    Nothing ->
      let newStability | responseShownAnswer = minStability
                       | otherwise           = knownWordStability
          newModel = Model
            { modelUserId    = responseUserId
            , modelWord      = responseWord
            , modelStability = round newStability
            , modelCreatedAt = responseCreatedAt
            , modelReviewAt  = updateReviewAt response newStability
            }
      in newModel
    Just model -> updateModel response model

addResponse :: Pool Connection -> UserId -> Response -> IO ()
addResponse pool userId response = do
  let word = responseWord response
      now = responseCreatedAt response
      mbSentenceId = responseSentenceId response
  newModel <- runDB pool $ \db -> do
    createResponse db response
    mbModel <- fetchModel db userId word
    when (responseCompleted response) $ do
      case mbModel of
        Nothing -> return ()
        Just model -> clearSchedule db userId (modelReviewAt model)
      let newModel = computeModel mbModel response
      createModel db newModel
    case mbSentenceId of
      Nothing -> return ()
      Just sentenceId ->
        when (responseCompleted response) $ do
          models <- fetchSentenceModels db userId sentenceId
          let mbReviewAts = sequence [ fmap modelReviewAt mbModel | (_,mbModel) <- models]
          case mbReviewAts of
            Nothing -> return ()
            Just [] -> return ()
            Just reviewAt ->
              setSchedule db userId sentenceId now (minimum reviewAt)
    return $ isNothing mbModel
  when (responseCompleted response) $
    -- Marking sentences dirty happens in the background such that
    -- we can continue serving requests as quickly as possible.
    void $ forkIO $ logExceptions "addReponse-updDirtySentences" $ runDBUnsafe pool $ \db ->
      if newModel
        then
        -- Mark sentences as dirty even if they've never been seen before.
          markAllSentencesDirty db userId word
        -- Only mark sentences that have been seen. That is,
        -- since we're not creating a model, only sentences that
        -- already have a review time are affected
        else
          markSeenSentencesDirty db userId word
