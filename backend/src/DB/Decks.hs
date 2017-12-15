{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module DB.Decks where

import           Control.Monad
import           Data.Char
import           Data.List
import           Data.Monoid
import qualified Data.Set                   as Set
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Time
import qualified Data.Vector                as V
import           Database.PostgreSQL.Simple

import           DB.Instances               ()
import           DB.Misc
import           Types


createContent :: Connection -> UserId -> ContentId -> Content -> IO ()
createContent conn owner contentId content =
  void $ execute conn "INSERT INTO texts(id, body, owner) VALUES (?,?,?)"
          (contentId, content, owner)

createNote :: Connection -> UserId -> DeckId -> ContentId -> IO ()
createNote conn userId deckId contentId = void $ execute conn
  "INSERT INTO notes(owner, deck_id, text_id) VALUES (?,?,?)\
  \ ON CONFLICT(owner, deck_id) DO UPDATE SET\
  \ text_id = EXCLUDED.text_id"
  (userId, deckId, contentId)

fetchNote :: Connection -> UserId -> DeckId -> IO (Maybe ContentId)
fetchNote conn userId deckId = fmap fromOnly <$> queryMaybe conn
  "SELECT text_id FROM notes WHERE owner = ? AND deck_id = ?" (userId, deckId)

createSlugs :: Connection -> DeckId -> [Text] -> IO [Text]
createSlugs conn deckId suggestedSlugs = do
  takenSlugs <- query conn "SELECT unnest(slugs) FROM decks WHERE id <> ?" (Only deckId)
  let slugSet = Set.fromList (map fromOnly takenSlugs)
      slugs = nub (map (mkSlug slugSet) suggestedSlugs)
  return slugs

createDeck :: Connection -> Deck -> IO ()
createDeck conn Deck{..} =
  -- Upsert decks, will quietly do nothing if row exists with different owner.
  void $ execute conn
      "INSERT INTO decks(id, owner, type, title, tags, slugs, text_id, created_at, hidden)\
      \          VALUES (?,  ?,     ?,     ?,    ?,    ?,     ?,       ?,          ?)\
      \ ON CONFLICT (id) DO UPDATE\
      \ SET type = EXCLUDED.type,\
      \     tags = EXCLUDED.tags,\
      \     title = EXCLUDED.title,\
      \     slugs = EXCLUDED.slugs,\
      \     text_id = EXCLUDED.text_id,\
      \     hidden = EXCLUDED.hidden\
      \ WHERE decks.owner = EXCLUDED.owner"
      ( deckId, deckOwner, deckType, deckTitle, V.fromList deckTags
      , V.fromList deckSlugs, deckContentId, deckCreatedAt, deckHidden)

mkSlug slugSet txt = trySlugs (slug : [ slug <> "-" <> T.pack (show n) | n <- [1..]])
  where
    (<>) = T.append
    slug = slugify txt
    trySlugs [] = ""
    trySlugs (attempt:rest)
      | attempt `Set.notMember` slugSet = attempt
      | otherwise = trySlugs rest
    slugify = T.map replace . T.filter (not . banned) . T.strip . T.toLower
    replace c | isSpace c = '-'
    replace c = c
    banned '-' = False
    banned c   = isPunctuation c

deckBySlug :: Connection -> Text -> IO (Maybe Deck)
deckBySlug conn slug = queryMaybe conn
  "SELECT id, owner, type, title, tags, slugs, nLikes, nComments, text_id, created_at, dirty, hidden\
  \  FROM decks\
  \ WHERE ? = ANY(slugs)" (Only slug)

setDeckTags :: Connection -> DeckId -> [Tag] -> IO ()
setDeckTags conn deckId tags =
  void $ execute conn "UPDATE decks SET tags = ?\
                      \ WHERE id = ?" (V.fromList tags, deckId)

setDeckFlags :: Connection -> DeckId -> Bool -> Bool -> IO ()
setDeckFlags conn deckId dirty processing = void $
  execute conn "UPDATE decks SET dirty = ?, processing = ?\
               \ WHERE id = ?" (dirty, processing, deckId)

setDeckSentences :: Connection -> DeckId -> [(Text, Maybe (SentenceId, Int))] -> IO ()
setDeckSentences conn deckId sentences = do
  void $ execute conn "DELETE FROM deck_sentences WHERE deck_id = ?" (Only deckId)
  void $ executeMany conn
            "INSERT INTO deck_sentences (deck_id, word, sentence_id, index, cost)\
            \ VALUES (?, ?, ?, ?, ?)"
            [ (deckId, w, fmap fst sid, n::Int, fmap snd sid)
            | (n,(w,sid)) <- zip [0..] sentences ]

fetchContent :: Connection -> ContentId -> IO Content
fetchContent conn contentId = fromOnly <$> querySingle conn
  "SELECT body FROM texts WHERE id = ?" (Only contentId)

fetchDirtyDecks :: Connection -> IO [Deck]
fetchDirtyDecks conn =
  query conn "SELECT id, owner, type, title, tags, slugs, nLikes, nComments, text_id, created_at, dirty, hidden\
             \  FROM decks\
             \ WHERE dirty = true" ()



fetchReviewCards :: Connection -> UserId -> DeckId -> Maybe Int -> IO [CardTemplate]
fetchReviewCards conn userId deckId mbBound = query conn
  "SELECT DISTINCT ON (created_at) index, word, id, simplified, english[1], words, models\
  \  FROM schedule_by_deck, sentences\
  \ WHERE user_id = ? AND deck_id = ? AND\
  \       (? IS NULL OR index < ?) AND\
  \       sentence_id = id\
  \ ORDER BY created_at, seen_at NULLS FIRST, review_at desc, sentence_id\
  \ LIMIT 10"
  (userId, deckId, mbBound, mbBound)
-- join deck_sentences and models
-- filter deck_id = pid
-- filter review_at < now
-- filter user_id = uid
--
fetchStudyCards :: Connection -> UTCTime -> UserId -> DeckId -> Maybe Int -> IO [CardTemplate]
fetchStudyCards conn now userId deckId mbBound = query conn
  "SELECT DISTINCT ON (review_at) index, word, id, simplified, english[1], words, models\
  \  FROM schedule_by_deck, sentences\
  \ WHERE user_id = ? AND deck_id = ? AND\
  \       (? IS NULL OR index < ?) AND\
  \       review_at < ? AND\
  \       sentence_id = id\
  \ ORDER BY review_at desc, seen_at NULLS FIRST, sentence_id\
  \ LIMIT 10"
  (userId, deckId, mbBound, mbBound, now)

fetchStudyCardsNew :: Connection -> UTCTime -> UserId -> DeckId -> IO [CardTemplate]
fetchStudyCardsNew conn now userId deckId = query conn
  "SELECT index, s.word, id, simplified, english, array_agg(sw.word) as words, array_agg(models.review_at) as model\
  \ FROM (SELECT index, word, id, simplified, english[1] as english\
  \        FROM deck_sentences ps\
  \        JOIN sentences ON ps.sentence_id = id\
  \       LEFT JOIN schedule s ON ps.sentence_id = s.sentence_id AND\
  \                               s.user_id= ? AND s.review_at > ?\
  \       WHERE deck_id = ? AND\
  \             s.review_at IS NULL\
  \       ORDER BY index\
  \       LIMIT 10\
  \      ) s\
  \  JOIN sentence_words sw on s.id = sw.sentence_id\
  \  LEFT JOIN models on sw.word = models.word AND models.user_id = ?\
  \  GROUP BY s.index, s.word, s.id, s.simplified, s.english\
  \  ORDER BY s.index"
  (userId, now, deckId, userId)
  -- "SELECT index, word, id, simplified, english[1], words, models\
  -- \  FROM (\
  -- \   SELECT index, deck_words.sentence_id, deck_words.word, \
  -- \          array_agg(deck_words.model) as words, array_agg(review_at) as models, bool_or(review_at IS NULL) new\
  -- \     FROM deck_words\
  -- \     LEFT JOIN models on models.word = model and user_id=?\
  -- \     WHERE deck_id=?\
  -- \  GROUP BY deck_words.word, index, deck_words.sentence_id ORDER BY index) tmp, sentences\
  -- \  WHERE new = true and sentences.id=tmp.sentence_id\
  -- \  LIMIT 10"
  -- ( userId, deckId )

-- FIXME: FavOnly?
searchDecks :: Connection -> [Text] -> [Tag] -> DeckOrdering -> Offset -> IO [Deck]
searchDecks conn keyWords tags ordering offset = query conn
    ("SELECT id, owner, type, title, tags, slugs, nLikes, nComments, text_id, created_at, dirty, hidden\
    \  FROM decks\
    \ WHERE title ILIKE ALL(?) AND not hidden"
    <> orderBy <>
    " OFFSET ?\
    \ LIMIT 10")
    (V.fromList keyWords, offset)
  where
    orderBy =
      case ordering of
        ByLikes    -> " ORDER BY nLikes desc"
        ByDate     -> " ORDER BY created_at desc"
        ByTrending -> " ORDER BY nLikes desc"

setFavorite :: Connection -> UserId ->  DeckId -> IO ()
setFavorite conn userId deckId = void $ execute conn
  "INSERT INTO favorites (user_id, deck_id) VALUES (?, ?)\
  \ ON CONFLICT DO NOTHING"
  (userId, deckId)

unsetFavorite :: Connection -> UserId ->  DeckId -> IO ()
unsetFavorite conn userId deckId = void $ execute conn
  "DELETE FROM favorites WHERE user_id = ? AND deck_id = ?"
  (userId, deckId)

setVisibility :: Connection -> UserId -> DeckId -> Bool -> IO ()
setVisibility conn userId deckId hidden = void $ execute conn
  "UPDATE decks SET hidden = ? WHERE owner = ? AND id = ?"
  (hidden, userId, deckId)
