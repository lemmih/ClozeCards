BEGIN;
select _v.register_patch('000-base', NULL, NULL);

drop schema public cascade;
create schema public;

create table versions
  ( key text primary key
  , version text not null
  );

create table users
  ( id serial primary key
  , email text
  , name text not null default('')
  , hash text not null default('')
  , created_at timestamptz not null default(now())
  , weeklyReports boolean not null default(true)
  , newsletter boolean not null default(true)
  , verified boolean not null default(false)
  , lastWeeklyReport timestamptz
  , unique (email)
  );

create table tokens
  ( user_id integer references users(id) not null
  , token   uuid not null
  , created_at timestamptz not null default(now())
  );

create table texts
  ( id uuid primary key
  , body jsonb not null
  , owner integer references users(id) not null
  , created_at timestamptz not null default(now())
  );

create table decks
  ( id uuid primary key
  , owner integer references users(id) not null
  , title text not null
  , type text not null
  , tags text[] not null
  , nLikes integer not null check (nLikes >= 0) default(0)
  , nComments integer not null check (nComments >= 0) default(0)
  , slugs text[] not null check (array_length(slugs,1) IS NOT NULL)
  , text_id uuid references texts(id) not null
  , created_at timestamptz not null default(now())
  , dirty boolean not null default(true)
  , processing boolean not null default(false)
  , hidden boolean not null default(false)
  , unique (slugs)
  , unique (text_id)
  , unique (text_id)
  );

create table favorites
  ( user_id integer references users(id) not null
  , deck_id uuid references decks(id) not null
  , unique (user_id, deck_id)
  );
CREATE INDEX ON favorites(deck_id);
CREATE INDEX ON favorites(user_id);

CREATE FUNCTION upd_cached_nlikes() RETURNS TRIGGER AS $upd_cached_nlikes$
  BEGIN
    IF (TG_OP = 'DELETE') THEN
      UPDATE decks
        SET nLikes = (SELECT COUNT(*) FROM favorites WHERE deck_id = OLD.deck_id)
        WHERE id = OLD.deck_id;
    ELSIF (TG_OP = 'INSERT') THEN
      UPDATE decks
        SET nLikes = (SELECT COUNT(*) FROM favorites WHERE deck_id = NEW.deck_id)
        WHERE id = NEW.deck_id;
    END IF;
    RETURN NULL;
  END;
$upd_cached_nlikes$ LANGUAGE plpgsql;

CREATE TRIGGER upd_cached_nlikes
  AFTER INSERT OR DELETE on favorites
    FOR EACH ROW EXECUTE PROCEDURE upd_cached_nlikes();

create table notes
  ( owner integer references users(id) not null
  , deck_id uuid references decks(id) not null
  , text_id uuid references texts(id) not null
  , unique (owner, deck_id)
  , unique (text_id)
  );

create table comments
  ( deck_id uuid references decks(id) not null
  , note_id integer references users(id)
  , owner integer references users(id) not null
  , text_id uuid references texts(id) not null
  , published timestamptz not null default(now())
  , edited boolean not null default(false)
  , unique (text_id)
  );

create table notifications
  ( deck_id uuid references decks(id) not null
  , note_id integer references users(id)
  , user_id integer references users(id) not null
  , dirty boolean not null
  );
CREATE UNIQUE INDEX ON notifications (deck_id, note_id, user_id)
WHERE note_id IS NOT NULL;
CREATE UNIQUE INDEX ON notifications (deck_id, user_id)
WHERE note_id IS NULL;

create table sentences
  ( id int primary key
  , simplified text not null
  , traditional text
  , english text[] not null check (array_length(english,1) IS NOT NULL)
  );

-- What happens when we receive a response?
-- We get: user_id, stencil_id, answer
-- We update the user model for the relavant word.
-- We immediately update the schedule for the stencil.
--  if we're updating the model:
--    find sentences that touch that word and union with schedule
--  if we're creating new model:
--    find sentences that touch that word
-- Once per day: find models that were modified in the last 24 hours and
--               mark sentences that use them as dirty.

create table dirty_sentences
  ( sentence_id int references sentences(id) not null
  , user_id int references users(id) not null
  , created_at timestamptz not null default(now())
  , unique (sentence_id, user_id)
  );
create index on dirty_sentences (created_at);

create table sentence_words
  ( sentence_id int references sentences(id) not null
  , word text not null
  , unique (sentence_id, word)
  );

create table deck_sentences
  ( deck_id uuid references decks(id) ON DELETE CASCADE not null
  , word text not null
  , sentence_id int references sentences(id)
  , index int not null
  , cost int
  , unique (deck_id, index)
  , unique (deck_id, word)
  );


create view deck_words AS (
  select deck_id, deck_sentences.sentence_id, deck_sentences.word as word, sentence_words.word as model, index
    from deck_sentences
    left join sentence_words ON sentence_words.sentence_id = deck_sentences.sentence_id);

create table schedule
  ( user_id int references users(id) not null
  , sentence_id int references sentences(id) not null
  , review_at timestamptz not null
  , seen_at timestamptz
  , unique (user_id, sentence_id)
  );
CREATE INDEX ON schedule (user_id, review_at);
CREATE INDEX ON schedule (user_id, sentence_id, review_at);

create table models
  ( user_id int references users(id) not null
  , word text not null
  , stability int not null
  , review_at timestamptz not null
  , created_at timestamptz not null
  , unique (user_id, word)
  );
CREATE INDEX ON models (user_id, word);
CREATE INDEX ON models (user_id, review_at);

create table responses
  ( user_id int references users(id) not null
  , word text not null
  , sentence_id int references sentences(id) not null
  , created_at timestamptz not null default(now())
  , completed boolean not null
  , value text not null
  , shown_answer boolean not null
  , factor int not null
  );

create view dirty_schedule AS (
  SELECT t.user_id, t.sentence_id, s.seen_at, t.review_at, t.created_at
    FROM (
      SELECT l.sentence_id, l.user_id, bool_or(review_at IS NULL) as any_null, min(review_at) as review_at, l.created_at
        FROM dirty_sentences l
        JOIN sentence_words r on l.sentence_id = r.sentence_id
      LEFT JOIN models ON r.word = models.word AND models.user_id = l.user_id
      GROUP BY l.sentence_id, l.user_id, l.created_at) t
  LEFT JOIN schedule s ON s.sentence_id = t.sentence_id and s.user_id = t.user_id
   WHERE not t.any_null AND
         (s.review_at IS NULL OR s.review_at <> t.review_at));

create view schedule_by_deck AS (
  SELECT deck_id, s.user_id, s.word, s.index, s.sentence_id, s.created_at, s.review_at, s.seen_at, array_agg(models.word) as words, array_agg(models.review_at) as models
    FROM (
      SELECT deck_id, m.user_id, ps.word, ps.index, s.sentence_id, m.created_at, s.review_at, s.seen_at
        FROM deck_sentences ps, models m, schedule s
       WHERE ps.word = m.word and m.user_id = s.user_id AND
             m.review_at = s.review_at) s
    JOIN sentence_words sw on s.sentence_id = sw.sentence_id
  LEFT JOIN models on sw.word = models.word AND models.user_id = s.user_id
  GROUP BY s.deck_id, s.user_id, s.word, s.index, s.sentence_id, s.created_at, s.review_at, s.seen_at);

-- SELECT index, s.word, id, simplified, english, array_agg(models.word) as words, array_agg(models.review_at) as models
--  FROM (SELECT index, word, id, simplified, english[1] as english
--         FROM deck_sentences ps
--         JOIN sentences ON ps.sentence_id = id
--        LEFT JOIN schedule s ON ps.sentence_id = s.sentence_id AND
--                                s.user_id= 1 AND s.review_at > now()
--        WHERE deck_id = 'a8ab4d72-a4ab-4b14-a874-79e10362c194' AND
--              s.review_at IS NULL
--        ORDER BY index
--        LIMIT 10
--       ) s
--   JOIN sentence_words sw on s.id = sw.sentence_id
--   LEFT JOIN models on sw.word = models.word AND models.user_id = 1
--   GROUP BY s.index, s.word, s.id, s.simplified, s.english
--   ORDER BY s.index

create table sentence_meanings
  ( user_id int references users(id)
  , sentence_id int references sentences(id) NOT NULL
  , word_offset int NOT NULL
  , word text NOT NULL
  , english text NOT NULL
  , UNIQUE (user_id, sentence_id, word_offset, word)
  );
CREATE UNIQUE INDEX ON sentence_meanings(sentence_id, word_offset, word)
  WHERE user_id IS NULL;
-- Sigh, I want a uniqueness constraint where user_id is NULL but that's not
-- possible. :(
-- Can't reference ordinary uniqueness indexes in 'ON CONFLICT'.

CREATE FUNCTION upd_default_meaning() RETURNS TRIGGER AS $upd_default_meaning$
  DECLARE
    INP RECORD;
    myrec RECORD;
    tmp boolean;
  BEGIN
    IF (TG_OP = 'DELETE') THEN
      INP := OLD;
    ELSIF (TG_OP = 'INSERT') THEN
      INP := NEW;
    END IF;

    IF INP.user_id IS NULL THEN
      RETURN NULL;
    END IF;

    SELECT english, count(*) as count
      INTO myrec
      FROM sentence_meanings
      WHERE sentence_id = INP.sentence_id AND
            word_offset = INP.word_offset AND
            word = INP.word AND
            user_id IS NOT NULL
      GROUP BY english
      ORDER BY count desc
      LIMIT 1;
    tmp := FOUND;
    DELETE FROM sentence_meanings WHERE
      sentence_id = INP.sentence_id AND
      word_offset = INP.word_offset AND
      word = INP.word AND
      user_id IS NULL;
    IF tmp THEN
      INSERT INTO sentence_meanings VALUES
        (NULL, INP.sentence_id, INP.word_offset, INP.word, myrec.english);
    END IF;

    RETURN NULL;
  END;
$upd_default_meaning$ LANGUAGE plpgsql;

CREATE TRIGGER upd_default_meaning
  AFTER INSERT OR DELETE on sentence_meanings
    FOR EACH ROW EXECUTE PROCEDURE upd_default_meaning();

COMMIT;
