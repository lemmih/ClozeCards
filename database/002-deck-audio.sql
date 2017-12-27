BEGIN;
select _v.register_patch('002-deck-audio', ARRAY['000-base'], NULL);

ALTER TABLE decks ADD audio_url text;

COMMIT;
