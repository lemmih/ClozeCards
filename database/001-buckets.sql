BEGIN;
select _v.register_patch('001-buckets', ARRAY['000-base'], NULL);

create table buckets
  ( key text not null
  , tag text
  , at timestamptz not null
  , value int not null
  , unique(key, tag, at)
  );

CREATE INDEX ON buckets(key, at);
CREATE INDEX ON buckets(key, tag, at);

COMMIT;
