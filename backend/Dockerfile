FROM haskell:8.2.1
RUN apt-get update && apt-get install -y libpq-dev
COPY clozecards.cabal .
RUN cabal update && cabal install --only-dependencies
COPY ./src/ ./src/
RUN ghc -o ClozeCards -isrc src/Main.hs

FROM ubuntu:17.10

RUN apt-get update && apt-get install -y netbase libpq-dev

COPY --from=0 ClozeCards /root/

EXPOSE 8000
ENV SQL_DB="dbname=ClozeCards user=postgres port=5432 host=psql"
ENTRYPOINT /root/ClozeCards
