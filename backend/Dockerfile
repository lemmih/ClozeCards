FROM haskell:8.4
RUN apt-get update && apt-get install -y libpq-dev
COPY clozecards.cabal .
RUN cabal update && cabal install --only-dependencies
COPY ./src/ ./src/
RUN ghc -o ClozeCards -isrc src/Main.hs

FROM ubuntu:17.10

# Set the locale
ENV LANG C.UTF-8
ENV LANGUAGE C.UTF-8
ENV LC_ALL C.UTF-8

RUN apt-get update && apt-get install -y netbase libpq-dev

COPY --from=0 ClozeCards /root/

EXPOSE 8000
ENV SQL_DB="dbname=ClozeCards user=postgres port=5432 host=psql"
ENTRYPOINT /root/ClozeCards
