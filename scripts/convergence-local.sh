#!/usr/bin/env bash
#
# Run a country's convergence leg locally against the SAME recorded inputs CI uses.
#
#   scripts/convergence-local.sh <pl|de|uk|us|es> [sbt alias, default the full leg's]
#
# CI's verdict legs are HERMETIC: they replay the corpus + enrichment-tree PAIR `Record scrape
# fixtures` pinned (`hermetic-<code>.txt` in the `convergence-fixtures` release) and never touch the
# network. `restore-country-corpus.sh` lays that pair into this checkout — downloaded every time,
# deliberately: the pin moves nightly, and a stale local copy would test different inputs from the
# ones CI tested while looking fine. Then the suite runs hermetically, so a local run answers
# exactly the question the CI leg did, offline, failing by name on anything the pair lacks.
# There is no fallback: no pinned pair, and this stops.
#
# TMDB_API_KEY is the one input nothing replays around (TmdbClient short-circuits without it).
# It comes from the environment if set, else from the Touch ID vault, INLINE into sbt's env only —
# never from a `.env.local`, whose MONGODB_URI is production's. OMDB_API_KEY likewise, optional
# (CI has it; without it the OMDb rung of ImdbIdResolver is absent).
#
# Every collection runs on a real MongoDB replica set (change streams and the staging fold's
# transaction both need one). MONGODB_URI if given; else the itAll mongod on :28017 if it
# answers; else a throwaway mongo:8.3.11 container on :27117. Each run's databases are created
# under a MONGODB_DB prefix unique to it and dropped by the suite (`ConvergenceStorage.close`).
set -euo pipefail

CODE="${1:-}"
case "$CODE" in
    pl) SPEC=convergencePoland  ;;
    de) SPEC=convergenceGermany ;;
    uk) SPEC=convergenceUk      ;;
    us) SPEC=convergenceUs      ;;
    es) SPEC=convergenceSpain   ;;
    *)  echo "usage: $0 <pl|de|uk|us|es> [sbt alias]" >&2; exit 2 ;;
esac
SPEC="${2:-$SPEC}"
cd "$(dirname "$0")/.."

scripts/restore-country-corpus.sh "$CODE"
echo "    (overwrites any committed corpus for $CODE — 'git checkout -- test/resources/fixtures/corpus' restores it)"

if [ -z "${TMDB_API_KEY:-}" ]; then
    TMDB_API_KEY="$(secrets get movies TMDB_API_KEY 2>/dev/null || true)"
    [ -n "$TMDB_API_KEY" ] || { echo "no TMDB_API_KEY in the environment or the vault (secrets get movies TMDB_API_KEY) — the leg would resolve nothing" >&2; exit 1; }
fi
OMDB_API_KEY="${OMDB_API_KEY:-$(secrets get movies OMDB_API_KEY 2>/dev/null || true)}"
[ -n "$OMDB_API_KEY" ] || echo "    no OMDB_API_KEY — the OMDb rung runs absent, as it does not in CI" >&2

if [ -n "${MONGODB_URI:-}" ]; then
    echo "==> MongoDB from MONGODB_URI"
elif (echo >/dev/tcp/127.0.0.1/28017) 2>/dev/null; then
    MONGODB_URI="mongodb://127.0.0.1:28017/?directConnection=true"
    echo "==> reusing the itAll MongoDB on :28017"
elif (echo >/dev/tcp/127.0.0.1/27117) 2>/dev/null; then
    # REUSE whatever already answers on the port: `docker run -p` fails outright when it is taken.
    MONGODB_URI="mongodb://127.0.0.1:27117/?directConnection=true"
    echo "==> reusing the MongoDB already listening on :27117"
else
    echo "==> starting a throwaway mongo:8.3.11 single-node replica set on :27117"
    docker rm -f convergence-local-mongo >/dev/null 2>&1 || true
    docker run -d --name convergence-local-mongo -p 27117:27017 mongo:8.3.11 --replSet rs0 --bind_ip_all >/dev/null
    until docker exec convergence-local-mongo mongosh --quiet --eval 'db.runCommand({ping:1})' >/dev/null 2>&1; do sleep 1; done
    docker exec convergence-local-mongo mongosh --quiet --eval \
        'try { rs.status().ok } catch (e) { rs.initiate({_id:"rs0",members:[{_id:0,host:"127.0.0.1:27017"}]}) }' >/dev/null
    until docker exec convergence-local-mongo mongosh --quiet --eval 'rs.status().myState' 2>/dev/null | grep -q '^1$'; do sleep 1; done
    MONGODB_URI="mongodb://127.0.0.1:27117/?directConnection=true"
fi
MONGODB_DB="${MONGODB_DB:-kinowo_conv_${CODE}_$$}"
echo "    databases under the prefix $MONGODB_DB"

echo "==> sbt $SPEC"
echo "    watch: '[$CODE] <phase> done in Ns', 'scraped N/M', 'staging round N: A → B rows', 'coverage — N films'"
exec env MONGODB_URI="$MONGODB_URI" MONGODB_DB="$MONGODB_DB" \
    TMDB_API_KEY="$TMDB_API_KEY" OMDB_API_KEY="$OMDB_API_KEY" \
    KINOWO_COUNTRY="$CODE" KINOWO_COUNTRIES="$CODE" \
    KINOWO_CONVERGENCE_ENRICHMENT_FIXTURES="enrichment-$CODE" KINOWO_CONVERGENCE_HERMETIC=true \
    sbt "$SPEC"
