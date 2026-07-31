#!/usr/bin/env bash
#
# Run a country's convergence leg locally against the SAME recorded inputs CI uses.
#
# The suite's inputs are not in the repository: the enrichment tree (recorded responses
# plus the remembered-verdict cache) lives in the rolling `convergence-fixtures` release,
# republished by every leg. Running without it means fetching thousands of live answers,
# which is slow, rate-limited, and produces a corpus nobody else can reproduce — so a
# local run that skips this is not the same experiment CI ran.
#
# Downloading it every time is deliberate. The asset is replaced in place, and a stale
# local copy is invisible: the run simply refetches whatever it lacks and looks fine while
# testing different inputs from the ones CI tested.
#
#   scripts/convergence-local.sh pl              # in-memory repositories (what CI runs)
#   scripts/convergence-local.sh pl --mongo      # every collection on a real MongoDB
#
# --mongo starts a single-node replica set in Docker on :27117 — a replica set because
# change streams and the staging fold's transaction are both rejected by a standalone
# mongod, and both are on the path. It is a throwaway container, never a tunnel to
# anything real.
set -euo pipefail

CODE="${1:-}"
case "$CODE" in
    pl) SPEC=convergencePoland  ;;
    de) SPEC=convergenceGermany ;;
    uk) SPEC=convergenceUk      ;;
    *)  echo "usage: $0 <pl|de|uk> [--mongo]" >&2; exit 2 ;;
esac
USE_MONGO="${2:-}"

RELEASE_TAG=convergence-fixtures
TREE="test/resources/fixtures/enrichment-$CODE"

echo "==> fetching $RELEASE_TAG / enrichment-$CODE.tar.gz"
STAGE=$(mktemp -d)
trap 'rm -rf "$STAGE"' EXIT
if gh release download "$RELEASE_TAG" --pattern "enrichment-$CODE.tar.gz" --dir "$STAGE" --clobber; then
    rm -rf "$TREE"
    tar -xzf "$STAGE/enrichment-$CODE.tar.gz"
    echo "    $(find "$TREE" -type f -not -path '*/.enrichment-cache/*' | wc -l | tr -d ' ') recorded responses, \
$( { find "$TREE/.enrichment-cache" -name '*.entry' 2>/dev/null || true; } | wc -l | tr -d ' ') remembered verdicts"
else
    echo "    no asset yet — this run fetches live and records what it learns" >&2
fi

# `.env.local` holds TMDB_API_KEY, and `Env` reads it from the WORKING DIRECTORY. A run
# from a fresh worktree without it enriches NOTHING and says so (the coverage guard fails
# the suite) — but it wastes a full run to find out.
if [ ! -f .env.local ] && [ -f "$(git rev-parse --show-toplevel)/../movies/.env.local" ]; then
    cp "$(git rev-parse --show-toplevel)/../movies/.env.local" .env.local
    echo "==> copied .env.local (TMDB_API_KEY) from the root checkout"
fi

if [ "$USE_MONGO" = "--mongo" ]; then
    # REUSE whatever already answers on the port, rather than assuming the only thing
    # that could be there is a container of ours. `docker run -p` fails outright when the
    # port is taken, and under `set -e` that kills the run before it starts — which is
    # exactly what happened the first time this script was used, because an earlier
    # hand-started container was still holding :27117.
    if (echo >/dev/tcp/127.0.0.1/27117) 2>/dev/null; then
        echo "==> reusing the MongoDB already listening on :27117"
    else
        echo "==> starting a throwaway mongo:7 single-node replica set on :27117"
        docker rm -f convergence-local-mongo >/dev/null 2>&1 || true
        docker run -d --name convergence-local-mongo -p 27117:27017 mongo:7 --replSet rs0 --bind_ip_all >/dev/null
        until docker exec convergence-local-mongo mongosh --quiet --eval 'db.runCommand({ping:1})' >/dev/null 2>&1; do sleep 1; done
        docker exec convergence-local-mongo mongosh --quiet --eval \
            'try { rs.status().ok } catch (e) { rs.initiate({_id:"rs0",members:[{_id:0,host:"127.0.0.1:27017"}]}) }' >/dev/null
        until docker exec convergence-local-mongo mongosh --quiet --eval 'rs.status().myState' 2>/dev/null | grep -q '^1$'; do sleep 1; done
    fi
    export MONGODB_URI="mongodb://127.0.0.1:27117/?directConnection=true"
    echo "    every repository will run on it"
else
    # Explicitly unset: a stray MONGODB_URI in the environment would silently switch the
    # storage under the run, and the two paths do not currently behave the same.
    unset MONGODB_URI
    echo "==> in-memory repositories (set --mongo for a real database)"
fi

export KINOWO_COUNTRY="$CODE" KINOWO_COUNTRIES="$CODE"
export KINOWO_CONVERGENCE_ENRICHMENT_FIXTURES="enrichment-$CODE"

echo "==> sbt $SPEC"
echo "    watch: '[$CODE] <phase> done in Ns', 'scraped N/M', 'staging round N: A → B rows', 'coverage — N films'"
exec sbt "$SPEC"
