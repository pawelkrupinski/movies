#!/usr/bin/env bash
#
# One-way sync of the country convergence ENRICHMENT CACHE from PROD
# (`convergence_test`, over the ssh tunnel prod-tunnel.sh opens to the Mongo host)
# into the local dev Mongo (:28017). One-shot, on-demand.
#
# Why this exists: the convergence suite replays a country's real repertoire and
# resolves it against TMDB/IMDb/Filmweb/RT/Metacritic. Cold, that is tens of
# thousands of live calls — Poland alone took 25 minutes, and the three CI legs
# together exceeded the workflow's 75-minute ceiling. The answers are cached in
# `convergence_test.enrichment_cache_<country>` with a 1-day TTL, and CI keeps
# that cache on the shared cluster precisely so each run inherits the last one's
# work. This script gives a DEV BOX the same head start, so a local
# `convergenceGermany` replays from disk instead of re-asking the internet.
#
# STRICTLY ONE-WAY, prod → local. Nothing here writes to prod: the dump is a
# read, and the restore targets the local instance only. A local run pointed at
# the synced copy (KINOWO_CONVERGENCE_CACHE_URI unset, so it falls back to the
# local MONGODB_URI) fills its OWN cache and never pushes back — which is the
# point. Prod's copy is CI's, and a laptop must not be able to poison it with a
# rate-limited 429 pinned as a verdict.
#
# NOT --drop, unlike sync-title-rules.sh: this is a cache, not a curated rule
# set. An entry the local box learned that prod hasn't is still a valid cached
# answer, and dropping it would throw away work for no benefit. Entries are
# upserted by `_id` (the credential-masked request key), so re-running refreshes
# what prod knows and leaves the rest. The TTL index on `fetchedAt` expires
# anything stale on either side within a day.
#
# Reads from .env.local (line-by-line, NOT sourced — the Mongo URIs contain
# `&`/`?`):
#   MONGO_ROOT_URI    prod tunnel = sync SOURCE (falls back to MONGODB_URI)
#   KINOWO_MONGO_SSH  ssh target for the prod tunnel (optional; see prod-tunnel.sh)
#   LOCAL_MONGO_URI   sync TARGET (default: mongodb://127.0.0.1:${LOCAL_MIRROR_PORT:-28017}/?directConnection=true)
#
# Usage:  scripts/local-mirror/sync-enrichment-cache.sh [--dry-run] [country…]
#           --dry-run   dump + count only; don't touch the local collections
#           country…    pl / de / uk / us / es (default: all five)
set -euo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(cd "$HERE/../.." && pwd)"
# Shared plumbing for every prod-sourced local script: envval (the .env.local
# reader), the tunnel, and — where relevant — the local Mongo starter. Sourced
# HERE, above the first envval call, because it is what defines it. Sourcing is
# side-effect free by contract, so an early source starts nothing.
. "$HERE/prod-tunnel.sh"
. "$HERE/local-mongo.sh"

DB="convergence_test"
DRY=""
COUNTRIES=()
for arg in "$@"; do
  case "$arg" in
    --dry-run)  DRY=1 ;;
    pl|de|uk|us|es) COUNTRIES+=("$arg") ;;
    *) echo "[enrich-cache] unknown argument: $arg (see the header for usage)" >&2; exit 1 ;;
  esac
done
[ ${#COUNTRIES[@]} -gt 0 ] || COUNTRIES=(pl de uk us es)


SRC="$(envval MONGO_ROOT_URI)"; [ -n "$SRC" ] || SRC="$(envval MONGODB_URI)"
DST="$(envval LOCAL_MONGO_URI)"; DST="${DST:-mongodb://127.0.0.1:${LOCAL_MIRROR_PORT:-28017}/?directConnection=true}"
[ -n "$SRC" ] || { echo "[enrich-cache] set MONGO_ROOT_URI (or MONGODB_URI) in .env.local" >&2; exit 1; }

# ── ensure the prod tunnel (source), starting our OWN ssh forward only when
# nothing already serves :27017 — never fighting a tunnel someone else owns.
# Shared with mirror.sh and sync-title-rules.sh so prod moving hosts is one edit.
init_prod_tunnel "enrich-cache" "$SRC"
TMP=""
cleanup() {
  close_prod_tunnel
  [ -n "$TMP" ] && rm -rf "$TMP" || true
}
trap cleanup EXIT INT TERM


ensure_prod_tunnel || { echo "[enrich-cache] prod Mongo unreachable — can you 'ssh' to the mongo host? (see prod-tunnel.sh)" >&2; exit 1; }
[ -n "$DRY" ] || ensure_local_mongo

TMP="$(mktemp -d)"
TOTAL=0
for CODE in "${COUNTRIES[@]}"; do
  COLL="enrichment_cache_${CODE}"
  echo "[enrich-cache] dumping prod ${DB}.${COLL}…"
  # numParallelCollections=1: the tunnel is a single proxied connection and a
  # convergence run may be filling this cache at the same time; one cursor at a
  # time is what it reliably sustains.
  mongodump --uri="$SRC" --db="$DB" --collection="$COLL" --out="$TMP" \
            --numParallelCollections=1 --quiet

  BSON="$TMP/$DB/$COLL.bson"
  if [ ! -f "$BSON" ]; then
    echo "[enrich-cache] $COLL: nothing dumped — prod has no cache for '$CODE' yet, skipping"
    continue
  fi
  COUNT="$(bsondump --quiet "$BSON" 2>/dev/null | wc -l | tr -d ' ')"
  echo "[enrich-cache] $COLL: $COUNT entries"
  TOTAL=$((TOTAL + COUNT))

  if [ -z "$DRY" ]; then
    # Upsert rather than --drop: see the header. A local entry prod lacks is
    # still a valid cached answer, and the TTL retires stale ones either way.
    mongorestore --uri="$DST" --db="$DB" --collection="$COLL" "$BSON" \
                 --numInsertionWorkersPerCollection=4 --quiet
    echo "[enrich-cache] $COLL → local $DB.$COLL"
  fi
done

if [ -n "$DRY" ]; then
  echo "[enrich-cache] dry run: $TOTAL entries available, local left untouched"
else
  echo "[enrich-cache] synced $TOTAL entries into local $DB"
  echo "[enrich-cache] local convergence runs now replay from it — leave KINOWO_CONVERGENCE_CACHE_URI unset"
  echo "[enrich-cache]   MONGODB_URI=\"$DST\" sbt convergencePoland"
fi
