#!/usr/bin/env bash
#
# Full corpus reset. Wipes the scrape / enrichment / staging / read-model
# collections so the worker re-scrapes every cinema from scratch and re-projects
# the read model; serving is empty until that completes (a few minutes of sparse
# repertoire — acceptable per the "short downtime is fine" rule).
#
# Two targets:
#   (default)   PROD  — kinowo (web) + kinowo-worker (worker) on Fly, prod Mongo
#                       over the `flyctl proxy ... --app kinowo-mongo` tunnel. The
#                       Fly machines are stopped FIRST so the worker can't race a
#                       half-finished scrape into a collection we're dropping, and
#                       restarted only after the wipe lands.
#   --local           — the native brew Mongo the local web+worker share (:28017,
#                       db `kinowo_local`; see scripts/local-mirror/start-local-mongo.sh).
#                       No Fly apps and no tunnel — stop your local `sbt web/run` +
#                       `sbt worker/run` yourself first so the worker doesn't
#                       re-scrape into the wipe.
#
# Drops: detailCache, freshness, movies, pending_movies, tasks, web_movies,
#        web_screenings — i.e. everything the scrape→enrich→project pipeline
#        rebuilds from scratch (`pending_movies` is the staging incubator added
#        alongside `movies`). --local ALSO drops scheduled_runs (see below).
#   NOT dropped — operational state, curated config, and accounts the worker does
#   not rebuild: uptimeBuckets, uptimeServiceTags (monitoring history), titleRules
#   (admin-curated — seed it locally from prod with
#   scripts/local-mirror/sync-title-rules.sh), users, userStates (accounts). Add
#   filmwebFallback /
#   filmwebFallbackMeta (per-cinema fallback prober state) and/or
#   normalizationReports (a backfill report) to COLLECTIONS below if you want a
#   fully-clean slate that re-derives those too.
#   scheduled_runs is kept in PROD (worker scheduling — dropping it re-fires every
#   sweep at once) but DROPPED in --local: the fixture worker's reapers are
#   once-daily and gate on it, so a leftover `scrape@<today>` record makes a
#   restarted local stack skip the scrape and never repopulate kinowo_local.
#
# PROD reads MONGODB_URI (prod tunnel) + MONGODB_DB from .env.local.
# LOCAL defaults to mongodb://127.0.0.1:${LOCAL_MIRROR_PORT:-28017}/?directConnection=true
#       and db `kinowo_local`; override with LOCAL_MONGO_URI / LOCAL_MONGO_DB.
#
# Usage:  scripts/reset-corpus.sh [--local] [--dry-run] [--yes]
#           --local     reset the local brew Mongo instead of prod
#           --dry-run   list which collections WOULD be dropped, change nothing
#           --yes       skip the confirmation prompt
set -euo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(cd "$HERE/.." && pwd)"

WEB_APP="kinowo"
WORKER_APP="kinowo-worker"
MONGO_APP="kinowo-mongo"
COLLECTIONS=(detailCache freshness movies pending_movies tasks web_movies web_screenings)

MODE="prod"
ASSUME_YES=""
DRY=""
for arg in "$@"; do
  case "$arg" in
    --local)   MODE="local" ;;
    --dry-run) DRY=1 ;;
    --yes)     ASSUME_YES=1 ;;
    *) echo "[reset] unknown argument: $arg (see the header for usage)" >&2; exit 1 ;;
  esac
done

# LOCAL also drops scheduled_runs: the local fixture worker's scrape/enrich
# reapers run once-daily and gate on this collection, so a leftover
# `scrape@<today>` record makes a freshly-restarted local stack skip the scrape —
# leaving an empty kinowo_local that never repopulates. Dropping it forces the
# next boot to re-scrape from scratch. PROD deliberately keeps it: dropping it
# there would re-fire every scheduled sweep at once.
[ "$MODE" = "local" ] && COLLECTIONS+=(scheduled_runs)

# Read KEY=VALUE from .env.local WITHOUT sourcing it — the Mongo URI contains
# `&`/`?`, which a shell `source` would mangle (same approach as mirror.sh).
envval() {
  { grep -E "^$1=" "$ROOT/.env.local" 2>/dev/null || true; } | head -1 | cut -d= -f2- \
    | sed -e 's/^["'"'"']//' -e 's/["'"'"']$//'
}

if [ "$MODE" = "local" ]; then
  URI="${LOCAL_MONGO_URI:-mongodb://127.0.0.1:${LOCAL_MIRROR_PORT:-28017}/?directConnection=true}"
  DB="${LOCAL_MONGO_DB:-kinowo_local}"
else
  URI="$(envval MONGODB_URI)"
  DB="$(envval MONGODB_DB)"
  [ -n "$URI" ] || { echo "[reset] set MONGODB_URI in .env.local (prod tunnel)" >&2; exit 1; }
  [ -n "$DB" ]  || { echo "[reset] set MONGODB_DB in .env.local" >&2; exit 1; }
fi

echo "[reset] $MODE — target db '$DB'; ${DRY:+would }drop: ${COLLECTIONS[*]}"
if [ "$MODE" = "local" ]; then
  echo "[reset] LOCAL: stop your local web+worker (sbt) first so the worker can't re-scrape into the wipe."
  CONFIRM_WORD="wipe-local"
else
  echo "[reset] will stop+restart Fly apps: $WEB_APP, $WORKER_APP"
  CONFIRM_WORD="wipe"
fi
if [ -z "$DRY" ] && [ -z "$ASSUME_YES" ]; then
  read -r -p "[reset] This wipes the $MODE corpus. Type '$CONFIRM_WORD' to proceed: " ans
  [ "$ans" = "$CONFIRM_WORD" ] || { echo "[reset] aborted."; exit 1; }
fi

# --- stop / start all machines for a Fly app (prod only) --------------------
stop_app() {
  local app="$1"
  echo "[reset] stopping $app..."
  local ids
  ids="$(flyctl machines list --app "$app" --json | jq -r '.[].id')"
  [ -n "$ids" ] || { echo "[reset]   (no machines)"; return; }
  for id in $ids; do flyctl machine stop "$id" --app "$app"; done
}

start_app() {
  local app="$1"
  echo "[reset] starting $app..."
  local ids
  ids="$(flyctl machines list --app "$app" --json | jq -r '.[].id')"
  [ -n "$ids" ] || { echo "[reset]   (no machines)"; return; }
  for id in $ids; do flyctl machine start "$id" --app "$app"; done
}

# --- tunnel to prod Mongo (prod only), torn down on any exit ----------------
PROXY_PID=""
cleanup() {
  [ -n "$PROXY_PID" ] && kill "$PROXY_PID" 2>/dev/null || true
}
trap cleanup EXIT

open_tunnel() {
  echo "[reset] opening Mongo tunnel ($MONGO_APP)..."
  flyctl proxy 27017:27017 --app "$MONGO_APP" >/dev/null 2>&1 &
  PROXY_PID=$!
  for _ in $(seq 1 30); do  # wait up to ~30s for the tunnel to answer a ping
    if mongosh "$URI" --quiet --eval 'db.runCommand({ping:1})' >/dev/null 2>&1; then return 0; fi
    sleep 1
  done
  echo "[reset] prod Mongo not reachable via the tunnel" >&2
  exit 1
}

require_local_mongo() {
  mongosh "$URI" --quiet --eval 'db.runCommand({ping:1})' >/dev/null 2>&1 && return 0
  echo "[reset] local Mongo not reachable at $URI — start it with scripts/local-mirror/start-local-mongo.sh" >&2
  exit 1
}

# --- run ---------------------------------------------------------------------
if [ "$MODE" = "prod" ]; then
  [ -z "$DRY" ] && { stop_app "$WEB_APP"; stop_app "$WORKER_APP"; }
  open_tunnel               # needed to query, even for --dry-run
else
  require_local_mongo
fi

echo "[reset] ${DRY:+[dry-run] }collections in '$DB':"
# `getSiblingDB($DB)` targets the db explicitly — the --local URI carries no db,
# so a bare `db` would resolve to `test`.
COLLS_JS="$(printf '"%s",' "${COLLECTIONS[@]}")"
mongosh "$URI" --quiet --eval "
  var dry = ${DRY:-0} == 1;
  var target = db.getSiblingDB('$DB');
  var wanted = [${COLLS_JS%,}];
  var existing = target.getCollectionNames();
  wanted.forEach(function (c) {
    if (existing.indexOf(c) === -1) { print('  skip       ' + c + ' (absent)'); return; }
    if (!dry) target.getCollection(c).drop();
    print('  ' + (dry ? 'would drop ' : 'dropped   ') + c);
  });
"

if [ "$MODE" = "prod" ] && [ -z "$DRY" ]; then
  start_app "$WEB_APP"
  start_app "$WORKER_APP"
fi

if [ -n "$DRY" ]; then
  echo "[reset] dry-run only — nothing changed."
else
  echo "[reset] done. Worker will re-scrape + re-project; serving is sparse until it catches up."
fi
