#!/usr/bin/env bash
#
# Full corpus reset. Stops the serving + scraping apps, wipes the scrape /
# enrichment / read-model collections from prod Mongo, then brings both apps
# back. After this the worker re-scrapes every cinema from scratch and
# re-projects the read model; the web app serves empty until that completes
# (a few minutes of sparse repertoire — acceptable per the "short downtime is
# fine" rule).
#
# Stops:    kinowo (web), kinowo-worker (worker)  — machines stopped, not destroyed
# Drops:    detailCache, freshness, movies, tasks, web_movies, web_screenings
# Restarts: kinowo, kinowo-worker
#
# Apps are stopped FIRST so the worker can't re-create a collection (or race a
# half-finished scrape into it) while we're dropping; restarted only after the
# wipe lands.
#
# Reads MONGODB_URI (prod, via the flyctl tunnel) + MONGODB_DB from .env.local.
# The URI points at 127.0.0.1:27017, so this script opens the
# `flyctl proxy ... --app kinowo-mongo` tunnel itself and tears it down on exit.
#
# Usage:  scripts/reset-corpus.sh           # prompts before the drop
#         scripts/reset-corpus.sh --yes      # skip the confirmation prompt
set -euo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(cd "$HERE/.." && pwd)"

WEB_APP="kinowo"
WORKER_APP="kinowo-worker"
MONGO_APP="kinowo-mongo"
COLLECTIONS=(detailCache freshness movies tasks web_movies web_screenings)

ASSUME_YES=""
[ "${1:-}" = "--yes" ] && ASSUME_YES=1

# Read KEY=VALUE from .env.local WITHOUT sourcing it — the Mongo URI contains
# `&`/`?`, which a shell `source` would mangle (same approach as mirror.sh).
envval() {
  { grep -E "^$1=" "$ROOT/.env.local" 2>/dev/null || true; } | head -1 | cut -d= -f2- \
    | sed -e 's/^["'"'"']//' -e 's/["'"'"']$//'
}
URI="$(envval MONGODB_URI)"
DB="$(envval MONGODB_DB)"
[ -n "$URI" ] || { echo "[reset] set MONGODB_URI in .env.local (prod tunnel)" >&2; exit 1; }
[ -n "$DB" ]  || { echo "[reset] set MONGODB_DB in .env.local" >&2; exit 1; }

echo "[reset] target db '$DB' — will drop: ${COLLECTIONS[*]}"
echo "[reset] will stop+restart: $WEB_APP, $WORKER_APP"
if [ -z "$ASSUME_YES" ]; then
  read -r -p "[reset] This wipes the prod corpus. Type 'wipe' to proceed: " ans
  [ "$ans" = "wipe" ] || { echo "[reset] aborted."; exit 1; }
fi

# --- stop all machines for an app -------------------------------------------
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

# --- tunnel to prod Mongo, torn down on any exit ----------------------------
PROXY_PID=""
cleanup() {
  [ -n "$PROXY_PID" ] && kill "$PROXY_PID" 2>/dev/null || true
}
trap cleanup EXIT

open_tunnel() {
  echo "[reset] opening Mongo tunnel ($MONGO_APP)..."
  flyctl proxy 27017:27017 --app "$MONGO_APP" >/dev/null 2>&1 &
  PROXY_PID=$!
  # wait up to ~30s for the tunnel to answer a ping
  for _ in $(seq 1 30); do
    if mongosh "$URI" --quiet --eval 'db.runCommand({ping:1})' >/dev/null 2>&1; then
      return 0
    fi
    sleep 1
  done
  echo "[reset] prod Mongo not reachable via the tunnel" >&2
  exit 1
}

# --- run ---------------------------------------------------------------------
stop_app "$WEB_APP"
stop_app "$WORKER_APP"

open_tunnel

echo "[reset] dropping collections..."
COLLS_JS="$(printf '"%s",' "${COLLECTIONS[@]}")"
mongosh "$URI" --quiet --eval "
  var target = [${COLLS_JS%,}];
  var existing = db.getCollectionNames();
  target.forEach(function (c) {
    if (existing.indexOf(c) === -1) { print('  skip  ' + c + ' (absent)'); return; }
    db.getCollection(c).drop();
    print('  dropped ' + c);
  });
"

start_app "$WEB_APP"
start_app "$WORKER_APP"

echo "[reset] done. Worker will re-scrape + re-project; web serves sparse until it catches up."
