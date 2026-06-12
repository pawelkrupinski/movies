#!/usr/bin/env bash
#
# Keep a local `movies` mirror in sync with prod, continuously, so the dev
# `/debug` page reads the full corpus from a fast LAN Mongo instead of over the
# 30–60s prod `flyctl` tunnel (where findAll's 60s timeout intermittently
# strands the table empty). Steps:
#
#   1. ensure the local mirror Mongo is up (start-local-mongo.sh),
#   2. ensure prod (the source) is reachable via the tunnel,
#   3. seed the local `movies` once if empty (or `--reseed` to force),
#   4. tail prod's `movies` change stream → local, restarting on disconnect.
#
# Reads MONGODB_URI (prod tunnel = source) and MONGODB_MOVIES_MIRROR_URI
# (local = target) from .env.local. Long-running — leave it in its own
# terminal; Ctrl-C to stop. The web app reads the mirror only when
# MONGODB_MOVIES_MIRROR_URI is set (see web modules.Wiring); unset → /debug
# reads prod directly, exactly as before.
#
# Usage:  scripts/local-mirror/mirror.sh [--reseed]
set -euo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(cd "$HERE/../.." && pwd)"

# Read KEY=VALUE from .env.local WITHOUT sourcing it — the Mongo URIs contain
# `&`/`?`, which a shell `source` would treat as backgrounding / globbing and
# mangle (matching how the app's tools.Env parses the file line-by-line).
envval() {
  # `|| true` so a missing key (grep exit 1 under pipefail) yields empty output
  # instead of aborting before the friendly check below.
  { grep -E "^$1=" "$ROOT/.env.local" 2>/dev/null || true; } | head -1 | cut -d= -f2- \
    | sed -e 's/^["'"'"']//' -e 's/["'"'"']$//'
}
SRC="$(envval MONGODB_URI)"
DST="$(envval MONGODB_MOVIES_MIRROR_URI)"
[ -n "$SRC" ] || { echo "[mirror] set MONGODB_URI in .env.local (prod tunnel = sync source)" >&2; exit 1; }
[ -n "$DST" ] || { echo "[mirror] set MONGODB_MOVIES_MIRROR_URI in .env.local (local mirror = sync target)" >&2; exit 1; }
RESEED="${1:-}"

# Tune the prod source connection: zlib wire compression so the seed cursor and
# change stream carry ~6x less data over the tunnel (the uncompressed mongodump
# cursor drops mid-transfer; this is the same path the app's findAll uses), and
# a 10s server-selection timeout so a brief tunnel slowdown doesn't fail the
# watch. Appended only when the URI doesn't already set them.
SRCZ="$SRC"
case "$SRCZ" in *\?*) JOIN="&" ;; *) JOIN="?" ;; esac
case "$SRCZ" in *compressors=*) ;; *) SRCZ="$SRCZ${JOIN}compressors=zlib"; JOIN="&" ;; esac
case "$SRCZ" in *serverSelectionTimeoutMS=*) ;; *) SRCZ="$SRCZ${JOIN}serverSelectionTimeoutMS=10000" ;; esac

# 1. local mirror up
"$HERE/start-local-mongo.sh"

# 2. prod reachable
if ! mongosh "$SRCZ" --quiet --eval 'db.runCommand({ping:1})' >/dev/null 2>&1; then
  echo "[mirror] prod not reachable via MONGODB_URI — is the tunnel up?" >&2
  echo "         flyctl proxy 27017:27017 --app kinowo-mongo" >&2
  exit 1
fi

reseed() { mongosh "$SRCZ" --quiet --eval "var DST='$DST'" --file "$HERE/seed.js"; }

# 3. seed if empty or forced
LOCAL_N="$(mongosh "$DST" --quiet --eval 'print(db.getCollection("movies").countDocuments())' 2>/dev/null | tail -1)"
if [ "$RESEED" = "--reseed" ] || [ "${LOCAL_N:-0}" = "0" ]; then reseed; fi

# 4. tail, restart on disconnect. tail.js exits 2 when its saved resume token
#    has aged out of prod's oplog (resume impossible) → full re-seed; any other
#    exit is a transient blip → just re-run, resuming from the saved token (no
#    re-seed, so near-real-time survives tunnel hiccups).
while true; do
  echo "[mirror] tailing prod→local change stream (Ctrl-C to stop)…"
  set +e; mongosh "$SRCZ" --quiet --eval "var DST='$DST'" --file "$HERE/tail.js"; code=$?; set -e
  if [ "$code" -eq 2 ]; then
    echo "[mirror] resume token expired — re-seeding…"; reseed
  else
    echo "[mirror] stream ended (exit $code) — resuming in 2s…"
  fi
  sleep 2
done
