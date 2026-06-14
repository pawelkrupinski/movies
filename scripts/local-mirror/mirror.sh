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

# ── Resilience helpers ───────────────────────────────────────────────────────
# Ensure the prod tunnel (sync source) is reachable, starting — and keeping
# alive — our OWN `flyctl proxy` whenever nothing already serves :27017 (an
# `sbt run` proxy, or a manual one). We only touch a proxy WE started, so we
# never fight a tunnel someone else owns; `cleanup` kills ours on exit. This is
# what makes the daemon survive a dropped/hung tunnel: the next cycle restarts it.
PROXY_PID=""
ensure_tunnel() {
  nc -z -w2 127.0.0.1 27017 2>/dev/null && return 0      # already served (ours or theirs)
  if [ -n "$PROXY_PID" ]; then                            # ours died or hung — replace it
    echo "[mirror] tunnel down — restarting flyctl proxy"
    kill "$PROXY_PID" 2>/dev/null || true; PROXY_PID=""
  else
    echo "[mirror] no tunnel on :27017 — starting flyctl proxy"
  fi
  flyctl proxy 27017:27017 --app kinowo-mongo >/dev/null 2>&1 &
  PROXY_PID=$!
  for _ in $(seq 1 30); do nc -z -w2 127.0.0.1 27017 2>/dev/null && return 0; sleep 1; done
  return 1
}
cleanup() { [ -n "$PROXY_PID" ] && kill "$PROXY_PID" 2>/dev/null || true; }
trap cleanup EXIT INT TERM

# Ensure the local mirror Mongo (native, brew-managed) is reachable on :28017,
# re-running the idempotent starter only when it isn't — so a stopped service
# self-heals. Returns non-zero (instead of crashing) when the starter can't bring
# it up, so the loop just retries — e.g. at login before `brew services` has it.
ensure_local_mongo() {
  nc -z -w2 127.0.0.1 28017 2>/dev/null && return 0
  echo "[mirror] local Mongo not reachable on :28017 — (re)starting"
  "$HERE/start-local-mongo.sh" || return 1
}

reseed() { mongosh "$SRCZ" --quiet --eval "var DST='$DST'" --file "$HERE/seed.js"; }

# ── Resilient sync loop ──────────────────────────────────────────────────────
# Every cycle re-ensures the mirror Mongo + the tunnel, (re)seeds when the local
# `movies` is empty (a fresh container or a wiped volume), then tails. tail.js
# exits 2 when its resume token has aged out of prod's oplog → full re-seed; any
# other exit is a transient blip → resume from the saved token. Nothing here is
# fatal: a down tunnel, a stopped container, or a wiped mirror all recover on the
# next cycle instead of killing the sync.
FORCE_RESEED="$([ "$RESEED" = "--reseed" ] && echo 1 || echo 0)"
while true; do
  if ! ensure_local_mongo; then echo "[mirror] local Mongo unavailable (Docker down?) — retrying in 5s"; sleep 5; continue; fi
  if ! ensure_tunnel; then echo "[mirror] tunnel unavailable — retrying in 5s"; sleep 5; continue; fi

  LOCAL_N="$(mongosh "$DST" --quiet --eval 'print(db.getCollection("movies").countDocuments())' 2>/dev/null | tail -1)" || LOCAL_N=""
  if [ "$FORCE_RESEED" = "1" ] || [ "${LOCAL_N:-0}" = "0" ]; then reseed; FORCE_RESEED=0; fi

  echo "[mirror] tailing prod→local change stream (Ctrl-C to stop)…"
  set +e; mongosh "$SRCZ" --quiet --eval "var DST='$DST'" --file "$HERE/tail.js"; code=$?; set -e
  if [ "$code" -eq 2 ]; then echo "[mirror] resume token expired — re-seeding…"; reseed
  else echo "[mirror] stream ended (exit $code) — recovering in 2s…"; fi
  sleep 2
done
