#!/usr/bin/env bash
#
# Keep a local mirror of prod's /debug data in sync, continuously, so the dev
# `/debug` pages read from a fast LAN Mongo instead of over the prod `flyctl`
# tunnel — where the corpus scan takes 30–60s (intermittently stranding the
# table empty at findAll's 60s timeout) and every per-row read pays a ~110ms
# round-trip. Steps, per mirrored database:
#
#   1. ensure the local mirror Mongo is up (start-local-mongo.sh),
#   2. ensure prod (the source) is reachable via the tunnel,
#   3. seed the mirrored collections once if empty (or `--reseed` to force),
#   4. tail prod's database change stream → local, restarting on disconnect.
#
# WHICH databases and collections are mirrored lives in mirror-targets.js —
# every country /debug can switch to (so `?country=uk` is LAN-fast too, not just
# the boot country), and the four collections a /debug load reads. Each database
# gets its OWN tailer process: a change stream is per-database here (a
# deployment-wide one is Unauthorized for these credentials) and mongosh is
# single-threaded, so N databases means N supervised children, not one loop.
#
# Reads MONGODB_URI (prod tunnel = source), MONGODB_MOVIES_MIRROR_URI (local =
# target) and the optional KINOWO_MIRROR_DBS (space/comma-separated prod
# databases; default: every kinowo* database the tunnel exposes) from
# .env.local. Long-running — leave it in its own terminal; Ctrl-C to stop. The
# web app reads the mirror only when MONGODB_MOVIES_MIRROR_URI is set (see web
# modules.Wiring); unset → /debug reads prod directly, exactly as before.
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

# Which prod databases to mirror. Explicit KINOWO_MIRROR_DBS wins (space- or
# comma-separated); otherwise discover every `kinowo*` database the tunnel
# exposes, so onboarding a country needs no edit here. The mirror databases
# themselves (`*_prod_mirror`) live on the LOCAL instance, never on prod, so
# there is nothing to exclude from this listing.
discover_dbs() {
  local configured; configured="$(envval KINOWO_MIRROR_DBS | tr ',' ' ')"
  if [ -n "$configured" ]; then echo "$configured"; return 0; fi
  mongosh "$SRCZ" --quiet --eval \
    'db.adminCommand({listDatabases:1,nameOnly:true}).databases.map(d=>d.name).filter(n=>/^kinowo/.test(n)).join(" ")' \
    2>/dev/null | tail -1
}

reseed()   { mongosh "$SRCZ" --quiet --eval "var DST='$DST'; var SRC_DB='$1'" --file "$HERE/mirror-targets.js" --file "$HERE/seed.js"; }
mirror_db() { echo "${1}_prod_mirror"; }   # must match mirror-targets.js `mirrorDbFor`

# ── Resilient per-database sync loop ─────────────────────────────────────────
# Every cycle re-ensures the mirror Mongo + the tunnel, (re)seeds when this
# database's `movies` is empty (a fresh instance or a wiped volume), then tails.
# tail.js exits 2 when its resume token has aged out of prod's oplog → full
# re-seed; any other exit is a transient blip → resume from the saved token.
# Nothing here is fatal: a down tunnel, a stopped Mongo, or a wiped mirror all
# recover on the next cycle instead of killing the sync.
supervise_db() {
  local srcdb="$1" force="$2" code local_n
  while true; do
    if ! ensure_local_mongo; then echo "[mirror] $srcdb: local Mongo unavailable — retrying in 5s"; sleep 5; continue; fi
    if ! ensure_tunnel;     then echo "[mirror] $srcdb: tunnel unavailable — retrying in 5s";     sleep 5; continue; fi

    local_n="$(mongosh "$DST" --quiet --eval \
      "print(db.getSiblingDB('$(mirror_db "$srcdb")').getCollection('movies').countDocuments())" 2>/dev/null | tail -1)" || local_n=""
    if [ "$force" = "1" ] || [ "${local_n:-0}" = "0" ]; then reseed "$srcdb"; force=0; fi

    set +e; mongosh "$SRCZ" --quiet --eval "var DST='$DST'; var SRC_DB='$srcdb'" \
      --file "$HERE/mirror-targets.js" --file "$HERE/tail.js"; code=$?; set -e
    if [ "$code" -eq 2 ]; then echo "[mirror] $srcdb: resume token expired — re-seeding…"; reseed "$srcdb"
    else echo "[mirror] $srcdb: stream ended (exit $code) — recovering in 2s…"; fi
    sleep 2
  done
}

# ── Fan out: one supervised tailer per database ──────────────────────────────
# The tunnel has to be up before we can list databases, so ensure it first;
# after that each child re-ensures it independently. `cleanup` already kills our
# flyctl proxy on exit — extend it to take the children down with us, so Ctrl-C
# doesn't strand N background mongosh processes.
FORCE_RESEED="$([ "$RESEED" = "--reseed" ] && echo 1 || echo 0)"
until ensure_local_mongo && ensure_tunnel; do echo "[mirror] waiting for local Mongo + tunnel…"; sleep 5; done

DBS="$(discover_dbs)"
[ -n "$DBS" ] || { echo "[mirror] no kinowo* databases found via the tunnel — is MONGODB_URI right?" >&2; exit 1; }
echo "[mirror] mirroring: $DBS"

CHILDREN=()
cleanup_children() { for pid in "${CHILDREN[@]:-}"; do kill "$pid" 2>/dev/null || true; done; }
trap 'cleanup_children; cleanup' EXIT INT TERM

for srcdb in $DBS; do
  supervise_db "$srcdb" "$FORCE_RESEED" &
  CHILDREN+=($!)
done
echo "[mirror] ${#CHILDREN[@]} tailer(s) running (Ctrl-C to stop)…"
wait
