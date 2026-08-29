#!/usr/bin/env bash
#
# Keep a local mirror of prod's /debug data in sync, continuously, so the dev
# `/debug` pages read from a fast LAN Mongo instead of over the prod ssh
# tunnel — where the corpus scan takes 30–60s (intermittently stranding the
# table empty at findAll's 60s timeout) and every per-row read pays a ~110ms
# round-trip. Steps, per mirrored database:
#
#   1. ensure the local mirror Mongo is up (start-local-mongo.sh),
#   2. ensure prod (the source) is reachable via the tunnel,
#   3. seed the mirrored collections when the mirror is empty, has drifted too
#      far to catch up by resuming, or is missing one (or `--reseed` to force),
#   4. tail prod's database change stream → local, restarting on disconnect,
#   5. notice mirror-targets.js changing under it and restart the tailers, so
#      adding a collection takes effect on a RUNNING mirror rather than at the
#      next manual restart (see targets_fingerprint below).
#
# WHICH databases and collections are mirrored lives in mirror-targets.js —
# every country /debug can switch to (so `?country=uk` is LAN-fast too, not just
# the boot country), and the collections a /debug load reads. Each database
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
# Reading .env.local is a STARTUP step, not a load-time one: mirror-resilience-spec.sh
# sources this file for `supervise_db` alone and supplies its own endpoints, and it
# must not need — or touch — real credentials to do that.
load_endpoints() {
  SRC="$(envval MONGODB_URI)"
  DST="$(envval MONGODB_MOVIES_MIRROR_URI)"
  [ -n "$SRC" ] || { echo "[mirror] set MONGODB_URI in .env.local (prod tunnel = sync source)" >&2; exit 1; }
  [ -n "$DST" ] || { echo "[mirror] set MONGODB_MOVIES_MIRROR_URI in .env.local (local mirror = sync target)" >&2; exit 1; }

  # Tune the prod source connection: zlib wire compression so the seed cursor and
  # change stream carry ~6x less data over the tunnel (the uncompressed mongodump
  # cursor drops mid-transfer; this is the same path the app's findAll uses), and
  # a 10s server-selection timeout so a brief tunnel slowdown doesn't fail the
  # watch. Appended only when the URI doesn't already set them.
  SRCZ="$SRC"
  local join
  case "$SRCZ" in *\?*) join="&" ;; *) join="?" ;; esac
  case "$SRCZ" in *compressors=*) ;; *) SRCZ="$SRCZ${join}compressors=zlib"; join="&" ;; esac
  case "$SRCZ" in *serverSelectionTimeoutMS=*) ;; *) SRCZ="$SRCZ${join}serverSelectionTimeoutMS=10000" ;; esac

  # What prod-tunnel.sh health-checks the tunnel with. It has to be an
  # AUTHENTICATED ping, not a TCP probe: the port answering says nothing about
  # whether anything is behind it (see prod_tunnel_answers). Also tell it where
  # .env.local is, so the ssh target can be overridden there.
  TUNNEL_PROBE_URI="$SRC"
  PROD_TUNNEL_ENV_FILE="$ROOT/.env.local"
}

# ── Keep our own log from growing without bound ──────────────────────────────
# Under launchd this process's stdout IS $MIRROR_LOG, and nothing else rotates
# it — a tailer stuck in a restart loop grew it to 23MB. launchd holds an
# append-mode fd on the file, so trimming has to happen IN PLACE: renaming would
# leave the agent writing to the renamed inode and the "rotated" file empty.
. "$HERE/log-path.sh"
MAX_LOG_BYTES=$((8 * 1024 * 1024))
LOG_KEEP_LINES=2000
rotate_log() {
  [ -f "$MIRROR_LOG" ] || return 0
  local size; size="$(stat -f%z "$MIRROR_LOG" 2>/dev/null || echo 0)"
  [ "$size" -gt "$MAX_LOG_BYTES" ] || return 0
  local trimmed="$MIRROR_LOG.trimmed"
  tail -n "$LOG_KEEP_LINES" "$MIRROR_LOG" > "$trimmed" 2>/dev/null || return 0
  cat "$trimmed" > "$MIRROR_LOG"          # truncate + refill, keeping the inode
  rm -f "$trimmed"
  echo "[mirror] log passed $((MAX_LOG_BYTES / 1024 / 1024))MB — trimmed to its last $LOG_KEEP_LINES lines"
}

# ── Resilience helpers ───────────────────────────────────────────────────────
# Ensure the prod tunnel (sync source) is reachable, starting — and keeping
# alive — our OWN ssh forward whenever nothing already serves :27017 (an
# `sbt run` tunnel, or a manual one). We only touch a tunnel WE started, so we
# never fight one someone else owns; `cleanup` kills ours on exit. This is what
# makes the daemon survive a dropped/hung tunnel: the next cycle restarts it.
# How that tunnel is opened — and why it is ssh to mongo-1 rather than the
# `flyctl proxy` this used to run — lives in prod-tunnel.sh, shared with the
# other prod-sourced scripts so there is exactly one place to repoint.
TUNNEL_TAG="mirror"
. "$HERE/prod-tunnel.sh"
ensure_tunnel() { ensure_prod_tunnel; }
cleanup() { close_prod_tunnel; }
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

# Re-seed, treating a failure as a recoverable cycle rather than the end of this
# supervisor — 0 to carry on, non-zero for the caller to `continue` on.
#
# The seed was the ONE fallible step running bare under `set -e`, and a seed is
# exactly when the tunnel is under most load. When it dropped mid-copy
# (2026-08-02) mongosh exited non-zero and `set -e` killed all three
# supervisors outright; the parent then sat in `wait` forever, so launchd saw a
# live process and never restarted it, and the mirror stayed frozen — mid-seed,
# `movie_slots` dropped and never refilled — for two days while /debug served
# 934 films with zero cinemas apiece. Every other fallible step here already
# wraps itself this way; this one didn't.
reseed_or_wait() {
  local srcdb="$1" code
  set +e; reseed "$srcdb"; code=$?; set -e
  [ "$code" -eq 0 ] && return 0
  echo "[mirror] $srcdb: seed failed (exit $code) — retrying in 30s"
  sleep 30
  return 1
}

# ── Noticing that mirror-targets.js changed under a RUNNING mirror ────────────
# Every mongosh invocation below re-reads mirror-targets.js, so a new collection
# is picked up the next time one STARTS. The catch is that a healthy `tail.js`
# never ends: its change-stream `$match` pins the collection list it was launched
# with, so on a long-lived mirror an edit to MIRRORED_COLLECTIONS reached nothing
# until the whole agent was restarted by hand. Worse, the halfway state is a
# loop: `needs_reseed` (which DOES re-read) sees the new collection missing and
# re-seeds it, the stale tailer never tails it, prod moves on, and the next
# check re-seeds it again.
#
# So the parent republishes the digest of the current list once a minute, and
# each tailer compares it against the one it started with — see supervise_db.
# The digest covers the SET of names only, so editing a comment or reordering
# the literal doesn't churn every tailer (targets-fingerprint.js).
TARGETS_POLL="${KINOWO_MIRROR_TARGETS_POLL:-60}"
# Created at startup, not on `source` — sourcing this file must leave no temp
# file behind. `published_fingerprint` reads "unset" as "don't know", like every
# other unreadable digest.

# `--nodb`: this needs no server, so a down tunnel can't stop us noticing an
# edit. Empty output (a syntax error in the file, a missing mongosh) is treated
# by every reader as "don't know" and never as a change.
targets_fingerprint() {
  mongosh --nodb --quiet --file "$HERE/mirror-targets.js" --file "$HERE/targets-fingerprint.js" 2>/dev/null | tail -1
}

# What the children compare against. Falls back to the last good value rather
# than to empty, so one failed read can't look like an edit.
published_fingerprint() { cat "${TARGETS_FP_FILE:-}" 2>/dev/null; }

# Does this database's mirror need a full re-seed before it is worth tailing?
# staleness.js exits 3 when the mirror has drifted beyond what resuming a change
# stream can close (see staleness-rule.js for the thresholds and why). Any other
# exit — fresh, or the check itself failing on a blip — means tail, and the next
# cycle judges again; a broken check must never block the sync.
needs_reseed() {
  local code
  set +e
  mongosh "$SRCZ" --quiet --eval "var DST='$DST'; var SRC_DB='$1'" \
    --file "$HERE/mirror-targets.js" --file "$HERE/staleness-rule.js" --file "$HERE/staleness.js"
  code=$?
  set -e
  [ "$code" -eq 3 ]
}

# ── Resilient per-database sync loop ─────────────────────────────────────────
# Every cycle re-ensures the mirror Mongo + the tunnel, re-seeds when this
# database's mirror is empty or has drifted too far (needs_reseed), then tails.
# tail.js exits 2 when its resume token has aged out of prod's oplog → full
# re-seed; any other exit is a transient blip → resume from the saved token.
# Nothing here is fatal: a down tunnel, a stopped Mongo, or a wiped mirror all
# recover on the next cycle instead of killing the sync.
supervise_db() {
  local srcdb="$1" force="$2" code fails=0 backoff
  while true; do
    if ! ensure_local_mongo; then echo "[mirror] $srcdb: local Mongo unavailable — retrying in 5s"; sleep 5; continue; fi
    if ! ensure_tunnel;     then echo "[mirror] $srcdb: tunnel unavailable — retrying in 5s";     sleep 5; continue; fi

    if [ "$force" = "1" ] || needs_reseed "$srcdb"; then
      # `force` stays set on failure: a forced re-seed still owes one next cycle.
      reseed_or_wait "$srcdb" || continue
      force=0
    fi

    # The list this tailer is about to pin. Captured BEFORE launching it, so an
    # edit landing mid-launch is caught on the next poll rather than missed.
    local launched_with; launched_with="$(published_fingerprint)"
    local retargeted=0

    set +e
    mongosh "$SRCZ" --quiet --eval "var DST='$DST'; var SRC_DB='$srcdb'" \
      --file "$HERE/mirror-targets.js" --file "$HERE/tail.js" &
    local tailpid=$!
    # Watch for a retarget while the tailer streams. An empty reading means the
    # digest could not be computed — treated as "unchanged", never as an edit.
    while kill -0 "$tailpid" 2>/dev/null; do
      sleep "$TARGETS_POLL"
      local now; now="$(published_fingerprint)"
      if [ -n "$now" ] && [ -n "$launched_with" ] && [ "$now" != "$launched_with" ]; then
        echo "[mirror] $srcdb: mirrored-collection list changed — restarting tailer to pick it up"
        retargeted=1
        kill "$tailpid" 2>/dev/null || true
        break
      fi
    done
    wait "$tailpid"; code=$?
    set -e

    # A tailer WE stopped is not a failure: go straight round again, where
    # needs_reseed seeds whatever the new list added and tail.js re-reads it.
    # Resetting `fails` keeps a retarget from counting toward the backoff.
    if [ "$retargeted" = "1" ]; then fails=0; continue; fi

    if [ "$code" -eq 2 ]; then
      echo "[mirror] $srcdb: resume token expired — re-seeding…"
      reseed_or_wait "$srcdb" || continue
      fails=0; continue
    fi
    # A stream that ends immediately, over and over, is the shape that filled the
    # log with ~1000 identical lines: back off as the failures pile up, and after
    # the first few say it only every 20th time (with the streak, so a glance at
    # the log still shows how bad it is).
    fails=$((fails + 1))
    backoff=$(( fails < 5 ? 2 : (fails < 20 ? 15 : 60) ))
    if [ "$fails" -le 3 ] || [ $((fails % 20)) -eq 0 ]; then
      echo "[mirror] $srcdb: stream ended (exit $code, $fails in a row) — recovering in ${backoff}s…"
    fi
    sleep "$backoff"
  done
}

# ── Fan out: one supervised tailer per database ──────────────────────────────
# Everything below runs only when this file is EXECUTED. Sourced (by
# mirror-resilience-spec.sh, which drives `supervise_db` against stubs) it must
# define the functions and stop — no .env.local read, no tunnel, no children.
[ "${BASH_SOURCE[0]}" = "${0}" ] || return 0

# The tunnel has to be up before we can list databases, so ensure it first;
# after that each child re-ensures it independently. `cleanup` already kills our
# flyctl proxy on exit — extend it to take the children down with us, so Ctrl-C
# doesn't strand N background mongosh processes.
load_endpoints
TARGETS_FP_FILE="$(mktemp -t kinowo-mirror-targets)"
FORCE_RESEED="$([ "${1:-}" = "--reseed" ] && echo 1 || echo 0)"
until ensure_local_mongo && ensure_tunnel; do echo "[mirror] waiting for local Mongo + tunnel…"; sleep 5; done

DBS="$(discover_dbs)"
[ -n "$DBS" ] || { echo "[mirror] no kinowo* databases found via the tunnel — is MONGODB_URI right?" >&2; exit 1; }
echo "[mirror] mirroring: $DBS"

CHILDREN=()
cleanup_children() { for pid in "${CHILDREN[@]:-}"; do kill "$pid" 2>/dev/null || true; done; }
trap 'cleanup_children; rm -f "$TARGETS_FP_FILE"; cleanup' EXIT INT TERM

# Publish the current collection-list digest BEFORE the tailers start, so each
# one has something to compare against from its first poll.
targets_fingerprint > "$TARGETS_FP_FILE"
echo "[mirror] watching mirror-targets.js for collection changes (every ${TARGETS_POLL}s)"

for srcdb in $DBS; do
  supervise_db "$srcdb" "$FORCE_RESEED" &
  CHILDREN+=($!)
done

echo "[mirror] ${#CHILDREN[@]} tailer(s) running (Ctrl-C to stop)…"

# Trim from the PARENT only — N children truncating one file in place would race
# each other. Startup pass first, then every 5 minutes for a run that never ends.
# Counted after the line above so the rotator never reads as a tailer.
rotate_log
( while true; do sleep 300; rotate_log; done ) &
CHILDREN+=($!)

# Re-read mirror-targets.js from the PARENT only — N children each spawning a
# mongosh every poll would be N times the cost for one shared answer. A read that
# comes back empty (mid-edit file, mongosh hiccup) leaves the last good digest in
# place, so a half-written file can't be mistaken for an edit and bounce every
# tailer.
( while true; do
    sleep "$TARGETS_POLL"
    fp="$(targets_fingerprint)"
    [ -n "$fp" ] && printf '%s\n' "$fp" > "$TARGETS_FP_FILE"
  done ) &
CHILDREN+=($!)
wait
