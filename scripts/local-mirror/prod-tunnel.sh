#!/usr/bin/env bash
#
# The one place that knows HOW a laptop reaches prod Mongo. Sourced by every
# local script whose source is prod: mirror.sh (the /debug mirror daemon),
# sync-title-rules.sh, sync-enrichment-cache.sh, ../reset-corpus.sh.
#
# WHY THIS FILE EXISTS AT ALL. Those four had four copies of the same
# "nc -z :27017 || flyctl proxy" block. When prod moved off the Fly app
# `kinowo-mongo` onto the Hetzner host mongo-1 (2026-08-29) each copy had to be
# found and repointed independently, and the one that got missed would keep
# dialling a machine that is now STOPPED — failing in a way that looks like a
# network blip rather than a wrong destination. One definition, one repoint.
#
# WHY AN SSH PORT-FORWARD AND NOT A REMOTE DUMP. Everything downstream speaks
# the Mongo wire protocol to 127.0.0.1:27017 — seed.js, tail.js, staleness.js,
# mongodump, mongorestore, and the app's own MONGODB_URI in .env.local. A local
# forward is a drop-in for what `flyctl proxy` published on the same port, so
# NOTHING else changed: no URI rewrites, no credentials moved, no new files.
# Dumping on mongo-1 and streaming the result back cannot work here anyway — the
# mirror's steady state is tail.js holding a CHANGE STREAM open for days, which
# is a live cursor, not a dump.
#
# WHY 127.0.0.1 ON THE FAR SIDE. mongod on mongo-1 listens on three addresses:
# 127.0.0.1, 10.20.0.10 (the private Hetzner subnet, which a laptop cannot
# route to) and a Fly 6PN WireGuard address (which is how the deployed apps
# reach it). Only the loopback one is reachable from inside an ssh session, so
# that is the far end of the forward.
#
# Configuration, in precedence order:
#   $KINOWO_MONGO_SSH      environment override (a one-off, or a rescue host)
#   KINOWO_MONGO_SSH=      in .env.local
#   root@178.105.221.61       mongo-1's public address, as pinned in
#                          infra/nix/hosts/mongo-1/default.nix
#
# Contract for callers:
#   * set $TUNNEL_TAG   — the log prefix, e.g. "mirror"
#   * set $TUNNEL_PROBE_URI — a Mongo URI (with credentials) to health-check
#                         with; without it the check degrades to a bare TCP
#                         probe, which is exactly the weak check described below
#   * set $PROD_TUNNEL_ENV_FILE — path to .env.local, if the ssh target may be
#                         configured there
#   * call `close_prod_tunnel` from your own EXIT/INT/TERM trap
#
# Sourcing this file must stay side-effect free: mirror.sh sources it at load
# time, and mirror-resilience-spec.sh asserts that sourcing mirror.sh reads no
# .env.local and starts nothing. So everything below is a function definition.

# Read one key out of an env file, the way the app's own tools.Env does.
#
# Line-by-line, NOT `source`: .env.local's Mongo URIs carry `&` and `?`, which a
# shell source would treat as backgrounding and globbing. `|| true` so a missing
# key (grep exit 1 under pipefail) yields empty output instead of aborting the
# caller before it can print a friendly message.
#
# This lives here because all four prod-sourced scripts — mirror.sh,
# sync-title-rules.sh, sync-enrichment-cache.sh, ../reset-corpus.sh — already
# source this file, and every one of them had defined a byte-identical copy of
# it. The file below used a seventh inline copy whose comment said it "matches
# envval in each caller"; a comment promising that two blocks stay identical is
# the cheapest possible substitute for making them one.
#
#   envval KEY [file]   — file defaults to $PROD_TUNNEL_ENV_FILE, then $ROOT/.env.local
envval() {
  local file="${2:-${PROD_TUNNEL_ENV_FILE:-${ROOT:-.}/.env.local}}"
  { grep -E "^$1=" "$file" 2>/dev/null || true; } | head -1 | cut -d= -f2- \
    | sed -e 's/^["'"'"']//' -e 's/["'"'"']$//'
}

# Declare what this caller is and what it talks to, in one line instead of the
# three-assignment preamble every caller copied. The env file defaults to
# $ROOT/.env.local, which is what all three passed anyway.
#
#   init_prod_tunnel <tag> <probe-uri> [env-file]
#
# Callers still install their own EXIT trap: what has to be torn down differs
# (the sync scripts also remove a mktemp dir, reset-corpus does not), and a
# shared trap that guesses at that would be worse than three explicit ones.
init_prod_tunnel() {
  TUNNEL_TAG="$1"
  TUNNEL_PROBE_URI="$2"
  PROD_TUNNEL_ENV_FILE="${3:-${ROOT:-.}/.env.local}"
}

# The pid of a tunnel WE started. Empty means the port is served by someone
# else's process (an `sbt run` forward, a manual one) and is not ours to kill.
PROD_TUNNEL_PID=""

# Resolved lazily, never at source time — see the side-effect note above.
prod_tunnel_target() {
  if [ -n "${KINOWO_MONGO_SSH:-}" ]; then printf '%s\n' "$KINOWO_MONGO_SSH"; return 0; fi
  local from_env=""
  if [ -n "${PROD_TUNNEL_ENV_FILE:-}" ] && [ -f "$PROD_TUNNEL_ENV_FILE" ]; then
    from_env="$(envval KINOWO_MONGO_SSH "$PROD_TUNNEL_ENV_FILE")"
  fi
  printf '%s\n' "${from_env:-root@178.105.221.61}"
}

# Does prod actually ANSWER on :27017, or is the port merely accepting?
#
# The old check was `nc -z 127.0.0.1 27017`, and that is precisely how the
# cutover broke the mirror: a `flyctl proxy` left over from before the move went
# on accepting connections locally while the machine behind it was stopped, so
# every cycle read "tunnel healthy" and then every change stream died with
# ECONNRESET — 420 consecutive failures in the log with nothing calling it a
# tunnel problem. A liveness check that a dead backend passes is worse than
# none, because it routes the failure somewhere it cannot be diagnosed.
#
# The probe URI's own serverSelectionTimeoutMS is replaced with a short one: the
# sync URIs set 10s so a loaded tunnel doesn't fail a real query, but that is a
# 10s stall per cycle when we are only asking "is anything there".
prod_tunnel_answers() {
  nc -z -w2 127.0.0.1 27017 2>/dev/null || return 1
  [ -n "${TUNNEL_PROBE_URI:-}" ] || return 0        # nothing to authenticate with; TCP is all we have
  local probe; probe="$(printf '%s' "$TUNNEL_PROBE_URI" | sed -E 's/[?&]serverSelectionTimeoutMS=[0-9]+//g')"
  case "$probe" in *\?*) probe="$probe&serverSelectionTimeoutMS=3000" ;;
                   *)    probe="$probe?serverSelectionTimeoutMS=3000" ;; esac
  mongosh "$probe" --quiet --eval 'db.adminCommand({ping:1})' >/dev/null 2>&1
}

# Bring the tunnel up if prod isn't answering, and keep it OURS. Returns 0 when
# prod answers on 127.0.0.1:27017, non-zero when it doesn't — never exits, so a
# long-running caller (mirror.sh) can just retry on the next cycle.
ensure_prod_tunnel() {
  local tag="${TUNNEL_TAG:-tunnel}"
  prod_tunnel_answers && return 0

  if [ -n "$PROD_TUNNEL_PID" ]; then
    echo "[$tag] tunnel to prod Mongo is down — restarting it"
    kill "$PROD_TUNNEL_PID" 2>/dev/null || true
    wait "$PROD_TUNNEL_PID" 2>/dev/null || true
    PROD_TUNNEL_PID=""
  elif nc -z -w2 127.0.0.1 27017 2>/dev/null; then
    # Someone else's process holds the port and prod does not answer through it.
    # We must not kill it — it may be an `sbt run` forward someone is using — but
    # we also cannot bind over it, so say exactly what to look for instead of
    # retrying into a wall. The overwhelmingly likely cause after the cutover is
    # a `flyctl proxy ... --app kinowo-mongo` left running from before the move.
    echo "[$tag] :27017 is held by another process but prod does NOT answer through it." >&2
    echo "[$tag] A stale 'flyctl proxy --app kinowo-mongo' is the usual cause — that app is" >&2
    echo "[$tag] stopped and its tunnel accepts connections that go nowhere. Kill it with:" >&2
    echo "[$tag]   pkill -f 'flyctl proxy 27017'" >&2
    return 1
  fi

  local target; target="$(prod_tunnel_target)"
  echo "[$tag] opening ssh tunnel to prod Mongo ($target) on :27017"
  # ExitOnForwardFailure: without it ssh happily holds a session open with NO
  # forward when the local bind fails, which reads as a live tunnel and fails
  # later as a connection error. BatchMode: under launchd there is no one to
  # answer a passphrase prompt, so hang-forever must become fail-now.
  # ServerAlive*: a laptop that sleeps or changes network leaves a half-open TCP
  # session that neither errors nor carries data — the ~45s probe window turns
  # that into a clean exit the supervisor can restart.
  ssh -N -L 27017:127.0.0.1:27017 \
      -o ExitOnForwardFailure=yes -o BatchMode=yes -o ConnectTimeout=10 \
      -o ServerAliveInterval=15 -o ServerAliveCountMax=3 \
      "$target" >/dev/null 2>&1 &
  PROD_TUNNEL_PID=$!

  local _
  for _ in $(seq 1 30); do
    prod_tunnel_answers && return 0
    # A tunnel that died on its own (bad key, host down) will never answer —
    # stop waiting out the full 30s for a process that is already gone.
    kill -0 "$PROD_TUNNEL_PID" 2>/dev/null || { PROD_TUNNEL_PID=""; break; }
    sleep 1
  done
  echo "[$tag] prod Mongo did not answer via $target within 30s" >&2
  return 1
}

# Tear down ONLY a tunnel we started, so we never strand an ssh after exit and
# never kill one that was not ours. Safe to call more than once.
close_prod_tunnel() {
  [ -n "$PROD_TUNNEL_PID" ] || return 0
  kill "$PROD_TUNNEL_PID" 2>/dev/null || true
  PROD_TUNNEL_PID=""
}
