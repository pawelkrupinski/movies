#!/usr/bin/env bash
#
# The one place that knows HOW a laptop reaches prod Mongo. Sourced by every
# local script whose source is prod: mirror.sh (the /debug mirror daemon),
# sync-enrichment-cache.sh, ../reset-corpus.sh.
#
# WHY THIS FILE EXISTS AT ALL. Those scripts had their own copies of the same
# "nc -z :27017 || flyctl proxy" block. When prod moved off the Fly app
# `kinowo-mongo` onto the Hetzner host mongo-1 (2026-08-29) each copy had to be
# found and repointed independently, and the one that got missed would keep
# dialling a machine that is now STOPPED — failing in a way that looks like a
# network blip rather than a wrong destination. One definition, one repoint.
#
# WHY AN SSH PORT-FORWARD AND NOT A REMOTE DUMP. Everything downstream speaks
# the Mongo wire protocol to 127.0.0.1:27017 — seed.js, tail.js, staleness.js,
# mongodump, mongorestore, and the app's own MONGODB_URI (from the vault). A local
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
# Configuration — not a secret, so the environment or a committed default, never a file:
#   $KINOWO_MONGO_SSH      environment override (a one-off, or a rescue host)
#   root@178.105.221.61    mongo-1's public address, as pinned in
#                          infra/nix/hosts/mongo-1/default.nix
#
# WHY A PORT OF OUR OWN. The forward used to bind a fixed :27017, so two scripts at once shared it, and the one
# that finished first closed it under the other mid-command. Each caller now opens its OWN forward on a free local
# port ($PROD_TUNNEL_PORT: set it to pin one, else prod_tunnel_free_port picks, starting from a pid-derived slot in
# 27100-27899 so concurrent callers rarely probe the same one). prod_tunnel_uri puts that port into the URI a caller
# hands its command, so the URI and the forward cannot disagree; ensure_prod_tunnel exports PROD_TUNNEL_PORT.
#
# Contract for callers:
#   * set $TUNNEL_TAG   — the log prefix, e.g. "mirror"
#   * set $TUNNEL_PROBE_URI — a Mongo URI (with credentials) to health-check
#                         with; without it the check degrades to a bare TCP
#                         probe, which is exactly the weak check described below
#   * call `close_prod_tunnel` from your own EXIT/INT/TERM trap
#
# Sourcing this file must stay side-effect free: mirror.sh sources it at load
# time, and mirror-resilience-test.sh asserts that sourcing mirror.sh asks the
# vault for nothing and starts nothing. So everything below is a function
# definition.
#
# It also brings ensure_vault_env (../vault-env.sh) with it: every caller of this
# file needs a prod Mongo URI, and that is a secret — the environment's, else the
# Touch ID vault's. There is no .env.local to read it from any more (2026-10-09).
# shellcheck source=scripts/vault-env.sh
. "$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)/vault-env.sh"

# Declare what this caller is and what it talks to, in one line instead of the
# two-assignment preamble every caller copied.
#
#   init_prod_tunnel <tag> <probe-uri>
#
# Callers still install their own EXIT trap: what has to be torn down differs
# (the sync scripts also remove a mktemp dir, reset-corpus does not), and a
# shared trap that guesses at that would be worse than three explicit ones.
init_prod_tunnel() {
  TUNNEL_TAG="$1"
  TUNNEL_PROBE_URI="$2"
}

# The laptop's view of a prod Mongo URI.
#
# The vault's MONGODB_URI / MONGO_ROOT_URI are what the CLUSTER uses: they name
# the in-cluster address (10.20.0.13:27017), which a laptop cannot route to. The
# old .env.local carried a hand-edited copy pointed at the tunnel instead; rather
# than keep a second vault key in step with the first, every laptop-side script
# derives the tunnel form from the one real value, here:
#   * the host list after the credentials becomes 127.0.0.1:<tunnel port> (prod_tunnel_free_port),
#   * the query carries directConnection=true — a replica-set member reached
#     through a forward must not be "discovered" past it to addresses the laptop
#     cannot reach — replacing any directConnection already there,
#   * credentials, database path and every other query parameter are kept.
# Idempotent, so an exported URI that is already in tunnel form passes through.
#
#   prod_tunnel_uri <uri>   — prints the rewritten URI
prod_tunnel_uri() {
  local rest="${1#*://}" creds="" tail query
  case "$rest" in *@*) creds="${rest%%@*}@"; rest="${rest#*@}" ;; esac
  tail="${rest#"${rest%%[/?]*}"}"                 # drop the host list: what's left is /path?query
  case "$tail" in /*) ;; *) tail="/$tail" ;; esac
  case "$tail" in
    *\?*) query="${tail#*\?}"; tail="${tail%%\?*}"
          query="$(printf '%s' "$query" | tr '&' '\n' | { grep -v '^directConnection=' || true; } | paste -sd'&' -)" ;;
    *)    query="" ;;
  esac
  printf 'mongodb://%s127.0.0.1:%s%s?%sdirectConnection=true\n' "$creds" "$(prod_tunnel_free_port || echo 27017)" "$tail" "${query:+$query&}"
}

# The local port this caller's tunnel binds: $PROD_TUNNEL_PORT when set, else the first port from a pid-derived start
# (27100-27899) that nothing accepts on. $$ is the caller's pid even inside $(...), so the URI built before the tunnel
# opens and the tunnel opened later pick the same port. Fails (nothing printed) when 200 in a row are taken.
prod_tunnel_free_port() {
  if [ -n "${PROD_TUNNEL_PORT:-}" ]; then printf '%s\n' "$PROD_TUNNEL_PORT"; return 0; fi
  local port=$((27100 + $$ % 800))
  for _ in $(seq 1 200); do
    nc -z -w1 127.0.0.1 "$port" 2>/dev/null || { printf '%s\n' "$port"; return 0; }
    port=$((port + 1)); [ "$port" -lt 27900 ] || port=27100
  done
  return 1
}

# The port of THIS caller's tunnel: the pinned one, else the one its probe URI already names (so the forward is the
# one the URI the caller hands its command points at), else a free one.
prod_tunnel_port() {
  local from_uri
  if [ -n "${PROD_TUNNEL_PORT:-}" ]; then printf '%s\n' "$PROD_TUNNEL_PORT"; return 0; fi
  from_uri="$(printf '%s' "${TUNNEL_PROBE_URI:-}" | sed -nE 's#^mongodb://([^@/]*@)?127\.0\.0\.1:([0-9]+).*#\2#p')"
  if [ -n "$from_uri" ]; then printf '%s\n' "$from_uri"; return 0; fi
  prod_tunnel_free_port
}

# The pid of a tunnel WE started. Empty means the port is served by someone
# else's process (an `sbt run` forward, a manual one) and is not ours to kill.
PROD_TUNNEL_PID=""

# Resolved lazily, never at source time — see the side-effect note above.
prod_tunnel_target() {
  printf '%s\n' "${KINOWO_MONGO_SSH:-root@178.105.221.61}"
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
# `mongosh URI ARGS…` with the URI kept off mongosh's command line — see mongosh-at.sh. To run it in
# the BACKGROUND, call "$MONGOSH_AT" itself: a backgrounded function's $! is a subshell, not mongosh.
MONGOSH_AT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/mongosh-at.sh"
mongosh_at() { "$MONGOSH_AT" "$@"; }

prod_tunnel_answers() {
  nc -z -w2 127.0.0.1 "$(prod_tunnel_port || echo 27017)" 2>/dev/null || return 1
  [ -n "${TUNNEL_PROBE_URI:-}" ] || return 0        # nothing to authenticate with; TCP is all we have
  local probe; probe="$(printf '%s' "$TUNNEL_PROBE_URI" | sed -E 's/[?&]serverSelectionTimeoutMS=[0-9]+//g')"
  case "$probe" in *\?*) probe="$probe&serverSelectionTimeoutMS=3000" ;;
                   *)    probe="$probe?serverSelectionTimeoutMS=3000" ;; esac
  mongosh_at "$probe" --quiet --eval 'db.adminCommand({ping:1})' >/dev/null 2>&1
}

# Bring the tunnel up if prod isn't answering, and keep it OURS. Returns 0 when
# prod answers on 127.0.0.1:$PROD_TUNNEL_PORT (exported), non-zero when it doesn't — never exits, so a
# long-running caller (mirror.sh) can just retry on the next cycle.
ensure_prod_tunnel() {
  local tag="${TUNNEL_TAG:-tunnel}"
  PROD_TUNNEL_PORT="$(prod_tunnel_port)" || { echo "[$tag] no free local port in 27100-27899 for the prod tunnel" >&2; return 1; }
  export PROD_TUNNEL_PORT
  prod_tunnel_answers && return 0

  if [ -n "$PROD_TUNNEL_PID" ]; then
    echo "[$tag] tunnel to prod Mongo is down — restarting it"
    kill "$PROD_TUNNEL_PID" 2>/dev/null || true
    wait "$PROD_TUNNEL_PID" 2>/dev/null || true
    PROD_TUNNEL_PID=""
  elif nc -z -w2 127.0.0.1 "$PROD_TUNNEL_PORT" 2>/dev/null; then
    # Someone else's process holds the port and prod does not answer through it.
    # We must not kill it — it may be a forward someone is using — but we also
    # cannot bind over it, so say exactly what to look for instead of retrying
    # into a wall. (Our own free-port pick avoids this; it is a PINNED port, or
    # a port taken between the pick and the bind. After the cutover a stale
    # `flyctl proxy ... --app kinowo-mongo` on :27017 was the usual cause: that
    # app is stopped and its tunnel accepts connections that go nowhere.)
    echo "[$tag] :$PROD_TUNNEL_PORT is held by another process but prod does NOT answer through it." >&2
    echo "[$tag] Find the holder with 'lsof -nP -iTCP:$PROD_TUNNEL_PORT -sTCP:LISTEN' and stop that one pid yourself," >&2
    echo "[$tag] or unset PROD_TUNNEL_PORT to take a free port." >&2
    return 1
  fi

  local target; target="$(prod_tunnel_target)"
  echo "[$tag] opening ssh tunnel to prod Mongo ($target) on :$PROD_TUNNEL_PORT"
  # ExitOnForwardFailure: without it ssh happily holds a session open with NO
  # forward when the local bind fails, which reads as a live tunnel and fails
  # later as a connection error. BatchMode: under launchd there is no one to
  # answer a passphrase prompt, so hang-forever must become fail-now.
  # ServerAlive*: a laptop that sleeps or changes network leaves a half-open TCP
  # session that neither errors nor carries data — the ~45s probe window turns
  # that into a clean exit the supervisor can restart.
  # ControlMaster=no / ControlPath=none: the forward is THIS process's own
  # connection. Under `ControlMaster auto` + `ControlPersist` (a `Host *` default
  # in ~/.ssh/config) ssh hands it to a backgrounded master instead — ppid 1, the
  # same argv — so PROD_TUNNEL_PID is a client whose kill leaves the port open for
  # an hour, and mirror.sh's orphan sweep, which reads a ppid-1 forward as
  # stranded, killed the live tunnel on every start (a restart every ~50 s,
  # 2026-11-06 → 11-09, every mirror frozen).
  ssh -N -L "$PROD_TUNNEL_PORT:127.0.0.1:27017" \
      -o ExitOnForwardFailure=yes -o BatchMode=yes -o ConnectTimeout=10 \
      -o ServerAliveInterval=15 -o ServerAliveCountMax=3 \
      -o ControlMaster=no -o ControlPath=none \
      "$target" >/dev/null 2>&1 &
  PROD_TUNNEL_PID=$!

  local _
  for _ in $(seq 1 30); do
    prod_tunnel_answers && return 0
    # A tunnel that died on its own (bad key, host down) will never answer —
    # stop waiting out the full 30s for a process that is already gone, and say
    # that is what happened: "did not answer within 30s" after a 0s ssh exit sent
    # the reader looking for a slow prod instead of a refused login.
    if ! kill -0 "$PROD_TUNNEL_PID" 2>/dev/null; then
      local code=0; wait "$PROD_TUNNEL_PID" 2>/dev/null || code=$?
      PROD_TUNNEL_PID=""
      echo "[$tag] ssh to $target exited (code $code) before prod answered — 255 is a connection or login failure" >&2
      return 1
    fi
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
