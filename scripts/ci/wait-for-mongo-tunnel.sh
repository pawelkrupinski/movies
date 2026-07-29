#!/usr/bin/env bash
#
# Start a `flyctl proxy` to prod Mongo and DO NOT RETURN until bytes actually flow
# through it — then hold it open for the rest of the job.
#
# Why this is not just `flyctl proxy … & nc -z`: `nc -z` succeeds the instant
# flyctl BINDS the local port, which happens before it has a usable upstream. The
# convergence legs took that as ready, connected, and got
#
#   MongoTimeoutException: … state=CONNECTING,
#   exception={MongoSocketReadTimeoutException: Timeout while receiving message}
#
# — a socket that opens and then never answers. Every read then failed its
# retries, the corpus came back empty, and three legs failed eight minutes later
# on a tunnel that had reported itself healthy at second three.
#
# So the readiness probe speaks Mongo. It sends the smallest legal handshake (a
# legacy OP_QUERY `{isMaster:1}` against `admin.$cmd`) and requires a reply — the
# one check that distinguishes "port is bound" from "the database is reachable".
# No mongosh needed; GitHub runners have python3.
#
# The proxy is also RESTARTED rather than trusted: flyctl's tunnel is known to
# come up unusable, and a retry costs seconds against a leg that otherwise burns
# its whole timeout.
#
# Usage:  scripts/ci/wait-for-mongo-tunnel.sh [local-port] [app]
#           local-port  default 27018 (27017 is the leg's own throwaway Mongo)
#           app         default kinowo-mongo
# Requires FLY_API_TOKEN in the environment.
set -euo pipefail

PORT="${1:-27018}"
APP="${2:-kinowo-mongo}"
ATTEMPTS="${TUNNEL_ATTEMPTS:-3}"
PROBE_TRIES="${TUNNEL_PROBE_TRIES:-30}"
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROBE="$HERE/mongo-ping.py"

[ -f "$PROBE" ] || { echo "[tunnel] missing probe script at $PROBE" >&2; exit 1; }

# SUPERVISED, not started-and-trusted. `flyctl proxy` does not merely come up
# unusable — it DIES mid-job, and everything after it sees `Connection refused`
# for the rest of the run:
#
#   attempt 1/3 failed: MongoSocketReadException: Prematurely reached end of stream
#   attempt 2/3 failed: ConnectException: Connection refused
#
# A convergence leg holds this tunnel open for tens of minutes across tens of
# thousands of queries, so "it was alive when the step finished" says nothing. The
# supervisor restarts it whenever it exits, which turns a fatal drop into a couple
# of seconds the driver's own retries ride straight over.
LOG="${TUNNEL_LOG:-/tmp/mongo-tunnel.log}"
SUPERVISOR="${TMPDIR:-/tmp}/mongo-tunnel-supervisor.sh"
cat > "$SUPERVISOR" <<SUPERVISE
#!/usr/bin/env bash
while true; do
  flyctl proxy "$PORT:27017" --app "$APP" >>"$LOG" 2>&1 || true
  echo "[tunnel] proxy exited — restarting" >>"$LOG"
  sleep 2
done
SUPERVISE
chmod +x "$SUPERVISOR"

# `setsid` + `nohup`, not a plain `&`: the supervisor has to outlive BOTH this
# script and the step's shell, because the reads it exists for happen several
# steps later. A backgrounded child stays in the shell's process group and dies
# with it — which is exactly what happened when this was tested, leaving the probe
# passing and the tunnel gone moments afterwards.
setsid nohup "$SUPERVISOR" >/dev/null 2>&1 < /dev/null &
echo "[tunnel] supervisor detached, log $LOG"

for _ in $(seq 1 $((ATTEMPTS * PROBE_TRIES))); do
  if python3 "$PROBE" "$PORT" 2>/dev/null; then
    echo "[tunnel] ready on :$PORT — Mongo answered a handshake"
    exit 0
  fi
  sleep 2
done

echo "[tunnel] prod Mongo tunnel never answered a handshake on :$PORT" >&2
exit 1
