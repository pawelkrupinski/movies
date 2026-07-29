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

for attempt in $(seq 1 "$ATTEMPTS"); do
  echo "[tunnel] attempt $attempt/$ATTEMPTS: flyctl proxy $PORT:27017 --app $APP"
  flyctl proxy "$PORT:27017" --app "$APP" &
  PROXY_PID=$!

  for _ in $(seq 1 "$PROBE_TRIES"); do
    if python3 "$PROBE" "$PORT" 2>/dev/null; then
      echo "[tunnel] ready on :$PORT — Mongo answered a handshake (pid $PROXY_PID)"
      exit 0
    fi
    sleep 2
  done

  echo "[tunnel] attempt $attempt bound :$PORT but Mongo never answered — restarting"
  kill "$PROXY_PID" 2>/dev/null || true
  wait "$PROXY_PID" 2>/dev/null || true
done

echo "[tunnel] prod Mongo tunnel never became usable after $ATTEMPTS attempts" >&2
exit 1
