#!/usr/bin/env bash
#
# Run the it/ layer locally against a MongoDB of this run's own.
#
#   scripts/it-local.sh                      # sbt itAll
#   scripts/it-local.sh 'worker/IntegrationTest/testOnly integration.FooSpec'
#
# Why not the shared mongod on :28017: every agent on this Mac writes to it at once — another
# itAll, an identity shadow over a whole country, a convergence leg — and a server's capacity is
# shared even when its databases are not. On 2026-10-12 an itAll on a unique MONGODB_DB prefix
# still failed seven suites, every one on a Mongo round trip past its 10 s budget: the server's
# 12 write tickets were all out with 29 writers queued, writes waited 4-5 s on the journal
# (`waitForWriteConcernDurationMillis`) behind another agent's identity shadow, and flow control
# throttled whoever was left. Each suite passed alone. CI never sees it: its integration job has a
# mongod to itself.
#
# So this gives the run the same: a single-node replica set (change streams and the staging
# fold's transactions need one) on a free port, its data in a temporary directory, a 1 GB cache
# so several agents' servers fit in memory side by side, and everything removed when the run
# ends — however it ends. MONGODB_DB is set too (`kinowo_it`): no other run can reach this server,
# so the prefix cannot collide.
set -euo pipefail

cd "$(dirname "$0")/.."

MONGOD="$(command -v mongod || true)"
if [ -z "$MONGOD" ]; then
    for candidate in /opt/homebrew/opt/mongodb-community*/bin/mongod; do
        [ -x "$candidate" ] && { MONGOD="$candidate"; break; }
    done
fi
[ -n "$MONGOD" ] || { echo "no mongod on PATH or under /opt/homebrew/opt/mongodb-community* (brew install mongodb-community@7.0)" >&2; exit 1; }
MONGOSH="$(command -v mongosh || true)"
[ -n "$MONGOSH" ] || { echo "no mongosh on PATH (brew install mongosh)" >&2; exit 1; }

DATA="$(mktemp -d "${TMPDIR:-/tmp}/kinowo-it-mongo.XXXXXX")"
PORT="$(python3 -I -c 'import socket; s = socket.socket(); s.bind(("127.0.0.1", 0)); print(s.getsockname()[1])')"
MONGOD_PID=""

# shellcheck disable=SC2329 # invoked by the trap below
cleanup() {
    if [ -n "$MONGOD_PID" ] && kill -0 "$MONGOD_PID" 2>/dev/null; then
        kill "$MONGOD_PID" 2>/dev/null || true
        for _ in $(seq 1 30); do kill -0 "$MONGOD_PID" 2>/dev/null || break; sleep 1; done
        kill -9 "$MONGOD_PID" 2>/dev/null || true
    fi
    rm -rf "$DATA"
}
trap cleanup EXIT INT TERM

"$MONGOD" --replSet rs0 --bind_ip 127.0.0.1 --port "$PORT" --dbpath "$DATA" \
    --wiredTigerCacheSizeGB 1 --logpath "$DATA/mongod.log" >/dev/null 2>&1 &
MONGOD_PID=$!

URI="mongodb://127.0.0.1:$PORT/?directConnection=true"
for _ in $(seq 1 60); do
    "$MONGOSH" --quiet "$URI" --eval 'db.runCommand({ping: 1}).ok' >/dev/null 2>&1 && break
    kill -0 "$MONGOD_PID" 2>/dev/null || { echo "mongod exited on start — see $DATA/mongod.log:" >&2; tail -20 "$DATA/mongod.log" >&2; exit 1; }
    sleep 1
done
"$MONGOSH" --quiet "$URI" --eval "rs.initiate({_id: 'rs0', members: [{_id: 0, host: '127.0.0.1:$PORT'}]})" >/dev/null
for _ in $(seq 1 60); do
    [ "$("$MONGOSH" --quiet "$URI" --eval 'rs.status().myState' 2>/dev/null)" = "1" ] && break
    sleep 1
done
echo "==> mongod $("$MONGOD" --version | head -1 | awk '{print $NF}') of this run's own on :$PORT ($DATA)"

status=0
MONGODB_URI="$URI" MONGODB_DB="kinowo_it" sbt "${@:-itAll}" || status=$?
exit "$status"
