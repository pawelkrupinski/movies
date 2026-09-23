#!/usr/bin/env bash
#
# Ensure the native (Homebrew) single-node replica-set Mongo that backs local
# dev is up on :28017. Single-node replica set (not standalone) because both the
# /debug live SSE view AND the local web+worker stack read change streams, which
# a standalone mongod rejects (error 40573) — the same reason prod runs
# `--replSet rs0`. No auth: it's loopback-only, dev-only, already-public data.
#
# One instance, two databases:
#   - kinowo_prod_mirror  synced from prod by mirror.sh (the /debug corpus mirror, read-only)
#   - kinowo_local        the local web+worker read/write playground (its own change streams)
#
# This replaces the former `mongo:7.0` Docker container. It's managed by
# `brew services` when that works, so it restarts at login on its own; mirror.sh
# re-invokes this whenever :28017 goes unreachable.
#
# The formula is DETECTED, not hardcoded: a hardcoded `mongodb-community@7.0`
# failed outright (`set -e`, nothing started) on a machine whose `brew services`
# could not find/load that service. And when `brew services` cannot start it for
# any reason, this falls back to a plain `mongod --config … --fork` — same config,
# just not restarted at login. `start-local-mongo-spec.sh` holds both paths.
set -euo pipefail

PORT="${LOCAL_MIRROR_PORT:-28017}"
PREFIX="$(brew --prefix)"
CONF="$PREFIX/etc/mongod.conf"
DBPATH="$PREFIX/var/mongodb"
LOGPATH="$PREFIX/var/log/mongodb/mongo.log"
U="mongodb://127.0.0.1:${PORT}/?directConnection=true"

# Our config is the source of truth: loopback, our port, single-node replica
# set. `brew services` launches `mongod --config "$CONF"`, so writing it here
# (default is a standalone on 27017) is what makes change streams work and keeps
# us off the prod tunnel's 27017.
mkdir -p "$DBPATH" "$(dirname "$LOGPATH")"
cat > "$CONF" <<CONF
systemLog:
  destination: file
  path: $LOGPATH
  logAppend: true
storage:
  dbPath: $DBPATH
net:
  bindIp: 127.0.0.1
  port: $PORT
replication:
  replSetName: rs0
CONF

# The installed MongoDB formula — the newest `mongodb-community[@X.Y]`, or the one
# LOCAL_MONGO_FORMULA names. Empty when brew has none (mongod from elsewhere).
FORMULA="${LOCAL_MONGO_FORMULA:-$(brew list --formula -1 2>/dev/null \
  | grep -E '^mongodb-community(@[0-9.]+)?$' | sort -V | tail -1 || true)}"

answers_ping() {
  local tries="$1"
  for _ in $(seq 1 "$tries"); do
    [ "$(mongosh "$U" --quiet --eval 'db.runCommand({ping:1}).ok' 2>/dev/null | tail -1)" = "1" ] && return 0
    sleep 1
  done
  return 1
}

# The formula's own mongod when brew has one, so the fallback runs the same
# version `brew services` would; else whatever `mongod` is on PATH.
mongod_bin() {
  local own="$PREFIX/opt/$FORMULA/bin/mongod"
  if [ -n "$FORMULA" ] && [ -x "$own" ]; then echo "$own"; else echo mongod; fi
}

# (Re)start to apply the config, then wait for connections. Only called when
# :28017 is down (or at first setup), so a restart is safe here.
started=""
if [ -n "$FORMULA" ]; then
  echo "[local-mongo] (re)starting $FORMULA via brew services on :$PORT"
  if brew services restart "$FORMULA" >/dev/null 2>&1 && answers_ping 30; then started="brew services ($FORMULA)"
  else echo "[local-mongo] brew services could not start $FORMULA — falling back to mongod --fork" >&2
  fi
else
  echo "[local-mongo] no mongodb-community formula installed — starting mongod directly" >&2
fi
if [ -z "$started" ]; then
  "$(mongod_bin)" --config "$CONF" --fork >/dev/null
  answers_ping 60 || { echo "[local-mongo] mongod did not answer on :$PORT within 60s" >&2; exit 1; }
  started="mongod --fork"
fi

# Initiate the single-node set once; the catch makes it idempotent on restarts.
mongosh "$U" --quiet --eval '
  try { rs.status(); print("[local-mongo] replica set already initiated"); }
  catch (e) {
    rs.initiate({_id:"rs0", members:[{_id:0, host:"127.0.0.1:'"$PORT"'"}]});
    print("[local-mongo] replica set initiated");
  }' 2>&1 | tail -1

# Change streams need a PRIMARY.
for _ in $(seq 1 30); do
  [ "$(mongosh "$U" --quiet --eval 'print(db.hello().isWritablePrimary)' 2>/dev/null | tail -1)" = "true" ] \
    && { echo "[local-mongo] PRIMARY ready on :$PORT ($started)"; exit 0; }
  sleep 1
done
echo "[local-mongo] WARN: did not reach PRIMARY within 30s" >&2
exit 1
