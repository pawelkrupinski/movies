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
# `brew services`, so it restarts at login on its own; mirror.sh re-invokes this
# whenever :28017 goes unreachable.
set -euo pipefail

SERVICE="mongodb-community@7.0"
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

# (Re)start to apply the config, then wait for connections. Only called when
# :28017 is down (or at first setup), so a restart is safe here.
echo "[local-mongo] (re)starting $SERVICE on :$PORT"
brew services restart "$SERVICE" >/dev/null

for _ in $(seq 1 60); do
  [ "$(mongosh "$U" --quiet --eval 'db.runCommand({ping:1}).ok' 2>/dev/null | tail -1)" = "1" ] && break
  sleep 1
done

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
    && { echo "[local-mongo] PRIMARY ready on :$PORT ($SERVICE)"; exit 0; }
  sleep 1
done
echo "[local-mongo] WARN: did not reach PRIMARY within 30s" >&2
exit 1
