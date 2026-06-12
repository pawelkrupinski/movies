#!/usr/bin/env bash
#
# Start (or reuse) the local single-node replica-set Mongo that mirrors prod's
# `movies` collection, so the dev `/debug` corpus dump is a fast LAN read
# instead of a 30–60s read over the prod `flyctl` tunnel. See README.md.
#
# Single-node replica set (not a standalone) because `/debug`'s live SSE view
# reads a change stream, and a standalone mongod rejects `$changeStream`
# (error 40573) — the same reason prod runs `--replSet rs0`. No `--auth`: it's
# a throwaway, loopback-only, dev-only mirror of already-public showtime data.
set -euo pipefail

CONTAINER=kinowo-local-mongo
VOLUME=kinowo-mirror-data
PORT="${LOCAL_MIRROR_PORT:-28017}"          # host port; container always 27017
IMAGE=mongo:7.0                             # match prod (server 7.0.x)
U="mongodb://127.0.0.1:${PORT}/?directConnection=true"

if docker ps --format '{{.Names}}' | grep -qx "$CONTAINER"; then
  echo "[local-mongo] already running on :$PORT"
elif docker ps -a --format '{{.Names}}' | grep -qx "$CONTAINER"; then
  echo "[local-mongo] starting existing container on :$PORT"
  docker start "$CONTAINER" >/dev/null
else
  echo "[local-mongo] creating container on :$PORT (image $IMAGE)"
  docker run -d --name "$CONTAINER" -p "$PORT:27017" \
    -v "$VOLUME:/data/db" "$IMAGE" --replSet rs0 --bind_ip_all >/dev/null
fi

# Wait for mongod to accept connections.
for _ in $(seq 1 60); do
  [ "$(mongosh "$U" --quiet --eval 'db.runCommand({ping:1}).ok' 2>/dev/null | tail -1)" = "1" ] && break
  sleep 1
done

# Initiate the single-node set once. `host` is the container-internal
# 127.0.0.1:27017; clients reach it from the host via `directConnection=true`,
# which bypasses replica-set host discovery, so the advertised host is moot.
mongosh "$U" --quiet --eval '
  try { rs.status(); print("[local-mongo] replica set already initiated"); }
  catch (e) {
    rs.initiate({_id:"rs0", members:[{_id:0, host:"127.0.0.1:27017"}]});
    print("[local-mongo] replica set initiated");
  }' 2>&1 | tail -1

# Change streams need a PRIMARY.
for i in $(seq 1 30); do
  [ "$(mongosh "$U" --quiet --eval 'print(db.hello().isWritablePrimary)' 2>/dev/null | tail -1)" = "true" ] \
    && { echo "[local-mongo] PRIMARY ready on :$PORT"; exit 0; }
  sleep 1
done
echo "[local-mongo] WARN: did not reach PRIMARY within 30s" >&2
exit 1
