#!/usr/bin/env bash
# Start a throwaway MongoDB in Docker as a SINGLE-NODE REPLICA SET on 127.0.0.1:27017, and return
# once it is PRIMARY.
#
# A replica set rather than a standalone because change streams (MovieRepoIntegrationSpec, the
# read-model projector) and multi-document transactions (the staging fold) are refused by a
# standalone mongod. Not a `services:` block because GitHub cannot override a service container's
# command, and `--replSet` is a command-line flag.
#
# BOUNDED. Every wait gives up after MONGO_START_TIMEOUT_SECONDS (default 180) with the
# container's log tail, instead of looping until the job's own ceiling -- which, in the nightly
# stress run, is three hours of a runner doing nothing.
#
# Usage: start-mongo-replset.sh [extra mongod args...]   e.g. --wiredTigerCacheSizeGB 2
# Tested by scripts/ci/start-mongo-replset-test.sh against a stub `docker`.
set -uo pipefail

timeout_seconds="${MONGO_START_TIMEOUT_SECONDS:-180}"
deadline=$((SECONDS + timeout_seconds))

mongosh_eval() { docker exec mongo mongosh --quiet --eval "$1"; }

# wait_for <what> <command...>: retry once a second until it succeeds or the deadline passes.
wait_for() {
    local what="$1"; shift
    until "$@"; do
        if [ "$SECONDS" -ge "$deadline" ]; then
            echo "::error::MongoDB did not become $what within ${timeout_seconds}s"
            docker logs --tail 50 mongo 2>&1 || true
            return 1
        fi
        sleep 1
    done
}

is_up()      { mongosh_eval 'db.runCommand({ping:1})' >/dev/null 2>&1; }
is_primary() { mongosh_eval 'rs.status().myState' 2>/dev/null | grep -q '^1$'; }

docker run -d --name mongo -p 27017:27017 mongo:7 --replSet rs0 "$@" || exit 1
wait_for "reachable" is_up || exit 1
mongosh_eval 'rs.initiate({_id:"rs0",members:[{_id:0,host:"127.0.0.1:27017"}]})' || exit 1
wait_for "PRIMARY" is_primary || exit 1
echo "MongoDB replica set rs0 is PRIMARY on 127.0.0.1:27017"
