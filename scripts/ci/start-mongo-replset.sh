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
#
# MONGO_MAJORITY_JOURNAL=false acknowledges a majority write once it is applied, not once it is
# synced to the journal (the replica set's `writeConcernMajorityJournalDefault`). Every write the
# app makes is `w: majority`, so on a database that dies with the runner it is a disk sync per write
# bought for nothing: ~7 ms a write against ~0.2 ms, measured on a single-node set. A convergence leg
# makes hundreds of thousands of them (the US take-up alone ~160,000).
#
# MONGO_TMPFS=<size> (e.g. 6g) puts the data directory on a RAM-backed tmpfs of at most that size.
# A convergence leg's replays write hundreds of thousands of documents into databases that die with
# the runner, and on disk they kept 1.3 of the runner's 4 cores in iowait (run 37513892540); a leg
# holds 1.1-1.5 GB there. Only where the box has the room: a 10g-heap leg does not.
#
# On the runner's own network (`--network host`), not a published port: a client reaching a published
# port on 127.0.0.1 is relayed by docker-proxy, a userland process copying every byte of every round
# trip. A convergence leg makes millions of them, and through its detail phases the runner was ~0.7 of
# a core busier than the JVM and mongod together (run 37581348550). The image's mongod binds every
# interface, so 127.0.0.1:27017 reaches it directly.
#
# The image is pulled through mirror.gcr.io, Google's pull-through cache of Docker Hub's official
# images, not from Docker Hub itself: GitHub's runners pull anonymously and share egress IPs, so
# Docker Hub's per-IP anonymous limit refused the pull outright ("toomanyrequests", run 37989550418)
# and failed the job before a single test ran.
set -uo pipefail

timeout_seconds="${MONGO_START_TIMEOUT_SECONDS:-180}"
majority_journal="${MONGO_MAJORITY_JOURNAL:-true}"
deadline=$((SECONDS + timeout_seconds))

# HOME outside /data/db: the image's HOME is the data directory, so a probe's mongosh would write its
# lock files there while the entrypoint's `find /data/db ... -exec chown` walks it -- and a lock file
# gone between the two fails the chown, which kills the container before mongod starts. A tmpfs data
# directory starts root-owned, so the entrypoint walks it every time (run 37586335937, Poland: "chown:
# cannot access '/data/db/.mongodb/mongosh/am-unknown.json.lock'", never reachable).
mongosh_eval() { docker exec -e HOME=/tmp mongo mongosh --quiet --eval "$1"; }

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

storage=()
[ -n "${MONGO_TMPFS:-}" ] && storage=(--tmpfs "/data/db:rw,size=${MONGO_TMPFS}")
docker run -d --name mongo --network host ${storage[@]+"${storage[@]}"} mirror.gcr.io/library/mongo:8.3.11 --replSet rs0 "$@" || exit 1
wait_for "reachable" is_up || exit 1
mongosh_eval "rs.initiate({_id:\"rs0\",writeConcernMajorityJournalDefault:$majority_journal,members:[{_id:0,host:\"127.0.0.1:27017\"}]})" || exit 1
wait_for "PRIMARY" is_primary || exit 1
echo "MongoDB replica set rs0 is PRIMARY on 127.0.0.1:27017"
