#!/usr/bin/env bash
# Purpose: Start a throwaway MongoDB in Docker as a single-node replica set on 127.0.0.1:27017, and return once it is PRIMARY.
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
# The image is .github/pinned-images.tsv's `mongo`, pulled BY DIGEST by scripts/ci/pull-pinned-image.sh: GitHub's own
# registry first (this repo's GHCR mirror of it), then mirror.gcr.io, then Docker Hub. In CI the job pulls it through
# ./.github/actions/pull-pinned-image, which runs this script after the pull with PINNED_IMAGE set to what it pulled
# (CiPinnedImageSpec); without PINNED_IMAGE (locally, a test) this script runs the pull itself.
#
# NATIVE WHERE THE JOB IMAGE BAKES IT: a job image may carry this very release's mongod and mongosh, copied out
# of the mongo image, at MONGO_NATIVE_HOME (/opt/mongodb/<version>). There it runs on
# the job's own localhost with the same flags -- no image pulled into the job's empty dockerd and no layers unpacked: 8 s
# of a convergence row's setup on average, 28 s at p90 (2026-10-23). Its data in /data/db (a tmpfs there for
# MONGO_TMPFS), its log at MONGO_NATIVE_LOG. It leaves a marker naming its home, which scripts/ci/mongo-exec.sh reads to
# run a command beside it instead of in the container. MONGO_NO_NATIVE=1 runs the container regardless.
set -uo pipefail
here=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)

timeout_seconds="${MONGO_START_TIMEOUT_SECONDS:-180}"
majority_journal="${MONGO_MAJORITY_JOURNAL:-true}"
deadline=$((SECONDS + timeout_seconds))
started=$SECONDS

# HOME outside /data/db: the image's HOME is the data directory, so a probe's mongosh would write its
# lock files there while the entrypoint's `find /data/db ... -exec chown` walks it -- and a lock file
# gone between the two fails the chown, which kills the container before mongod starts. A tmpfs data
# directory starts root-owned, so the entrypoint walks it every time (run 37586335937, Poland: "chown:
# cannot access '/data/db/.mongodb/mongosh/am-unknown.json.lock'", never reachable).
# The pin's tag is the release (MongoServerVersionParitySpec), so the baked home is /opt/mongodb/<that tag>.
native_home=${MONGO_NATIVE_HOME:-/opt/mongodb/$("$here/pull-pinned-image.sh" --pin mongo tag)}
marker=${MONGO_NATIVE_MARKER:-/tmp/kinowo-mongo-native}
export MONGO_NATIVE_MARKER=$marker
log=${MONGO_NATIVE_LOG:-/tmp/mongod.log}
rm -f "$marker"
native=""
[ -z "${MONGO_NO_NATIVE:-}" ] && [ -x "$native_home/bin/mongod" ] && [ -x "$native_home/bin/mongosh" ] && native=1

mongosh_eval() { "$here/mongo-exec.sh" mongosh --quiet --eval "$1"; }
log_tail() { if [ -n "$native" ]; then tail -n 50 "$log" 2>&1; else docker logs --tail 50 mongo 2>&1; fi; }

# A CONTAINER THAT HAS EXITED never becomes reachable: say so at once, with mongod's own fatal line, instead of polling it
# for the whole bound. Every leg of convergence #523 (run 41199223125, 2026-10-31) waited out its 180 s for a mongod that
# had died in its first second ("MongoDB cannot start: Linux kernel versions 6.19 and newer has a known incompatibility",
# SERVER-121912; fixed by running those jobs on ubuntu-22.04, c85592243), and the log said only "did not become
# reachable". A `docker inspect` that cannot answer says nothing either way: the wait goes on. The native mongod
# needs no such check: `--fork` returns only once the child is up, and fails when it is not.
container_exited() {
    [ -z "$native" ] || return 1
    local state
    state=$(docker inspect -f '{{.State.Running}} {{.State.ExitCode}}' mongo 2>/dev/null) || return 1
    [ "${state%% *}" = false ] || return 1
    echo "::error::MongoDB's container exited (status ${state#* }) $((SECONDS - started))s after it started, before it became $1: $(fatal_line)"
}
# mongod's first fatal ("s":"F") log line, else its last line: the reason a start failed, for the error line itself.
fatal_line() {
    local tail_text fatal
    tail_text=$(log_tail 2>/dev/null)
    fatal=$(grep -m1 '"s":"F"' <<< "$tail_text" | grep -o '"msg":"[^"]*"' | head -1)
    [ -n "$fatal" ] || fatal=$(tail -n 1 <<< "$tail_text")
    printf '%s' "${fatal:-no log}" | cut -c1-300
}

# wait_for <what> <command...>: retry once a second until it succeeds, the deadline passes or the container exits.
# Each wait says how long it took: a start that is slow (a cold pull, a loaded runner) shows where its time went.
wait_for() {
    local what="$1" waited=$SECONDS; shift
    until "$@"; do
        if container_exited "$what"; then
            log_tail || true
            return 1
        fi
        if [ "$SECONDS" -ge "$deadline" ]; then
            echo "::error::MongoDB did not become $what within ${timeout_seconds}s: $(fatal_line)"
            log_tail || true
            return 1
        fi
        sleep 1
    done
    echo "MongoDB $what after $((SECONDS - waited))s ($((SECONDS - started))s since the start)"
}

is_up()      { mongosh_eval 'db.runCommand({ping:1})' >/dev/null 2>&1; }
is_primary() { mongosh_eval 'rs.status().myState' 2>/dev/null | grep -q '^1$'; }

if [ -n "$native" ]; then
    dbpath=${MONGO_NATIVE_DBPATH:-/data/db}
    { mkdir -p "$dbpath" 2>/dev/null || sudo mkdir -p "$dbpath"; } && [ -w "$dbpath" ] || sudo chown "$(id -u):$(id -g)" "$dbpath" || exit 1
    if [ -n "${MONGO_TMPFS:-}" ]; then
        # A tmpfs that will not mount leaves the data on disk: slower, never wrong.
        sudo mount -t tmpfs -o "size=$MONGO_TMPFS,uid=$(id -u),gid=$(id -g),mode=0700" tmpfs "$dbpath" \
            || echo "::warning::could not mount a ${MONGO_TMPFS} tmpfs at $dbpath -- MongoDB's data stays on disk"
    fi
    # one .wt file per collection: a hosted runner's open-file limit, as the container's dockerd gives it
    ulimit -n "$(ulimit -Hn)" 2> /dev/null || true
    echo "MongoDB: the job image's native $("$native_home/bin/mongod" --version | head -1)"
    "$native_home/bin/mongod" --replSet rs0 --bind_ip 127.0.0.1 --port 27017 --dbpath "$dbpath" \
        --fork --logpath "$log" "$@" > /dev/null || { tail -n 50 "$log" 2>/dev/null; exit 1; }
    echo "$native_home" > "$marker"
    wait_for "reachable" is_up || exit 1
    mongosh_eval "rs.initiate({_id:\"rs0\",writeConcernMajorityJournalDefault:$majority_journal,members:[{_id:0,host:\"127.0.0.1:27017\"}]})" || exit 1
    wait_for "PRIMARY" is_primary || exit 1
    echo "MongoDB replica set rs0 is PRIMARY on 127.0.0.1:27017"
    exit 0
fi

storage=()
[ -n "${MONGO_TMPFS:-}" ] && storage=(--tmpfs "/data/db:rw,size=${MONGO_TMPFS}")
image=${PINNED_IMAGE:-}
[ -n "$image" ] || image=$("$here/pull-pinned-image.sh" mongo) || exit 1
docker run -d --name mongo --network host ${storage[@]+"${storage[@]}"} "$image" --replSet rs0 "$@" || exit 1
wait_for "reachable" is_up || exit 1
mongosh_eval "rs.initiate({_id:\"rs0\",writeConcernMajorityJournalDefault:$majority_journal,members:[{_id:0,host:\"127.0.0.1:27017\"}]})" || exit 1
wait_for "PRIMARY" is_primary || exit 1
echo "MongoDB replica set rs0 is PRIMARY on 127.0.0.1:27017"
