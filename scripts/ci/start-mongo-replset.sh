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
# and failed the job before a single test ran. Should the mirror refuse it too, the image comes from
# Docker Hub after all -- logged in as the CI account when DOCKERHUB_USERNAME/DOCKERHUB_TOKEN are set
# (repo secrets; a fork's PR has none and pulls anonymously), so the pull counts against that
# account's limit instead of the shared IP's. The token goes to `docker login` on stdin, never argv.
#
# NATIVE WHERE THE JOB IMAGE BAKES IT: the Forgejo job image carries this very release's mongod and mongosh, copied out
# of the mongo image, at MONGO_NATIVE_HOME (/opt/mongodb/<version>; infra nix/files/forgejo-job-image). There it runs on
# the job's own localhost with the same flags -- no image pulled into the job's empty dockerd and no layers unpacked: 8 s
# of a convergence row's setup on average, 28 s at p90 (2026-10-23). Its data in /data/db (a tmpfs there for
# MONGO_TMPFS), its log at MONGO_NATIVE_LOG. It leaves a marker naming its home, which scripts/ci/mongo-exec.sh reads to
# run a command beside it instead of in the container. MONGO_NO_NATIVE=1 runs the container regardless.
set -uo pipefail
here=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)

timeout_seconds="${MONGO_START_TIMEOUT_SECONDS:-180}"
majority_journal="${MONGO_MAJORITY_JOURNAL:-true}"
deadline=$((SECONDS + timeout_seconds))

# HOME outside /data/db: the image's HOME is the data directory, so a probe's mongosh would write its
# lock files there while the entrypoint's `find /data/db ... -exec chown` walks it -- and a lock file
# gone between the two fails the chown, which kills the container before mongod starts. A tmpfs data
# directory starts root-owned, so the entrypoint walks it every time (run 37586335937, Poland: "chown:
# cannot access '/data/db/.mongodb/mongosh/am-unknown.json.lock'", never reachable).
native_home=${MONGO_NATIVE_HOME:-/opt/mongodb/8.3.13}
marker=${MONGO_NATIVE_MARKER:-/tmp/kinowo-mongo-native}
export MONGO_NATIVE_MARKER=$marker
log=${MONGO_NATIVE_LOG:-/tmp/mongod.log}
rm -f "$marker"
native=""
[ -z "${MONGO_NO_NATIVE:-}" ] && [ -x "$native_home/bin/mongod" ] && [ -x "$native_home/bin/mongosh" ] && native=1

mongosh_eval() { "$here/mongo-exec.sh" mongosh --quiet --eval "$1"; }
log_tail() { if [ -n "$native" ]; then tail -n 50 "$log" 2>&1; else docker logs --tail 50 mongo 2>&1; fi; }

# wait_for <what> <command...>: retry once a second until it succeeds or the deadline passes.
wait_for() {
    local what="$1"; shift
    until "$@"; do
        if [ "$SECONDS" -ge "$deadline" ]; then
            echo "::error::MongoDB did not become $what within ${timeout_seconds}s"
            log_tail || true
            return 1
        fi
        sleep 1
    done
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
image=mirror.gcr.io/library/mongo:8.3.13
hub_image=docker.io/library/mongo:8.3.13
# EVERY PULL IS BOUNDED, and a stalled one falls through to the next source like a refused one. github.com convergence
# 41216580092, `takes-drift-1 (united-states)`: the pull from mirror.gcr.io hung -- this script printed nothing for 9
# minutes, so the leg's 240 s wait for it failed -- while the same image came down in 24-75 s on every other leg of
# the run. A pull is ~300 MB: MONGO_PULL_SECONDS (default 120) is several times a slow one, not a race with a good one.
pull() {
    echo "pulling $1 (at most ${MONGO_PULL_SECONDS:-120}s)"
    timeout -k 10 "${MONGO_PULL_SECONDS:-120}" docker pull -q "$1"
    local status=$?
    [ "$status" -eq 124 ] && echo "::warning::the pull of $1 stalled past ${MONGO_PULL_SECONDS:-120}s"
    return "$status"
}
# A DOCKER HUB MIRROR OF THE DAEMON'S OWN comes first: on the Forgejo runners each job's dockerd is
# pointed at a pull-through cache on the CI host (infra's forgejo-registry-mirror), which serves Docker
# Hub's name from local disk instead of the 19-27 s internet pull every job made (2026-10-22). Only a
# Docker Hub name goes through a daemon's mirror; GitHub's runners have none and skip this.
if [ -n "$(docker info --format '{{range .RegistryConfig.Mirrors}}{{.}} {{end}}' 2>/dev/null)" ] &&
    pull "$hub_image"; then
    image="$hub_image"
elif ! pull "$image"; then
    echo "::warning::$image refused or stalled the pull; falling back to Docker Hub"
    image="$hub_image"
    if [ -n "${DOCKERHUB_USERNAME:-}" ] && [ -n "${DOCKERHUB_TOKEN:-}" ]; then
        printf '%s' "$DOCKERHUB_TOKEN" | docker login -u "$DOCKERHUB_USERNAME" --password-stdin || true
    fi
    pull "$image" || exit 1
fi
docker run -d --name mongo --network host ${storage[@]+"${storage[@]}"} "$image" --replSet rs0 "$@" || exit 1
wait_for "reachable" is_up || exit 1
mongosh_eval "rs.initiate({_id:\"rs0\",writeConcernMajorityJournalDefault:$majority_journal,members:[{_id:0,host:\"127.0.0.1:27017\"}]})" || exit 1
wait_for "PRIMARY" is_primary || exit 1
echo "MongoDB replica set rs0 is PRIMARY on 127.0.0.1:27017"
