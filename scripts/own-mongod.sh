#!/usr/bin/env bash
# A MongoDB of the caller's OWN: a single-node replica set on a free port, its data in a temporary directory, removed
# when the caller ends. Sourcing it defines functions only. Used by scripts/it-local.sh (the it/ layer) and
# scripts/identity-capture.sh --offline (a capture's throwaway database).
#
#   own_mongod_start <tag>   -> starts it; sets OWN_MONGOD_URI (mongodb://127.0.0.1:<port>/?directConnection=true),
#                               OWN_MONGOD_PORT, OWN_MONGOD_DATA. Fails (non-zero, said on stderr) when no mongod or
#                               mongosh is installed, or the server is not ours or never becomes PRIMARY.
#   own_mongod_stop          -> stops it and removes its data; safe to call twice, and before a start. The caller's
#                               EXIT/INT/TERM trap calls it.
#
# Why not the shared mongod on :28017: every agent on this Mac writes to it at once — another itAll, an identity shadow
# over a whole country, a convergence leg — and a server's capacity is shared even when its databases are not. On
# 2026-10-12 an itAll on a unique MONGODB_DB prefix still failed seven suites, every one on a Mongo round trip past its
# 10 s budget: the server's 12 write tickets were all out with 29 writers queued, writes waited 4-5 s on the journal
# (`waitForWriteConcernDurationMillis`) behind another agent's identity shadow, and flow control throttled whoever was
# left. Each suite passed alone. And :28017 also holds the local prod mirror, which nothing but its tailer writes.
#
# A replica set because change streams and the staging fold's transactions need one; a 1 GB cache so several agents'
# servers fit in memory side by side. $IT_LOCAL_START_SECONDS (default 60) bounds each of the two waits: for the server
# to answer, then to become PRIMARY.
#
# writeConcernMajorityJournalDefault: false — a majority write is acknowledged once applied, not once the journal is
# synced, exactly as CI's throwaway set (scripts/ci/start-mongo-replset.sh, MONGO_MAJORITY_JOURNAL). On a single node
# "majority" is otherwise "this node's journal fsync'd", and macOS syncs with F_FULLFSYNC, a whole-drive cache flush
# every agent's mongod and build queues behind: on 2026-10-23, at load 300-700, a suite's `dropDatabase` — which
# ALWAYS waits for majority, w:majority/wtimeout 10 min hard-coded in the server, whatever the client asks — failed
# with "waiting for replication timed out" (MovieRepositoryIntegrationSpec). Durability buys nothing here: the data
# directory is deleted when the run ends. scripts/convergence-local.sh's throwaway container initiates its set with the
# same document (throwaway_replica_set_initiate).

OWN_MONGOD_PID="" OWN_MONGOD_DATA="" OWN_MONGOD_PORT="" OWN_MONGOD_URI=""

# throwaway_replica_set_initiate <host:port> -> the mongosh expression initiating a throwaway single-node set rs0 on that
# member, majority acknowledged on apply (above).
throwaway_replica_set_initiate() {
    printf "rs.initiate({_id: 'rs0', writeConcernMajorityJournalDefault: false, members: [{_id: 0, host: '%s'}]})" "$1"
}

own_mongod_start() {
    local tag=${1:-own} mongod mongosh candidate ours="" primary="" start_seconds=${IT_LOCAL_START_SECONDS:-60}
    mongod="$(command -v mongod || true)"
    if [ -z "$mongod" ]; then
        for candidate in /opt/homebrew/opt/mongodb-community*/bin/mongod; do
            [ -x "$candidate" ] && { mongod="$candidate"; break; }
        done
    fi
    [ -n "$mongod" ] || { echo "no mongod on PATH or under /opt/homebrew/opt/mongodb-community* (brew install mongodb-community@7.0)" >&2; return 1; }
    mongosh="$(command -v mongosh || true)"
    [ -n "$mongosh" ] || { echo "no mongosh on PATH (brew install mongosh)" >&2; return 1; }

    OWN_MONGOD_DATA="$(mktemp -d "${TMPDIR:-/tmp}/kinowo-$tag-mongo.XXXXXX")"
    OWN_MONGOD_PORT="$(python3 -I -c 'import socket; s = socket.socket(); s.bind(("127.0.0.1", 0)); print(s.getsockname()[1])')"
    # the lock descriptors closed (scripts/jvm-gate.sh): a mongod left behind by a killed run holds none of its locks
    "$mongod" --replSet rs0 --bind_ip 127.0.0.1 --port "$OWN_MONGOD_PORT" --dbpath "$OWN_MONGOD_DATA" \
        --wiredTigerCacheSizeGB 1 --logpath "$OWN_MONGOD_DATA/mongod.log" >/dev/null 2>&1 6>&- 7>&- 8>&- &
    OWN_MONGOD_PID=$!
    OWN_MONGOD_URI="mongodb://127.0.0.1:$OWN_MONGOD_PORT/?directConnection=true"

    # The free port is only free when it was picked: another agent's server can take it before ours binds, and a bare
    # ping would then pass against THAT server — this run writing into another run's databases. It is ours only when it
    # serves our data directory.
    for _ in $(seq 1 "$start_seconds"); do
        kill -0 "$OWN_MONGOD_PID" 2>/dev/null || { echo "mongod exited on start — see $OWN_MONGOD_DATA/mongod.log:" >&2; tail -20 "$OWN_MONGOD_DATA/mongod.log" >&2; return 1; }
        [ "$("$mongosh" --quiet "$OWN_MONGOD_URI" --eval 'db.serverCmdLineOpts().parsed.storage.dbPath' 2>/dev/null)" = "$OWN_MONGOD_DATA" ] && { ours=1; break; }
        sleep 1
    done
    [ -n "$ours" ] || { echo "no mongod of this run's own answered on :$OWN_MONGOD_PORT within ${start_seconds}s (another server holds the port?)" >&2; return 1; }
    "$mongosh" --quiet "$OWN_MONGOD_URI" --eval "$(throwaway_replica_set_initiate "127.0.0.1:$OWN_MONGOD_PORT")" >/dev/null
    for _ in $(seq 1 "$start_seconds"); do
        [ "$("$mongosh" --quiet "$OWN_MONGOD_URI" --eval 'rs.status().myState' 2>/dev/null)" = "1" ] && { primary=1; break; }
        sleep 1
    done
    # Change streams and the staging fold's transactions need a primary: without one every suite fails, far from here.
    [ -n "$primary" ] || { echo "mongod on :$OWN_MONGOD_PORT did not become PRIMARY within ${start_seconds}s — see $OWN_MONGOD_DATA/mongod.log" >&2; tail -20 "$OWN_MONGOD_DATA/mongod.log" >&2; return 1; }
    echo "==> mongod $("$mongod" --version | head -1 | awk '{print $NF}') of this run's own on :$OWN_MONGOD_PORT ($OWN_MONGOD_DATA)"
}

own_mongod_stop() {
    if [ -n "$OWN_MONGOD_PID" ] && kill -0 "$OWN_MONGOD_PID" 2>/dev/null; then
        kill "$OWN_MONGOD_PID" 2>/dev/null || true
        for _ in $(seq 1 30); do kill -0 "$OWN_MONGOD_PID" 2>/dev/null || break; sleep 1; done
        kill -9 "$OWN_MONGOD_PID" 2>/dev/null || true
    fi
    OWN_MONGOD_PID=""
    [ -n "$OWN_MONGOD_DATA" ] && rm -rf "$OWN_MONGOD_DATA"
    OWN_MONGOD_DATA=""
}
