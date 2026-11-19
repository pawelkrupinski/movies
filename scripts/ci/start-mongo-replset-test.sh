#!/usr/bin/env bash
# start-mongo-replset.sh against a stub `docker` whose mongod comes up (or never does) on cue.
# Run: bash scripts/ci/start-mongo-replset-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m start-mongo-replset.sh\n'

stub_dir="$(mktemp -d)"
trap 'rm -rf "$stub_dir"' EXIT
# STUB_MONGO: `healthy` answers ping and reports PRIMARY after rs.initiate; `down` never answers
# a ping (and its container cannot be inspected); `exited` is a container whose mongod died at start, its log's
# fatal line saying why; `secondary` answers but never becomes PRIMARY. A pull succeeds and `image inspect` then names
# the pulled ref (its digest with it); where the pull's sources fall back is scripts/ci/pull-pinned-image-test.sh's.
cat > "$stub_dir/docker" <<'STUB'
#!/usr/bin/env bash
printf '%s\n' "$*" >> "$STUB_LOG"
case "$1" in
  run) exit 0 ;;
  logs) [ "$STUB_MONGO" = exited ] && printf '%s\n' '{"t":{},"s":"I","msg":"starting"}' \
          '{"t":{},"s":"F","c":"CONTROL","msg":"MongoDB cannot start: Linux kernel versions 6.19 and newer has a known incompatibility"}'
        exit 0 ;;
  inspect) case "$STUB_MONGO" in exited) echo "false 14" ;; healthy|secondary) echo "true 0" ;; *) exit 1 ;; esac; exit 0 ;;
  pull) echo "${*: -1}" >> "$STUB_LOG.pulled"; exit 0 ;;
  image) grep -qxF -- "${*: -1}" "$STUB_LOG.pulled" 2>/dev/null && echo "${*: -1}"; exit 0 ;;
  info) exit 0 ;;
esac
eval_arg="${*: -1}"
case "$STUB_MONGO:$eval_arg" in
  down:*) exit 1 ;;
  *:"rs.status().myState") [ "$STUB_MONGO" = healthy ] && echo 1 || echo 2 ;;
esac
exit 0
STUB
chmod +x "$stub_dir/docker"
export STUB_LOG="$stub_dir/log"

# start <stub state> [args...] -> "<exit status>"
start() {
  export STUB_MONGO="$1"; shift
  : > "$STUB_LOG"; : > "$STUB_LOG.pulled"
  PATH="$stub_dir:$PATH" MONGO_START_TIMEOUT_SECONDS=2 MONGO_NATIVE_HOME="${NATIVE_HOME:-$stub_dir/no-native}" \
    MONGO_NATIVE_MARKER="$stub_dir/marker" MONGO_NATIVE_LOG="$stub_dir/mongod.log" MONGO_NATIVE_DBPATH="$stub_dir/db" \
    bash "$REPO_ROOT/scripts/ci/start-mongo-replset.sh" "$@" > "$stub_dir/out" 2>&1
  echo "$?"
}

check "a healthy mongod becomes PRIMARY and the script succeeds" "0" "$(start healthy)"
check "...having initiated the replica set once" "1" "$(grep -c 'rs.initiate' "$STUB_LOG")"
check "extra mongod args reach docker run" "1" \
  "$(start healthy --wiredTigerCacheSizeGB 2 >/dev/null; grep -c '^run .*--replSet rs0 --wiredTigerCacheSizeGB 2$' "$STUB_LOG")"
check "majority writes wait for the journal by default" "1" "$(grep -c 'writeConcernMajorityJournalDefault:true' "$STUB_LOG")"
check "...and only for the journal when asked not to" "1" \
  "$(MONGO_MAJORITY_JOURNAL=false start healthy >/dev/null; grep -c 'writeConcernMajorityJournalDefault:false' "$STUB_LOG")"
pinned="$(bash "$REPO_ROOT/scripts/ci/pull-pinned-image.sh" --pin mongo mirror)@$(bash "$REPO_ROOT/scripts/ci/pull-pinned-image.sh" --pin mongo digest)"
check "mongod listens on the runner's own network, not behind docker-proxy" "1" \
  "$(start healthy >/dev/null; grep -cxF -- "run -d --name mongo --network host $pinned --replSet rs0" "$STUB_LOG")"
check "...publishing no port to relay" "0" "$(grep -c -- ' -p ' "$STUB_LOG")"
check "...the pinned image by digest, pulled from the GHCR mirror first (pull-pinned-image.sh)" "pull -q $pinned" \
  "$(grep '^pull' "$STUB_LOG" | head -1)"
check "an image the job already pulled (pull-pinned-image's PINNED_IMAGE) runs as it is" "0 0 run -d --name mongo --network host pulled@sha256:x --replSet rs0" \
  "$(PINNED_IMAGE=pulled@sha256:x start healthy) $(grep -c '^pull' "$STUB_LOG") $(grep '^run' "$STUB_LOG")"
check "the data directory is on disk by default" "0" "$(start healthy >/dev/null; grep -c -- '--tmpfs' "$STUB_LOG")"
check "...and on a RAM-backed tmpfs of the size asked for" "1" \
  "$(MONGO_TMPFS=6g start healthy >/dev/null; grep -c -- '^run -d --name mongo --network host --tmpfs /data/db:rw,size=6g ghcr.io/' "$STUB_LOG")"
check "every probe keeps mongosh's state out of the data directory" "0" \
  "$(start healthy >/dev/null; grep '^exec ' "$STUB_LOG" | grep -vc '^exec -e HOME=/tmp mongo mongosh ')"
check "a mongod that never answers fails, bounded, instead of waiting forever" "1" "$(start down)"
check "...and says why" "1" "$(grep -c 'did not become reachable' "$stub_dir/out")"
# Convergence #523 (run 41199223125): every leg waited out its 180 s for a mongod that died in its first second.
check "a container whose mongod exited fails at once, not at the bound" "1" "$(MONGO_START_TIMEOUT_SECONDS=60 start exited)"
check "...on its first probe, not one a second for the 60 s bound" "1" "$(grep -c 'ping:1' "$STUB_LOG")"
check "...naming the exit and mongod's own fatal line in the error" "1" \
  "$(grep -c '^::error::MongoDB.s container exited (status 14) .*"msg":"MongoDB cannot start: Linux kernel versions 6.19' "$stub_dir/out")"
check "a mongod that times out names its log's last word in the error" "1" \
  "$(start down >/dev/null; grep -c '^::error::MongoDB did not become reachable within 2s: ' "$stub_dir/out")"
check "a healthy start says how long each wait took" "2" "$(start healthy >/dev/null; grep -c '^MongoDB \(reachable\|PRIMARY\) after [0-9]*s' "$stub_dir/out")"
check "a mongod that never becomes PRIMARY fails too" "1" "$(start secondary)"
check "...naming what it waited for" "1" "$(grep -c 'did not become PRIMARY' "$stub_dir/out")"

# NATIVE: the job image's own mongod and mongosh (stubs here, answering as the container's do), started on the
# job's localhost with the container's flags; no docker at all. `sudo` runs its command, logging it.
native="$stub_dir/native"
mkdir -p "$native/bin"
cat > "$native/bin/mongod" <<'STUB'
#!/usr/bin/env bash
[ "$1" = --version ] && { echo "db version v8.3.11"; exit 0; }
printf 'mongod %s\n' "$*" >> "$STUB_LOG"
[ "$STUB_MONGO" = down ] && echo "mongod: the log says why" >> "$MONGO_NATIVE_LOG"
exit 0
STUB
cat > "$native/bin/mongosh" <<'STUB'
#!/usr/bin/env bash
printf 'mongosh HOME=%s %s\n' "$HOME" "$*" >> "$STUB_LOG"
eval_arg="${*: -1}"
case "$STUB_MONGO:$eval_arg" in
  down:*) exit 1 ;;
  *:"rs.status().myState") [ "$STUB_MONGO" = healthy ] && echo 1 || echo 2 ;;
esac
exit 0
STUB
cat > "$stub_dir/sudo" <<'STUB'
#!/usr/bin/env bash
printf 'sudo %s\n' "$*" >> "$STUB_LOG"
[ "$1" = mount ] && exit "${STUB_MOUNT:-0}"
exec "$@"
STUB
chmod +x "$native/bin/mongod" "$native/bin/mongosh" "$stub_dir/sudo"
export MONGO_NATIVE_LOG="$stub_dir/mongod.log"
NATIVE_HOME=$native
check "a job image that bakes mongod starts it natively, PRIMARY" "0" "$(start healthy --wiredTigerCacheSizeGB 2)"
check "...with the container's flags, on localhost, forked to its log" "1" \
  "$(grep -c "^mongod --replSet rs0 --bind_ip 127.0.0.1 --port 27017 --dbpath $stub_dir/db --fork --logpath $stub_dir/mongod.log --wiredTigerCacheSizeGB 2$" "$STUB_LOG")"
check "...pulling and running no image at all" "0" "$(grep -c '^\(pull\|run\|info\) ' "$STUB_LOG")"
check "...initiating the set through the native mongosh, HOME out of the data directory" "1" \
  "$(grep -c '^mongosh HOME=/tmp --quiet --eval rs.initiate' "$STUB_LOG")"
check "...and naming its home for mongo-exec.sh" "$native" "$(cat "$stub_dir/marker")"
check "mongo-exec.sh then runs beside it, not in a container" "mongosh HOME=/tmp --quiet --eval x" \
  "$(: > "$STUB_LOG"; PATH="$stub_dir:$PATH" MONGO_NATIVE_MARKER="$stub_dir/marker" "$REPO_ROOT/scripts/ci/mongo-exec.sh" mongosh --quiet --eval x; cat "$STUB_LOG")"
check "...detached, at once" "0" \
  "$(PATH="$stub_dir:$PATH" MONGO_NATIVE_MARKER="$stub_dir/marker" "$REPO_ROOT/scripts/ci/mongo-exec.sh" -d true; echo $?)"
check "...and with no marker, in the mongo container as before" "exec -d -e HOME=/tmp mongo du -sh /data/db" \
  "$(: > "$STUB_LOG"; PATH="$stub_dir:$PATH" MONGO_NATIVE_MARKER="$stub_dir/none" "$REPO_ROOT/scripts/ci/mongo-exec.sh" -d du -sh /data/db; cat "$STUB_LOG")"
check "the native data directory on a RAM-backed tmpfs of the size asked for" "1" \
  "$(MONGO_TMPFS=6g start healthy >/dev/null; grep -c "^sudo mount -t tmpfs -o size=6g,uid=$(id -u),gid=$(id -g),mode=0700 tmpfs $stub_dir/db$" "$STUB_LOG")"
check "...a tmpfs that will not mount leaves it on disk, and says so" "0 1" \
  "$(STUB_MOUNT=1 MONGO_TMPFS=6g start healthy) $(grep -c 'could not mount a 6g tmpfs' "$stub_dir/out")"
check "a native mongod that never answers fails, bounded" "1" "$(start down)"
check "...with its own log's tail" "1" "$(grep -c '^mongod: the log says why' "$stub_dir/out")"
check "MONGO_NO_NATIVE=1 runs the container regardless" "1" \
  "$(MONGO_NO_NATIVE=1 start healthy >/dev/null; grep -c '^run -d --name mongo ' "$STUB_LOG")"
check "...leaving no marker behind" "0" "$([ -e "$stub_dir/marker" ] && echo 1 || echo 0)"
NATIVE_HOME=""

spec_summary
