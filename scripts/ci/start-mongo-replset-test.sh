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
# a ping; `secondary` answers but never becomes PRIMARY. STUB_MIRROR=down refuses every pull from
# mirror.gcr.io. STUB_DAEMON_MIRROR is the daemon's own Docker Hub mirror (`docker info`), none when
# empty. A `login` records what it read on stdin in $STUB_LOGIN.
cat > "$stub_dir/docker" <<'STUB'
#!/usr/bin/env bash
printf '%s\n' "$*" >> "$STUB_LOG"
case "$1" in
  run|logs) exit 0 ;;
  pull) [ "${STUB_MIRROR:-up}" = down ] && [[ "$*" == *mirror.gcr.io* ]] && exit 1; exit 0 ;;
  info) [ -n "${STUB_DAEMON_MIRROR:-}" ] && echo "$STUB_DAEMON_MIRROR"; exit 0 ;;
  login) cat > "$STUB_LOGIN"; exit 0 ;;
esac
eval_arg="${*: -1}"
case "$STUB_MONGO:$eval_arg" in
  down:*) exit 1 ;;
  *:"rs.status().myState") [ "$STUB_MONGO" = healthy ] && echo 1 || echo 2 ;;
esac
exit 0
STUB
chmod +x "$stub_dir/docker"
export STUB_LOG="$stub_dir/log" STUB_LOGIN="$stub_dir/login"

# start <stub state> [args...] -> "<exit status>"
start() {
  export STUB_MONGO="$1"; shift
  : > "$STUB_LOG"; : > "$STUB_LOGIN"
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
check "mongod listens on the runner's own network, not behind docker-proxy" "1" \
  "$(start healthy >/dev/null; grep -c -- '^run -d --name mongo --network host mirror.gcr.io/library/mongo:' "$STUB_LOG")"
check "...publishing no port to relay" "0" "$(grep -c -- ' -p ' "$STUB_LOG")"
check "the image comes from Google's Docker Hub mirror, not Docker Hub's rate-limited anonymous pulls" "1" \
  "$(start healthy >/dev/null; grep -c -- ' mirror.gcr.io/library/mongo:[0-9.]* --replSet' "$STUB_LOG")"
check "...without logging in to Docker Hub while the mirror serves it" "0" "$(grep -c '^login' "$STUB_LOG")"
check "a mirror that refuses the pull falls back to Docker Hub" "0" \
  "$(STUB_MIRROR=down DOCKERHUB_USERNAME=ci-user DOCKERHUB_TOKEN=s3cret start healthy)"
check "...running Docker Hub's image" "1" "$(grep -c -- ' docker.io/library/mongo:[0-9.]* --replSet' "$STUB_LOG")"
check "...logged in as the CI account" "1" "$(grep -c -- '^login -u ci-user --password-stdin$' "$STUB_LOG")"
check "...the token handed over on stdin" "s3cret" "$(cat "$STUB_LOGIN")"
check "...and never on a command line" "0" "$(grep -c s3cret "$STUB_LOG")"
check "with no Docker Hub token the fallback still pulls, anonymously" "0" "$(STUB_MIRROR=down start healthy)"
check "...without a login" "0" "$(grep -c '^login' "$STUB_LOG")"
check "a daemon with a Docker Hub mirror of its own pulls Docker Hub's name, through that mirror" "1" \
  "$(STUB_DAEMON_MIRROR=http://forgejo.internal:3012/ start healthy >/dev/null; grep -c -- '^run .* docker.io/library/mongo:[0-9.]* --replSet' "$STUB_LOG")"
check "...trying no other registry first" "pull -q docker.io/library/mongo:8.3.11" "$(grep '^pull' "$STUB_LOG" | head -1)"
check "...without logging in to Docker Hub" "0" "$(grep -c '^login' "$STUB_LOG")"
check "the data directory is on disk by default" "0" "$(start healthy >/dev/null; grep -c -- '--tmpfs' "$STUB_LOG")"
check "...and on a RAM-backed tmpfs of the size asked for" "1" \
  "$(MONGO_TMPFS=6g start healthy >/dev/null; grep -c -- '^run -d --name mongo --network host --tmpfs /data/db:rw,size=6g mirror.gcr.io/library/mongo:' "$STUB_LOG")"
check "every probe keeps mongosh's state out of the data directory" "0" \
  "$(start healthy >/dev/null; grep '^exec ' "$STUB_LOG" | grep -vc '^exec -e HOME=/tmp mongo mongosh ')"
check "a mongod that never answers fails, bounded, instead of waiting forever" "1" "$(start down)"
check "...and says why" "1" "$(grep -c 'did not become reachable' "$stub_dir/out")"
check "a mongod that never becomes PRIMARY fails too" "1" "$(start secondary)"
check "...naming what it waited for" "1" "$(grep -c 'did not become PRIMARY' "$stub_dir/out")"

# NATIVE: the Forgejo job image's own mongod and mongosh (stubs here, answering as the container's do), started on the
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
check "...with its own log's tail" "1" "$(grep -c 'mongod: the log says why' "$stub_dir/out")"
check "MONGO_NO_NATIVE=1 runs the container regardless" "1" \
  "$(MONGO_NO_NATIVE=1 start healthy >/dev/null; grep -c '^run -d --name mongo ' "$STUB_LOG")"
check "...leaving no marker behind" "0" "$([ -e "$stub_dir/marker" ] && echo 1 || echo 0)"
NATIVE_HOME=""

spec_summary
