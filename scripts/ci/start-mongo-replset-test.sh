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
# mirror.gcr.io. A `login` records what it read on stdin in $STUB_LOGIN.
cat > "$stub_dir/docker" <<'STUB'
#!/usr/bin/env bash
printf '%s\n' "$*" >> "$STUB_LOG"
case "$1" in
  run|logs) exit 0 ;;
  pull) [ "${STUB_MIRROR:-up}" = down ] && [[ "$*" == *mirror.gcr.io* ]] && exit 1; exit 0 ;;
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
  PATH="$stub_dir:$PATH" MONGO_START_TIMEOUT_SECONDS=2 \
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
check "the data directory is on disk by default" "0" "$(start healthy >/dev/null; grep -c -- '--tmpfs' "$STUB_LOG")"
check "...and on a RAM-backed tmpfs of the size asked for" "1" \
  "$(MONGO_TMPFS=6g start healthy >/dev/null; grep -c -- '^run -d --name mongo --network host --tmpfs /data/db:rw,size=6g mirror.gcr.io/library/mongo:' "$STUB_LOG")"
check "every probe keeps mongosh's state out of the data directory" "0" \
  "$(start healthy >/dev/null; grep '^exec ' "$STUB_LOG" | grep -vc '^exec -e HOME=/tmp mongo mongosh ')"
check "a mongod that never answers fails, bounded, instead of waiting forever" "1" "$(start down)"
check "...and says why" "1" "$(grep -c 'did not become reachable' "$stub_dir/out")"
check "a mongod that never becomes PRIMARY fails too" "1" "$(start secondary)"
check "...naming what it waited for" "1" "$(grep -c 'did not become PRIMARY' "$stub_dir/out")"

spec_summary
