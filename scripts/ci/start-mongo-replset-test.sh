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
# a ping; `secondary` answers but never becomes PRIMARY.
cat > "$stub_dir/docker" <<'STUB'
#!/usr/bin/env bash
printf '%s\n' "$*" >> "$STUB_LOG"
case "$1" in
  run|logs) exit 0 ;;
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
  : > "$STUB_LOG"
  PATH="$stub_dir:$PATH" MONGO_START_TIMEOUT_SECONDS=2 \
    bash "$REPO_ROOT/scripts/ci/start-mongo-replset.sh" "$@" > "$stub_dir/out" 2>&1
  echo "$?"
}

check "a healthy mongod becomes PRIMARY and the script succeeds" "0" "$(start healthy)"
check "...having initiated the replica set once" "1" "$(grep -c 'rs.initiate' "$STUB_LOG")"
check "extra mongod args reach docker run" "1" \
  "$(start healthy --wiredTigerCacheSizeGB 2 >/dev/null; grep -c '^run .*--replSet rs0 --wiredTigerCacheSizeGB 2$' "$STUB_LOG")"
check "a mongod that never answers fails, bounded, instead of waiting forever" "1" "$(start down)"
check "...and says why" "1" "$(grep -c 'did not become reachable' "$stub_dir/out")"
check "a mongod that never becomes PRIMARY fails too" "1" "$(start secondary)"
check "...naming what it waited for" "1" "$(grep -c 'did not become PRIMARY' "$stub_dir/out")"

spec_summary
