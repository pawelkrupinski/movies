#!/usr/bin/env bash
# mongo-top.sh against a stub `docker` that logs every call.
# Run: bash scripts/ci/mongo-top-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m mongo-top.sh\n'

stub_dir="$(mktemp -d)"
trap 'rm -rf "$stub_dir"' EXIT
# STUB_FAILS: the exec fails, as one against a container that is gone does.
cat > "$stub_dir/docker" <<'STUB'
#!/usr/bin/env bash
printf '%s\n' "$*" >> "$STUB_LOG"
[ -z "${STUB_FAILS:-}" ] || exit 1
exit 0
STUB
chmod +x "$stub_dir/docker"
export STUB_LOG="$stub_dir/log"

# top <args...> -> "<exit status>"; the output in $stub_dir/out
top() {
  : > "$STUB_LOG"
  PATH="$stub_dir:$PATH" bash "$REPO_ROOT/scripts/ci/mongo-top.sh" "$@" > "$stub_dir/out" 2>&1
  echo "$?"
}

check "sampling starts in the background, inside the mongo container" "0:1" \
  "$(status=$(top start 5); echo "$status:$(grep -c '^exec -d -e HOME=/tmp mongo mongosh ' "$STUB_LOG")")"
check "...every 5 seconds when asked" "1" "$(grep -c 'sleep(5 \* 1000)' "$STUB_LOG")"
check "the report asks the same container" "0:1" \
  "$(status=$(top report 3); echo "$status:$(grep -c '^exec -e HOME=/tmp mongo mongosh ' "$STUB_LOG")")"
# The image's HOME is the data directory: a mongosh run there writes its state into the database's own files.
check "every mongosh keeps its state out of the data directory" "0" \
  "$(top start >/dev/null; top report >/dev/null; grep '^exec ' "$STUB_LOG" | grep -vc -- ' -e HOME=/tmp mongo mongosh ')"
check "a report with no samples says so, and succeeds" "0:1" \
  "$(status=$(STUB_FAILS=1 top report); echo "$status:$(grep -c 'no samples' "$stub_dir/out")")"
# A native mongod's marker (mongo-exec.sh): the samples go under the runner's own temp directory, not a /tmp name every
# runner of the host shares, and `report` reads the same file `start` wrote.
runner_temp="$stub_dir/runner"; mkdir -p "$runner_temp"
printf '%s\n' "$stub_dir/native" > "$runner_temp/kinowo-mongo-native"
mkdir -p "$stub_dir/native/bin"
printf '#!/bin/sh\nprintf "%%s\\n" "$*" >> "$STUB_LOG"\n' > "$stub_dir/native/bin/mongosh"; chmod +x "$stub_dir/native/bin/mongosh"
# A native `start` runs detached (nohup ... &), so its stub's write lands a moment after the call returns.
native_top() { : > "$STUB_LOG"; RUNNER_TEMP="$runner_temp" top "$@" >/dev/null; for _ in $(seq 1 50); do [ -s "$STUB_LOG" ] && return; sleep 0.1; done; }
check "beside a native mongod, start keeps its samples under the runner's temp directory" "1" \
  "$(native_top start 5; grep -c "writeFileSync('$runner_temp/mongo-top.json'" "$STUB_LOG")"
check "...and report reads that same file" "1" \
  "$(native_top report 3; grep -c "readFileSync('$runner_temp/mongo-top.json'" "$STUB_LOG")"
check "...and the container's samples stay at its own /tmp" "1" \
  "$(top start 5 >/dev/null; grep -c "writeFileSync('/tmp/mongo-top.json'" "$STUB_LOG")"
check "an unknown command is a usage error" "64" "$(top bogus)"

spec_summary
