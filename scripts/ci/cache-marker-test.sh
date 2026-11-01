#!/usr/bin/env bash
# cache-marker.sh: hit, partial and miss, the restore's time and size, and exit 0 on anything.
# Run: bash scripts/ci/cache-marker-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m cache-marker.sh\n'
script="$REPO_ROOT/scripts/ci/cache-marker.sh"
dir="$(mktemp -d)"; trap 'rm -rf "$dir"' EXIT
export RUNNER_TEMP="$dir" CI_METRICS_FILE="$dir/m.jsonl"
cd "$dir" || exit 1
last() { tail -1 "$dir/m.jsonl" | python3 -c 'import json,sys; d=json.load(sys.stdin); print(d["status"], d["size_mb"], d["restore_s"] is not None)'; }

"$script" begin t
check "an empty tree is a miss" "ci-cache t miss k1" "$("$script" end t "" k1 "$dir/target/scala-*")"
check "...with its size and restore time" "miss 0 True" "$(last)"
mkdir -p target/scala-3; head -c 300000 /dev/zero > target/scala-3/x
"$script" begin t
check "files without an exact hit: a restore key matched" "ci-cache t partial k2" "$("$script" end t false k2 "$dir/target/scala-*")"
check "an exact hit" "ci-cache t hit k3" "$("$script" end t true k3 "$dir/target/scala-*")"
check "a '!' exclusion is not measured" "hit" "$("$script" end t true k4 '!target/x' > /dev/null; tail -1 m.jsonl | python3 -c 'import json,sys; print(json.load(sys.stdin)["status"])')"
check "no arguments still exit 0" "0" "$("$script" > /dev/null 2>&1; echo $?)"

spec_summary
