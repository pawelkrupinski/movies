#!/usr/bin/env bash
# in-background.sh: a chore started in one step, its log and exit status collected in another.
# Run: bash scripts/ci/in-background-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m in-background.sh\n'

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
export BACKGROUND_DIR="$work"
bg="$REPO_ROOT/scripts/ci/in-background.sh"

started=$SECONDS
bash "$bg" start slow bash -c 'sleep 2; echo "pulled the image"; exit 3'
check "start returns at once rather than running the chore in the foreground" "true" \
  "$([ $((SECONDS - started)) -lt 2 ] && echo true || echo false)"
out="$(bash "$bg" wait slow 30)"; status=$?
check "wait exits with the chore's own status" "3" "$status"
check "...after printing its log" "pulled the image" "$out"

bash "$bg" start quick true
bash "$bg" wait quick 30 > /dev/null
check "a chore that succeeded waits green" "0" "$?"

out="$(bash "$bg" wait never-started 30)"; status=$?
check "a chore nobody started is reported at once, so the caller can do it itself" "2" "$status"

bash "$bg" start hung bash -c 'echo "half way"; sleep 30'
started=$SECONDS
out="$(bash "$bg" wait hung 1)"; status=$?
check "a chore past its deadline fails the wait" "1" "$status"
check "...within the deadline, not the chore's own time" "true" \
  "$([ $((SECONDS - started)) -lt 10 ] && echo true || echo false)"
check "...and shows how far it got" "true" "$(printf '%s' "$out" | grep -q 'half way' && echo true || echo false)"
pkill -f 'echo "half way"; sleep 30' 2>/dev/null

spec_summary
