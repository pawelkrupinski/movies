#!/usr/bin/env bash
# in-background.sh: a chore started in one step, its log and exit status collected in another.
# Run: bash scripts/ci/in-background-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m in-background.sh\n'

work="$(mktemp -d)"
# the never-ending chore below is killed on the way out whatever happened, so a failing case strands nothing
trap 'kill "$(cat "$work/hung" 2>/dev/null)" 2>/dev/null; rm -rf "$work"' EXIT
export BACKGROUND_DIR="$work"
# Every call under shell-spec.sh's outer bound: a start or wait that broke into waiting on its chore fails the case,
# never holds the spec (a broken start once ran the never-ending chore below in the foreground for hours).
bg() { spec_bounded bash "$REPO_ROOT/scripts/ci/in-background.sh" "$@"; }

# The chore cannot end until it is let go, so start returning at all proves it did not run the chore in the
# foreground -- and the chore not finished once start returns says so again. Proven so, never by elapsed wall time,
# which a loaded machine stretches ~10x (shell-spec.sh's HUNG STUBS); the bound only fails a start that hangs.
bg start slow bash -c 'until [ -e "$0" ]; do sleep 0.05; done; echo "pulled the image"; exit 3' "$work/let-go"
check "start returns while its chore still runs, rather than running it in the foreground" "0 running" \
  "$? $([ -e "$work/slow.rc" ] && echo finished || echo running)"
touch "$work/let-go"
out="$(bg wait slow 600)"; status=$?
check "wait exits with the chore's own status" "3" "$status"
check "...after printing its log" "pulled the image" "$out"

bg start quick true
bg wait quick 30 > /dev/null
check "a chore that succeeded waits green" "0" "$?"

out="$(bg wait never-started 30)"; status=$?
check "a chore nobody started is reported at once, so the caller can do it itself" "2" "$status"

# A chore that never ends on its own, waited on once it is half way: wait returns only by its deadline, while the
# chore runs on.
bg start hung bash -c 'echo "half way"; echo $$ > "$0"; exec sleep 86400' "$work/hung"
wait_until "the hung chore wrote its pid" '[ -s "$work/hung" ]'
out="$(bg wait hung 1)"; status=$?
check "a chore past its deadline fails the wait" "1" "$status"
check "...on the deadline, not the chore's own end: it is still running" "true" \
  "$(kill -0 "$(cat "$work/hung" 2>/dev/null)" 2>/dev/null && echo true || echo false)"
check "...and shows how far it got" "true" "$(grep -q 'half way' <<<"$out" && echo true || echo false)"

spec_summary
