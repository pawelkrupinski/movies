#!/usr/bin/env bash
# stall-watch.sh ended the moment it starts, as run-script-tests ends a quick test's watchdog: nothing on its stderr
# (which run-script-tests appends to the test's log) and no stall-mark.* left in the temp directory. A TERM that came
# while the watchdog was still in `$(mktemp)` ended it mid-substitution: "mktemp: failed to print directory name: Broken
# pipe" in a test's log (Tool self-tests 41193766567 on github.com) and a mark left behind. SIGPIPE is ignored here, as
# on GitHub's runner, so mktemp reports the broken pipe instead of dying of it.
# Run: bash scripts/stall-watch-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m stall-watch.sh\n'

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
mkdir "$work/tmp"
: > "$work/err"
trap '' PIPE
sleep 60 & root=$!
# killed 0-9 ms after it starts, 150 times: the race window is the few ms from bash's start to the mark made
for i in $(seq 1 150); do
  TMPDIR="$work/tmp" bash "$REPO_ROOT/scripts/stall-watch.sh" watch "$root" 60 x "$work/stalled" 2>> "$work/err" & w=$!
  sleep "0.00$((i % 10))"
  kill "$w" 2> /dev/null; wait "$w" 2> /dev/null
done
kill "$root"; wait "$root" 2> /dev/null

check "a watchdog ended at once says nothing" "" "$(head -3 "$work/err")"
check "...and leaves no stall mark" "0" "$(find "$work/tmp" -name 'stall-mark.*' | wc -l | tr -d ' ')"
check "...nor calls anything stalled" "no" "$([ -s "$work/stalled" ] && echo yes || echo no)"

spec_summary
