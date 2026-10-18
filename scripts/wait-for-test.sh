#!/usr/bin/env bash
# scripts/wait-for: a line matching appears (in a file that may not exist yet) -> printed, exit 0; the writer ends with
# none -> exit 1 with its tail; nothing by the deadline -> exit 2; a usage error -> exit 2.
# Run: bash scripts/wait-for-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m wait-for\n'
work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
wf="$REPO_ROOT/scripts/wait-for"

( sleep 0.3; echo "compiling"; echo "exit=0 done" ) > "$work/late.log" &
out=$("$wf" "$work/late.log" '^exit=' --poll 0.1 --timeout 10); status=$?
check "a line that appears later: printed once it matches, exit 0" "0 exit=0 done" "$status $out"

( sleep 0.3; echo "LANDABLE (local)" > "$work/missing.log" ) &
out=$("$wf" "$work/missing.log" 'LANDABLE' --poll 0.1 --timeout 10); status=$?
check "a file that does not exist yet: waited for, then matched" "0 LANDABLE (local)" "$status $out"

( echo "[error] compile failed"; sleep 0.2 ) > "$work/dead.log" &
writer=$!
"$wf" "$work/dead.log" '^exit=' --pid "$writer" --poll 0.1 --timeout 10 > /dev/null 2> "$work/err"; status=$?
check "its writer ended with no match: exit 1, the file's tail shown" "1 yes" \
  "$status $(grep -q 'compile failed' "$work/err" && echo yes || echo no)"

: > "$work/never.log"
"$wf" "$work/never.log" 'x' --poll 0.1 --timeout 0 2> "$work/err"; status=$?
check "nothing by the deadline: exit 2, said" "2 yes" "$status $(grep -q 'after 0s' "$work/err" && echo yes || echo no)"

"$wf" "$work/never.log" > /dev/null 2>&1; status=$?
check "no pattern: a usage error, exit 2" "2" "$status"

"$wf" "$work/never.log" x --timeout > /dev/null 2> "$work/err"; status=$?
check "a flag with no value: a usage error, exit 2 -- never 1, which says the writer ended" "2 yes" \
  "$status $(grep -q -- '--timeout needs a value' "$work/err" && echo yes || echo no)"
"$wf" "$work/never.log" x --poll '' > /dev/null 2>&1; status=$?
check "...an empty one too" "2" "$status"

spec_summary
