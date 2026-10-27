#!/usr/bin/env bash
# shell-spec.sh's own helpers: wait_until waits for its condition however long it takes to hold, and a deadline it
# reaches fails a case, named, with what it waited on -- never silently.
# Run: bash scripts/shell-spec-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m shell-spec\n'

work="$(cd "$(mktemp -d)" && pwd -P)"
trap 'rm -rf "$work"' EXIT

# a condition that holds only on its third look: counted, not timed
echo 0 > "$work/looks"
third_look() { local n; n=$(cat "$work/looks"); echo $((n + 1)) > "$work/looks"; [ "$n" -ge 2 ]; }
wait_until "the third look" third_look; status=$?
check "wait_until: looks again until its condition holds" "0 3" "$status $(cat "$work/looks")"

touch "$work/there"
wait_until "a file there" '[ -e "$work/there" ]'
check "wait_until: one string is evaluated in the caller's shell" "0" "$?"
wait_until "a pattern there" grep -q there <<< "it is there"
check "wait_until: a command with its arguments is run as given" "0" "$?"

# a deadline reached: a failed case counted and named, and the status for a caller that branches on it
before=$fails
out=$(SPEC_WAIT_BOUND=0 wait_until "a file never made" '[ -e "$work/never" ]'; echo "status=$? fails=$fails")
check "wait_until: a deadline reached is a failure, counted" "status=1 fails=$((before + 1))" "$(tail -1 <<< "$out")"
check "wait_until: ...and names what it waited for, and on what" "named on" \
  "$(grep -q 'a file never made: not within 0s' <<< "$out" && printf named) $(grep -q 'waited on: \[ -e' <<< "$out" && printf on)"

spec_summary
