#!/usr/bin/env bash
#
# swift-test-shuffled.sh against a shim `swift`, so it runs in a second with no
# toolchain. The promise under test: a case counts as passed only when its own
# `swift test --filter` run shows exactly ONE test executed. `swift test` exits 0
# when a filter matches nothing ("No matching test cases were run"), so exit
# status alone would pass a case that was never run -- a renamed test, or a name
# the regex escaping mangles, would look green forever.
#
#   ios/scripts/swift-test-shuffled-test.sh
#
set -uo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
. "$HERE/../../scripts/shell-spec.sh"

printf '\033[36m▸\033[0m swift-test-shuffled.sh\n'

shim="$(mktemp -d)"
trap 'rm -rf "$shim"' EXIT
# `swift build` and `swift test list` answer from SHIM_TESTS; `swift test
# --filter <pattern>` reports by the test the pattern names: *Ghost* matches
# nothing, *Twice* runs two, *Broken* fails, anything else runs once and passes.
cat >"$shim/swift" <<'SHIM'
#!/usr/bin/env bash
case "$1 ${2:-}" in
  "build "*) exit 0 ;;
  "test list") printf '%s\n' $SHIM_TESTS; exit 0 ;;
esac
pattern=""
while [ $# -gt 0 ]; do [ "$1" = --filter ] && pattern="$2"; shift; done
# A test process may read stdin; SHIM_READS_STDIN makes every run drain it.
[ -n "${SHIM_READS_STDIN:-}" ] && cat >/dev/null
[ -n "${SHIM_LOG:-}" ] && echo "$pattern" >> "$SHIM_LOG"
case "$pattern" in
  *Ghost*)  echo "warning: No matching test cases were run"; exit 0 ;;
  *Twice*)  echo "	 Executed 2 tests, with 0 failures (0 unexpected) in 0.001 (0.002) seconds"; exit 0 ;;
  *Broken*) echo "	 Executed 1 test, with 1 failure (0 unexpected) in 0.001 (0.002) seconds"; exit 1 ;;
  *)        echo "	 Executed 1 test, with 0 failures (0 unexpected) in 0.001 (0.002) seconds"; exit 0 ;;
esac
SHIM
chmod +x "$shim/swift"

run() { SHIM_TESTS="$*" PATH="$shim:$PATH" "$HERE/swift-test-shuffled.sh" 7 2>&1; }

out="$(run A.T/testOne A.T/testTwo)"; code=$?
check "every case running once and passing is a pass" "0" "$code"
check "...and says so" "1" "$(printf '%s\n' "$out" | grep -c 'all 2 cases pass alone')"

out="$(run A.T/testOne A.T/testGhost)"; code=$?
check "a filter that matched nothing fails the run, though swift test exited 0" "1" "$code"
check "...naming the case that never ran" "1" "$(printf '%s\n' "$out" | grep -c '^FAILED alone: A.T/testGhost$')"

out="$(run A.T/testTwice)"; code=$?
check "a filter that ran more than the one case fails the run" "1" "$code"

out="$(run A.T/testBroken)"; code=$?
check "a case that fails alone still fails the run" "1" "$code"

: > "$shim/log"
SHIM_READS_STDIN=1 SHIM_LOG="$shim/log" run A.T/testOne A.T/testTwo A.T/testThree >/dev/null
check "a case that reads stdin cannot swallow the cases still to run" "3" "$(wc -l < "$shim/log" | tr -d ' ')"

spec_summary
