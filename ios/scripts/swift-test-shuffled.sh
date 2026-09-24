#!/usr/bin/env bash
#
# Every `swift test` case ALONE, in an order drawn from a seed — the iOS lane
# of the nightly order-independence workflow. `swift test` has no shuffle and
# XCTest always runs a process's tests in name order, so a test that passes
# only because another ran first (or never ran first) in that process, or
# because of what an earlier process left on disk (UserDefaults suites, the
# caches directory), looks healthy forever. Running each case in its own
# process, in a seeded order, meets both. Reproduce a failure with the seed it
# prints:
#
#     ios/scripts/swift-test-shuffled.sh <seed>
#
set -uo pipefail
seed="${1:?usage: swift-test-shuffled.sh <seed>}"
package="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
. "$package/../scripts/seeded-order.sh"

swift build --package-path "$package" --build-tests || exit 1
tests="$(swift test list --package-path "$package" --skip-build)" || exit 1
[ -n "$tests" ] || { echo "swift-test-shuffled: no tests listed" >&2; exit 1; }

ordered="$(printf '%s\n' "$tests" | seeded_order "$seed")"
count="$(printf '%s\n' "$ordered" | wc -l | tr -d ' ')"
echo "swift-test-shuffled: seed $seed — $count cases, each alone — reproduce with ios/scripts/swift-test-shuffled.sh $seed"

log="$(mktemp -t swift-test-shuffled)"
failed=()
while IFS= read -r test; do
    pattern="^$(printf '%s' "$test" | sed 's/[.[\*^$()+?{}|]/\\&/g')\$"
    # Exit status alone is not a pass: `swift test` exits 0 when the filter matches
    # nothing ("No matching test cases were run"), so a case the pattern misses
    # would pass without running. XCTest's last "Executed N test(s)" line is the
    # whole run's tally; it must say exactly one.
    swift test --package-path "$package" --skip-build --filter "$pattern" >"$log" 2>&1
    status=$?
    executed="$(grep -oE 'Executed [0-9]+ tests?' "$log" | tail -1 | grep -oE '[0-9]+')"
    if [ "$status" -ne 0 ] || [ "$executed" != 1 ]; then
        [ "$status" -eq 0 ] && echo "swift-test-shuffled: expected exactly 1 test to run, saw ${executed:-none}"
        echo "FAILED alone: $test"
        tail -40 "$log"
        failed+=("$test")
    fi
done <<<"$ordered"
rm -f "$log"

if [ "${#failed[@]}" -gt 0 ]; then
    echo "swift-test-shuffled: ${#failed[@]} of $count cases fail alone under seed $seed:"
    printf '  %s\n' "${failed[@]}"
    exit 1
fi
echo "swift-test-shuffled: all $count cases pass alone under seed $seed"
