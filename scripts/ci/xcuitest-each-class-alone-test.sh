#!/usr/bin/env bash
# xcuitest-each-class-alone.sh with a stub xcodebuild and a stub xcrun (the failed bundle's test tree), so it runs
# anywhere. The promise: every XCUITest class runs in its own xcodebuild, even after one fails; the step then fails
# naming the classes; and a failed test's first error is its own assertion, at its file and line -- the line its red
# is known by (Order independence #28-#31 failed in different classes and read as one red).
set -uo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
. "$HERE/../shell-spec.sh"
printf '\033[36m▸\033[0m xcuitest-each-class-alone.sh\n'

tmp="$(mktemp -d)"; trap 'rm -rf "$tmp"' EXIT
mkdir -p "$tmp/tests" "$tmp/bin" "$tmp/work" "$tmp/none"
cat >"$tmp/tests/AlphaUITests.swift" <<'SWIFT'
final class AlphaUITests: XCTestCase {}
SWIFT
cat >"$tmp/tests/Shared.swift" <<'SWIFT'
@MainActor
final class GammaUITests : XCTestCase {}
final class BetaUITests: XCTestCase {}
enum FixtureLaunch {}
SWIFT

# xcodebuild: records each run's class and arguments, prints a line to the class's log, fails $FAILING.
cat >"$tmp/bin/xcodebuild" <<'SH'
#!/usr/bin/env bash
for arg in "$@"; do case "$arg" in -only-testing:*) class=${arg##*/} ;; esac; done
echo "$class" >>"$CALLS"
echo "xcodebuild $class" >>"$CALLS.order"
echo "$*" >>"$CALLS.args"
echo "error: -[KinowoUITests.$class testLaunches] : xcodebuild's own report for $class"
[ "$class" != "${FAILING:-}" ]
SH
# xcrun xcresulttool get test-results tests --path <bundle>: the failed class's tree, its file inside the checkout.
# xcrun simctl <...>: the warm-up, recorded in order with the classes; fails when $WARMUP_FAILS.
cat >"$tmp/bin/xcrun" <<'SH'
#!/usr/bin/env bash
if [ "$1" = simctl ]; then echo "$*" >>"$CALLS.order"; [ -z "${WARMUP_FAILS:-}" ]; exit; fi
cat "$TREE"
SH
chmod +x "$tmp/bin/xcodebuild" "$tmp/bin/xcrun"
cat >"$tmp/beta.json" <<JSON
{"testNodes": [{"name": "Kinowo", "nodeType": "Test Plan", "result": "Failed", "children": [
 {"name": "KinowoUITests", "nodeType": "UI test bundle", "result": "Failed", "children": [
  {"name": "BetaUITests", "nodeType": "Test Suite", "result": "Failed", "children": [
   {"name": "testLaunches()", "nodeType": "Test Case", "result": "Failed", "children": [
    {"name": "XCTAssertTrue failed - Expected at least one film card after launch", "nodeType": "Failure Message",
     "sourceLocation": {"filePath": "$tmp/work/ios/KinowoUITests/BetaUITests.swift", "lineNumber": 25}}]}]}]}]}]}
JSON

alone() { # alone <failing class or ""> <tests dir>: runs the script in $tmp/work; sets out and code
  : >"$tmp/calls"; : >"$tmp/calls.args"; : >"$tmp/calls.order"; : >"$tmp/summary"
  out=$(cd "$tmp/work" && PATH="$tmp/bin:$PATH" CALLS="$tmp/calls" FAILING="$1" TREE="$tmp/beta.json" \
    DESTINATION="platform=iOS Simulator,id=SIM" XCUITEST_DIR="$2" GITHUB_STEP_SUMMARY="$tmp/summary" \
    "$HERE/xcuitest-each-class-alone.sh" 2>&1)
  code=$?
}

alone BetaUITests "$tmp/tests"
check "every class runs, each in its own xcodebuild, after one failed" "AlphaUITests BetaUITests GammaUITests" \
  "$(tr '\n' ' ' <"$tmp/calls" | sed 's/ $//')"
check "each run is that class's only, into its own bundle, on the destination, every test bounded" "1" \
  "$(grep -c -- "-destination platform=iOS Simulator,id=SIM .*-only-testing:KinowoUITests/BetaUITests -resultBundlePath build/alone/BetaUITests.xcresult -test-timeouts-enabled YES" "$tmp/calls.args")"
check "a failed class fails the step" "1" "$code"
check "the first error is the failed test's own assertion, at its file and line" \
  "::error file=ios/KinowoUITests/BetaUITests.swift,line=25,title=BetaUITests.testLaunches::BetaUITests.testLaunches: XCTAssertTrue failed - Expected at least one film card after launch" \
  "$(printf '%s\n' "$out" | grep -m1 -e '::error' -e 'error:')"
check "the failed class's own log follows" "1" "$(printf '%s\n' "$out" | grep -c "xcodebuild's own report for BetaUITests")"
check "a passing class's log stays in its file" "0" "$(printf '%s\n' "$out" | grep -c "report for AlphaUITests")"
check "the step's last word names the failed classes" "::error::XCUITest classes that fail when run alone: BetaUITests" \
  "$(printf '%s\n' "$out" | tail -1)"
check "and so does the job summary" "XCUITest classes that fail when run alone: BetaUITests" "$(cat "$tmp/summary")"

alone "" "$tmp/tests"
check "all passing: the step passes" "0" "$code"
check "the built app is installed and launched once on the destination before the first class, then ended" \
  "simctl install SIM build/dd/Build/Products/Debug-iphonesimulator/Kinowo.app|simctl launch SIM dev.kinowo.Kinowo|simctl terminate SIM dev.kinowo.Kinowo|xcodebuild AlphaUITests" \
  "$(head -4 "$tmp/calls.order" | paste -sd '|' -)"
WARMUP_FAILS=1 alone "" "$tmp/tests"
check "a failed warm-up only warns: every class still runs and the step passes" "0 3 1" \
  "$code $(grep -c '^xcodebuild ' "$tmp/calls.order") $(printf '%s\n' "$out" | grep -c '::warning::the Simulator warm-up launch')"
check "all passing: no error at all" "0" "$(printf '%s\n' "$out" | grep -c '::error')"

alone "" "$tmp/none"
check "no XCUITest class found fails the step" "1" "$code"
check "naming where it looked" "::error::no XCUITest classes found in $tmp/none" "$(printf '%s\n' "$out" | tail -1)"

spec_summary
