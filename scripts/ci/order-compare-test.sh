#!/usr/bin/env bash
# order-compare.sh: the Order independence verdict. Each leg runs through `leg`; on a failure its failing tests run
# again in default order (the control pass) and the leg is labelled REGRESSION (they fail there too) or ORDER (they
# pass there), never turned green; a leg failing naming no test has no verdict. Plus the sbt / JUnit / XCTest readers,
# scala's reverse vs shuffled comparison, the run's one-word verdict and its JSON, and the workflow's wiring of it.
# Run: bash scripts/ci/order-compare-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"
printf '\033[36m▸\033[0m order-compare.sh\n'
tool="$REPO_ROOT/scripts/ci/order-compare.sh"
work="$(mktemp -d)"; trap 'rm -rf "$work"' EXIT

# -- the readers
# an sbt log's tail as testUnitNoE2e prints it (a module's block each, coloured on a terminal)
printf '%s\n' '[info] Passed: Total 10, Failed 1' \
  $'\e[31m[error]\e[0m Failed tests:' $'[error] \tservices.movies.MovieServiceSpec' $'[error] \tclients.TmdbClientSpec' \
  '[error] Error during tests:' $'[error] \tservices.identity.ResolverSpec' \
  '[error] (web / Test / testOnly) sbt.TestsFailedException: Tests unsuccessful' \
  '[error] Failed tests:' $'[error] \tservices.movies.MovieServiceSpec' '[error] Total time: 12 s' > "$work/sbt.log"
check "failed-suites: the suites under Failed tests: and Error during tests:, once each, sorted, nothing else" \
  "clients.TmdbClientSpec services.identity.ResolverSpec services.movies.MovieServiceSpec" \
  "$("$tool" failed-suites "$work/sbt.log" | paste -sd' ' -)"
# Order independence's own log, as GitHub keeps it (#100: PopulationMonotonicityPart2Spec failed in both orders)
printf '%s\n' $'\e[0m[\e[0m\e[31merror\e[0m] \e[0m\e[0mFailed tests:\e[0m' \
  $'\e[0m[\e[0m\e[31merror\e[0m] \e[0m\e[0m\tservices.identity.PopulationMonotonicityPart2Spec\e[0m' \
  $'\e[0m[\e[0m\e[31merror\e[0m] \e[0m\e[0m(worker / Test / \e[31mtest\e[0m) sbt.TestsFailedException: Tests unsuccessful\e[0m' \
  $'\e[0J' > "$work/ci.log"
check "...CI's coloured log alike" "services.identity.PopulationMonotonicityPart2Spec" "$("$tool" failed-suites "$work/ci.log")"
check "...a log with none, or no log at all: none" "|" \
  "$(printf '[info] all passed\n[error] Total time: 3 s\n' > "$work/green.log"; "$tool" failed-suites "$work/green.log")|$("$tool" failed-suites "$work/none.log")"

mkdir -p "$work/junit"
cat > "$work/junit/TEST-a.xml" <<'XML'
<?xml version="1.0" encoding="UTF-8"?>
<testsuite name="pl.kinowo.order.ShuffledOrderSuite" tests="3" failures="1" errors="1">
  <testcase name="passes" classname="pl.kinowo.PassTest" time="0.1"/>
  <testcase name="nullUntilSet" classname="pl.kinowo.prefs.PrefsTest" time="0.2">
    <failure message="expected null" type="java.lang.AssertionError">java.lang.AssertionError: expected null
	at pl.kinowo.prefs.PrefsTest.nullUntilSet(PrefsTest.kt:12)</failure>
  </testcase>
  <testcase name="boom" classname="pl.kinowo.sync.SyncTest$Nested" time="0.0">
    <error message="boom" type="java.lang.IllegalStateException">boom</error>
  </testcase>
  <testcase name="passesToo" classname="pl.kinowo.PassTest" time="0.1"></testcase>
</testsuite>
XML
check "failed-junit: the class of each failed or errored case, once, sorted; a passing case's class never" \
  'pl.kinowo.prefs.PrefsTest pl.kinowo.sync.SyncTest$Nested' "$("$tool" failed-junit "$work/junit" | paste -sd' ' -)"
check "...no reports at all: none" "" "$("$tool" failed-junit "$work/no-such-dir")"

printf '%s\n' "Test Case '-[KinowoCoreTests.CityTests testA]' started." "Test Case '-[KinowoCoreTests.CityTests testA]' failed (0.001 seconds)." \
  "Test Case '-[KinowoUITests.CardShareMenuUITests testLongPress]' failed (55.224 seconds)." \
  "Test Case '-[KinowoCoreTests.CityTests testB]' passed (0.001 seconds)." > "$work/xctest.log"
check "failed-xctest: each failed case as <Module>.<Class>/<test>, a passed one never" \
  "KinowoCoreTests.CityTests/testA KinowoUITests.CardShareMenuUITests/testLongPress" "$("$tool" failed-xctest "$work/xctest.log" | paste -sd' ' -)"

# -- the default-order command each leg's control pass runs: exactly the failing tests, no order seed
check "control-command scala: those suites by testOnly, TEST_ORDER_SEED unset (default order)" \
  "env -u TEST_ORDER_SEED sbt testOnly a.ASpec b.BSpec" "$("$tool" control-command scala-shuffled a.ASpec b.BSpec | paste -sd' ' -)"
check "control-command android: those classes by --tests, offline, no -PtestOrderSeed" \
  "--offline testDebugUnitTest --tests p.ATest --tests p.BTest|0" \
  "$("$tool" control-command android p.ATest p.BTest | paste -sd' ' - | sed 's/.*--no-daemon //')|$("$tool" control-command android p.ATest | grep -c testOrderSeed)"
check "...with the leg's other settings (ORDER_CONTROL_EXTRA), so only the order differs" "1" \
  "$(ORDER_CONTROL_EXTRA=-PsyncModelSeeds=20000 "$tool" control-command android p.ATest | grep -c '^-PsyncModelSeeds=20000$')"
check "control-command swift: one process, the cases' anchored alternation (XCTest's own name order)" \
  'swift test --skip-build --filter ^(M\.A/testX|M\.B/testY)$' \
  "$("$tool" control-command swift M.A/testX M.B/testY | grep -v -- '--package-path' | grep -v '/ios$' | paste -sd' ' -)"
check "control-command ios: one xcodebuild run, -only-testing each class" \
  "-only-testing:KinowoUITests/AUITests -only-testing:KinowoUITests/BUITests" \
  "$("$tool" control-command ios AUITests BUITests | grep -- -only-testing | paste -sd' ' -)"
check "control-command: an unknown leg is a usage error" "64" "$("$tool" control-command nope x > /dev/null 2>&1; echo $?)"

# -- leg: the leg's command, then on a failure the control pass. A fake leg command writes the tests it failed to
# $ORDER_FAILED_FILE (as the each-alone scripts do); a fake control prints XCTest's failure lines for those it fails.
cat > "$work/fake-leg" <<'EOF'
#!/usr/bin/env bash
echo "running the leg"
[ -z "${FAKE_FAILED:-}" ] || printf '%s\n' $FAKE_FAILED > "$ORDER_FAILED_FILE"
exit "${FAKE_EXIT:-0}"
EOF
cat > "$work/fake-control" <<'EOF'
#!/usr/bin/env bash
shift # the leg
echo "control ran: $*" >> "$FAKE_CONTROL_CALLS"
for t in $FAKE_STILL_FAILING; do echo "Test Case '-[${t%%/*} ${t#*/}]' failed (0.1 seconds)."; done
[ -z "$FAKE_STILL_FAILING" ]
EOF
chmod +x "$work/fake-leg" "$work/fake-control"
run_leg() { # <dir> <failed> <exit> <still failing in default order>: the leg's exit, stdout in $work/out
  rm -rf "$1"; : > "$work/calls"; : > "$work/github-output"; : > "$work/summary"
  FAKE_FAILED="$2" FAKE_EXIT="$3" FAKE_STILL_FAILING="$4" FAKE_CONTROL_CALLS="$work/calls" ORDER_CONTROL_CMD="$work/fake-control" \
    GITHUB_OUTPUT="$work/github-output" GITHUB_STEP_SUMMARY="$work/summary" ORDER_REPLAY="ios/scripts/swift-test-shuffled.sh 42" \
    "$tool" leg swift "$1" -- "$work/fake-leg" > "$work/out" 2>&1
  echo $?
}
check "leg, a failure some of whose tests fail in default order too: REGRESSION, and the leg's own exit kept" \
  "3|regression|M.A/testX|order-verdict=regression" \
  "$(run_leg "$work/r1" 'M.A/testX M.B/testY' 3 'M.A/testX')|$(cat "$work/r1/swift.verdict")|$(cat "$work/r1/swift.control.txt")|$(cat "$work/github-output")"
check "...the control pass ran exactly the failing tests, once" "control ran: M.A/testX M.B/testY" "$(cat "$work/calls")"
check "...its annotation names what fails in default order, as agentctl reads it" "1" \
  "$(grep -c '^::error title=Order verdict (swift)::REGRESSION, not an order dependence: `M.A/testX` fail in default order too' "$work/out")"
check "...and the step summary says REGRESSION" "1" "$(grep -c '^### Order verdict (swift): REGRESSION' "$work/summary")"
check "leg, a failure whose tests all pass in default order: ORDER (or a flake), still red, with the replay" \
  "1|order|0|1" \
  "$(run_leg "$work/r2" 'M.B/testY' 1 '')|$(cat "$work/r2/swift.verdict")|$(grep -c . "$work/r2/swift.control.txt")|$(grep -c '::error title=Order verdict (swift)::ORDER DEPENDENCE (or a flake): `M.B/testY` fail in the swift order but pass in default order -- replay: ios/scripts/swift-test-shuffled.sh 42' "$work/out")"
check "leg, a failure naming no test: no verdict, no control pass, still red" \
  "2|none||1" \
  "$(run_leg "$work/r3" '' 2 '')|$(cat "$work/r3/swift.verdict")|$(cat "$work/calls")|$(grep -c '::warning title=Order verdict (swift)::NO ORDER VERDICT' "$work/out")"
check "leg, a pass: pass, no control pass, exit 0, its output kept in the log" \
  "0|pass|success||1" \
  "$(run_leg "$work/r4" '' 0 '')|$(cat "$work/r4/swift.verdict")|$(cat "$work/r4/swift.result")|$(cat "$work/calls")|$(grep -c 'running the leg' "$work/r4/swift.log")"
check "leg: an unknown leg is a usage error" "64" "$("$tool" leg nope "$work/x" -- true > /dev/null 2>&1; echo $?)"

# scala's failing suites come from its own log; android's from the JUnit reports the leg left
cat > "$work/fake-sbt-leg" <<'EOF'
#!/usr/bin/env bash
printf '%s\n' '[error] Failed tests:' $'[error] \ta.ASpec' $'[error] \tb.BSpec' '[error] Total time: 1 s'
exit 1
EOF
cat > "$work/fake-sbt-control" <<'EOF'
#!/usr/bin/env bash
printf '%s\n' '[error] Failed tests:' $'[error] \tb.BSpec' $'[error] \tz.NotAskedSpec'
exit 1
EOF
chmod +x "$work/fake-sbt-leg" "$work/fake-sbt-control"
ORDER_CONTROL_CMD="$work/fake-sbt-control" GITHUB_OUTPUT=/dev/null GITHUB_STEP_SUMMARY=/dev/null \
  "$tool" leg scala-reverse "$work/s" -- "$work/fake-sbt-leg" > /dev/null 2>&1
check "leg scala: the failing suites read from sbt's log; a suite the control names but the leg did not fail is no finding" \
  "a.ASpec b.BSpec|b.BSpec|regression" \
  "$(paste -sd' ' - < "$work/s/scala-reverse.txt")|$(cat "$work/s/scala-reverse.control.txt")|$(cat "$work/s/scala-reverse.verdict")"
cat > "$work/fake-gradle-control" <<'EOF'
#!/usr/bin/env bash
mkdir -p "$ORDER_ANDROID_RESULTS"; echo '<testsuite><testcase name="t" classname="pl.kinowo.prefs.PrefsTest"></testcase></testsuite>' > "$ORDER_ANDROID_RESULTS/TEST-x.xml"
EOF
chmod +x "$work/fake-gradle-control"
ORDER_ANDROID_RESULTS="$work/junit" ORDER_CONTROL_CMD="$work/fake-gradle-control" GITHUB_OUTPUT=/dev/null GITHUB_STEP_SUMMARY=/dev/null \
  "$tool" leg android "$work/a" -- false > /dev/null 2>&1
check "leg android: the failing classes read from the JUnit reports; all passing in default order: ORDER" \
  'pl.kinowo.prefs.PrefsTest pl.kinowo.sync.SyncTest$Nested|order' "$(paste -sd' ' - < "$work/a/android.txt")|$(cat "$work/a/android.verdict")"

# -- the run's verdict over every leg, and scala's two orders compared
record() { # <dir> <leg> <verdict> <failed> <default order failed> <result>
  printf '%s' "$3" > "$1/$2.verdict"; printf '%s' "$4" | tr ' ' '\n' | grep . > "$1/$2.txt"
  printf '%s' "$5" | tr ' ' '\n' | grep . > "$1/$2.control.txt"; echo "$6" > "$1/$2.result"
}
mkdir -p "$work/v"
record "$work/v" ios order 'AUITests' '' failure
record "$work/v" swift pass '' '' success
check "verdict: an order leg and a passing one: order" "order" "$("$tool" verdict "$work/v")"
record "$work/v" android regression 'p.A p.B' 'p.A' failure
check "...any regression: regression, and every leg's verdict and tests in order-verdict.json" \
  'regression|{"verdict":"regression","legs":{"android":{"verdict":"regression","failed":["p.A","p.B"],"default_order_failed":["p.A"]},"ios":{"verdict":"order","failed":["AUITests"],"default_order_failed":[]},"swift":{"verdict":"pass","failed":[],"default_order_failed":[]}}}' \
  "$("$tool" verdict "$work/v")|$(cat "$work/v/order-verdict.json")"
check "...the summary, a line a leg" \
  '- `android`: **REGRESSION, not an order dependence**: `p.A` fail in default order too|- `ios`: **order dependence (or a flake)**: `AUITests` pass in default order|- `swift`: passed' \
  "$("$tool" summary "$work/v" | sed -n '/every leg/,/reverse vs shuffled/p' | grep '^- ' | paste -sd'|' -)"
mkdir -p "$work/empty"
check "verdict: no leg recorded anything (each stopped at its preflight): none" "none" "$("$tool" verdict "$work/empty")"

legs() { # <reverse suites> <reverse result> <shuffled suites> <shuffled result>: scala's comparison
  rm -rf "$work/d"; mkdir -p "$work/d"
  printf '%s' "$1" | tr ' ' '\n' | grep . > "$work/d/scala-reverse.txt"; [ -z "$2" ] || echo "$2" > "$work/d/scala-reverse.result"
  printf '%s' "$3" | tr ' ' '\n' | grep . > "$work/d/scala-shuffled.txt"; [ -z "$4" ] || echo "$4" > "$work/d/scala-shuffled.result"
  "$tool" summary "$work/d" 4242 | sed -n '/reverse vs shuffled/,$p'
}
check "scala: the same suite failing in both orders: a regression, not an order dependence" \
  '- **regression, not an order dependence: the same suite fails in both orders**: `a.ASpec`, `b.BSpec`' \
  "$(legs 'a.ASpec b.BSpec' failure 'a.ASpec b.BSpec' failure | grep '^- ')"
check "...a suite failing in one order only: an order dependence, with the replay of that order" \
  '- **order dependence**: `c.CSpec` fails only in the shuffled order -- replay: `TEST_ORDER_SEED=4242 sbt testUnitNoE2e`' \
  "$(legs '' success 'c.CSpec' failure | grep '^- ')"
check "...both at once: the regression said first, the order dependence after" \
  "regression|order dependence**: \`r.RSpec\` fails only in the reverse" \
  "$(legs 'a.ASpec r.RSpec' failure 'a.ASpec' failure | grep '^- ' | sed -E 's/^- \*\*(regression|order dependence\*\*: `r.RSpec` fails only in the reverse).*/\1/' | paste -sd'|' -)"
check "...a leg failing naming no suite: no order verdict, and the other's suite no order dependence" \
  '- `a.ASpec` failed in the shuffled order; the reverse leg reached no verdict to compare it with|- **no order verdict** from the reverse leg: it ended failure naming no failing suite (a compile error, a timeout or the runner)' \
  "$(legs '' failure 'a.ASpec' failure | grep '^- ' | paste -sd'|' -)"
check "...a leg with no record (it never finished) likewise" "1" \
  "$(legs '' '' '' success | grep -c 'no order verdict\*\* from the reverse leg: it ended no record')"
check "...both green: both orders passed" "- both orders passed" "$(legs '' success '' success | grep '^- ')"
check "an unknown subcommand is a usage error" "64" "$("$tool" nope > /dev/null 2>&1; echo $?)"

# -- the workflow: every leg's tests run through `leg`, its record kept whatever it ended, and one job after every leg
# (reading needs only) says the run's verdict, in its summary and as an output
wf="$REPO_ROOT/.github/workflows/order-independence.yml"
job() { sed -n "/^    $1:\$/,/^    [a-z][a-z-]*:\$/p" "$wf"; }
for l in scala android android-instrumented swift ios; do
  check "the $l leg runs its tests through order-compare.sh leg, and keeps its record with if: always()" "1 1" \
    "$(job "$l" | grep -c 'order-compare.sh leg ') $(job "$l" | grep -A1 'name: Keep the order record' | grep -c 'if: always()')"
done
check "an order-verdict job after every leg, whatever they ended, writes the summary and the verdict output" "1 1 1 1" \
  "$(grep -c '^    order-verdict:' "$wf") $(job order-verdict | grep -c 'needs: \[seed, scala, android, android-instrumented, swift, ios\]') \
$(job order-verdict | grep -c 'order-compare.sh summary order-records "$SEED" >> "$GITHUB_STEP_SUMMARY"') \
$(job order-verdict | grep -c 'verdict: ${{ steps.verdict.outputs.verdict }}')"

spec_summary
