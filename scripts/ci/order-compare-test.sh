#!/usr/bin/env bash
# order-compare.sh: the suites an sbt log names as failing, and the Order independence scala verdict -- a suite failing
# in both orders labelled a regression, not an order dependence; one failing in a single order an order dependence; a
# leg failing naming no suite no verdict. Also that the workflow wires it: each scala leg records its suites, and one
# job after both says the verdict.
# Run: bash scripts/ci/order-compare-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"
printf '\033[36m▸\033[0m order-compare.sh\n'
tool="$REPO_ROOT/scripts/ci/order-compare.sh"
work="$(mktemp -d)"; trap 'rm -rf "$work"' EXIT

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

legs() { # <reverse suites> <reverse result> <shuffled suites> <shuffled result>: a summary dir
  rm -rf "$work/d"; mkdir -p "$work/d"
  printf '%s' "$1" | tr ' ' '\n' | grep . > "$work/d/reverse.txt"; [ -z "$2" ] || echo "$2" > "$work/d/reverse.result"
  printf '%s' "$3" | tr ' ' '\n' | grep . > "$work/d/shuffled.txt"; [ -z "$4" ] || echo "$4" > "$work/d/shuffled.result"
  "$tool" summary "$work/d" 4242
}
check "the same suite failing in both orders: a regression, not an order dependence" \
  '- **regression, not an order dependence: the same suite fails in both orders**: `a.ASpec`, `b.BSpec`' \
  "$(legs 'a.ASpec b.BSpec' failure 'a.ASpec b.BSpec' failure | grep '^- ')"
check "a suite failing in one order only: an order dependence, with the replay of that order" \
  '- **order dependence**: `c.CSpec` fails only in the shuffled order -- replay: `TEST_ORDER_SEED=4242 sbt testUnitNoE2e`' \
  "$(legs '' success 'c.CSpec' failure | grep '^- ')"
check "both at once: the regression said first, the order dependence after" \
  "regression|order dependence**: \`r.RSpec\` fails only in the reverse" \
  "$(legs 'a.ASpec r.RSpec' failure 'a.ASpec' failure | grep '^- ' | sed -E 's/^- \*\*(regression|order dependence\*\*: `r.RSpec` fails only in the reverse).*/\1/' | paste -sd'|' -)"
check "a leg failing naming no suite (a compile error, a timeout): no order verdict, and the other's suite no order dependence" \
  '- `a.ASpec` failed in the shuffled order; the reverse leg reached no verdict to compare it with|- **no order verdict** from the reverse leg: it ended failure naming no failing suite (a compile error, a timeout or the runner)' \
  "$(legs '' failure 'a.ASpec' failure | grep '^- ' | paste -sd'|' -)"
check "...a leg with no record (it never finished) likewise" "1" \
  "$(legs '' '' '' success | grep -c 'no order verdict\*\* from the reverse leg: it ended no record')"
check "both green: both orders passed" "- both orders passed" "$(legs '' success '' success | grep '^- ')"
check "an unknown subcommand is a usage error" "64" "$("$tool" nope > /dev/null 2>&1; echo $?)"

# the workflow: each scala leg records its failing suites and its unit step's outcome; one job after both (decisions
# read from needs only, no input) says the verdict into the run's summary
wf="$REPO_ROOT/.github/workflows/order-independence.yml"
check "each scala leg keeps its sbt log and names its failing suites from it" "1 1" \
  "$(grep -c 'sbt testUnitNoE2e 2>&1 | tee "$RUNNER_TEMP/sbt-unit.log"' "$wf") $(grep -c 'order-compare.sh failed-suites "$RUNNER_TEMP/sbt-unit.log"' "$wf")"
check "...under pipefail, so tee never hides sbt's failure" "1" "$(grep -B3 'sbt testUnitNoE2e 2>&1 | tee' "$wf" | grep -c 'set -o pipefail')"
check "a scala-compare job after both legs, whatever they ended, writes the summary" "1 1 1" \
  "$(grep -c '^    scala-compare:' "$wf") $(sed -n '/^    scala-compare:/,/^    [a-z]/p' "$wf" | grep -c 'needs: \[seed, scala\]') \
$(sed -n '/^    scala-compare:/,/^    [a-z]/p' "$wf" | grep -c 'order-compare.sh summary failed-suites "$SEED" >> "$GITHUB_STEP_SUMMARY"')"

spec_summary
