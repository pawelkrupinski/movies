#!/usr/bin/env bash
# convergence-findings-summary.sh over hand-cut suite logs.
# Run: bash .github/scripts/convergence-findings-summary-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m convergence-findings-summary.sh\n'

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
summary="$REPO_ROOT/.github/scripts/convergence-findings-summary.sh"

: > "$work/empty.log"
check "an empty log says so, and succeeds" \
  "$(printf '### leg\nThe suite produced no output — see the job log.')" "$(bash "$summary" leg "$work/empty.log")"
check "and so does a missing one" "0" "$(bash "$summary" leg "$work/missing.log" > /dev/null; echo $?)"

printf '\033[32m[info] CountryConvergenceSpec:\033[0m\n[info] - should converge\n[info]   + coverage 98%%\n' > "$work/suite.log"
printf '2026-10-15 INFO application logging\n[pl] boot done in 41 s\n[info] the corpus\n' >> "$work/suite.log"
printf '[info] Tests: succeeded 1, failed 0\n[info] Tests: succeeded 3, failed 1\n' >> "$work/suite.log"
expected=$(printf '%s\n' '### convergence (poland)' '' '**Tests: succeeded 3, failed 1**' '' '```' \
  'CountryConvergenceSpec:' '- should converge' '  + coverage 98%' '[pl] boot done in 41 s'   'the corpus' '```')
check "a suite's narration, timings and LAST totals, uncoloured, without application logging" \
  "$expected" "$(bash "$summary" "convergence (poland)" "$work/suite.log")"

# The tags the harness really prints: PhaseTimer's `[<country code>]` (TestWiring) and an order-independence
# pass's `[<code>p<pass>]` (CountryConvergenceBehaviour's `scope`), beside lookalikes that are application logging.
printf '[usp0] replayCutover done in 12.0s\n[esp2] replayProject …\n[us] boot done\n[pl1] not a tag\n[usx0] neither\n' > "$work/tags.log"
check "the country and order-pass tags the harness prints, and only those" \
  "$(printf '%s\n' '[usp0] replayCutover done in 12.0s' '[esp2] replayProject …' '[us] boot done')" \
  "$(bash "$summary" leg "$work/tags.log" | sed -n '/^```$/,/^```$/p' | sed '1d;$d')"

printf 'only application logging\n' > "$work/crashed.log"
check "a run that never totalled says the suite did not finish" \
  "**No test totals — the suite did not finish.**" "$(bash "$summary" leg "$work/crashed.log" | sed -n 3p)"

for i in $(seq 1 400); do echo "[pl] phase $i"; done > "$work/long.log"
out=$(set -o pipefail; bash "$summary" leg "$work/long.log"); status=$?
check "a report past 150 lines keeps its first 150 and still succeeds" "0 150" \
  "$status $(grep -c '^\[pl\] phase' <<< "$out")"

spec_summary
