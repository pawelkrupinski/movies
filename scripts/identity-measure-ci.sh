#!/usr/bin/env bash
# Measure a resolver variant on GitHub's runners (.github/workflows/identity-measure.yml) and
# download its reports, one directory per variant, in the layout a local
# IdentityShadowIntegrationSpec run writes (full-<cc>-report.txt, full-<cc>-decisions.txt, ...).
# (scripts/identity-measure is the other measurement: the takes check's moves for one commit, judged.)
#
# The variant is this worktree's diff against its merge-base with origin/main — committed and
# uncommitted changes and new files alike — sent as a patch, so no branch is ever pushed.
#
# usage: scripts/identity-measure-ci.sh <variant> [out-dir]
#   COUNTRIES=pl,uk,de,us,es  ROBUSTNESS=off  FOCUS="phrase,phrase"  BASE_ONLY=1 (measure the base, no patch)
#   RECORDING=<Record scrape fixtures run id>: replay that recording in every country, so measurements compare
#   on one input (the default, each country's pinned pair, moves whenever a new recording is pinned)
#   MAIN_IDLE_MINUTES=120 how long to wait for Main to go idle; MEASURE_MINUTES=360 how long for the run
# Exit: 0 the run succeeded, 1 it did not; 2 it could not be dispatched, found or waited on.
set -euo pipefail
here=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
. "$here/ci/dispatch.sh"
variant=${1:?usage: identity-measure-ci.sh <variant> [out-dir]}
out=${2:-target/identity-measure/$variant}
workflow=identity-measure.yml
die() { echo "identity-measure-ci: $*" >&2; exit 2; }

timeout 120 git fetch -q origin main || die "could not fetch origin main"
base=$(git merge-base HEAD origin/main)
patch=""
if [ -z "${BASE_ONLY:-}" ]; then
  # A throwaway index sees new files too, without touching the worktree's own index.
  index=$(mktemp); trap 'rm -f "$index"' EXIT
  GIT_INDEX_FILE=$index git read-tree HEAD
  GIT_INDEX_FILE=$index git add -A
  patch=$(GIT_INDEX_FILE=$index git diff --cached --binary "$base" | gzip -9 | base64 | tr -d '\n')
  # GitHub caps a dispatch's inputs at 65,535 characters together.
  [ "${#patch}" -lt 60000 ] || { echo "patch is ${#patch} chars after gzip+base64 — over the dispatch limit" >&2; exit 1; }
fi
echo "variant $variant: base $(git log --oneline -1 "$base"), patch ${#patch} chars"

# A measurement takes one runner per country from the 20 Main needs: wait for Main to be idle.
# A failed read of Main's runs is not "idle": it stops here.
wait_until_no_runs main.yml '.status != "completed"' $((${MAIN_IDLE_MINUTES:-120} * 60)) "Main is running" \
  || die "Main did not go idle, or its runs could not be read, within ${MAIN_IDLE_MINUTES:-120} minutes"

since=$(date -u +%Y-%m-%dT%H:%M:%SZ)
dispatch_workflow "$workflow" main -f variant="$variant" -f base="$base" -f patch="$patch" \
  -f countries="${COUNTRIES:-pl,uk,de,us,es}" -f robustness="${ROBUSTNESS:-off}" -f focus="${FOCUS:-}" -f recording="${RECORDING:-}" \
  || die "could not dispatch $workflow"
find_dispatched_run "$workflow" "$since" ".displayTitle == \"identity measure $variant\"" \
  || die "dispatched, but no run named 'identity measure $variant' was found"
run=$DISPATCHED_RUN
echo "run $run: $(timeout 60 gh run view "$run" --json url --jq .url || echo "$run")"

wait_for_run "$run" $((${MEASURE_MINUTES:-360} * 60)) || die "run $run gave no conclusion within ${MEASURE_MINUTES:-360} minutes"
echo "run $run: $RUN_CONCLUSION"
status=0
[ "$RUN_CONCLUSION" = success ] || status=1

rm -rf "$out"; mkdir -p "$out"
dl=$(mktemp -d)
# A run that failed or was cancelled before uploading has nothing to download: say so and still
# exit with the RUN's status below, rather than dying here under `set -e` with the download's.
if timeout 600 gh run download "$run" --pattern "identity-measure-$variant-*" --dir "$dl"; then
  for d in "$dl"/*/; do if [ -d "$d" ]; then cp -R "$d". "$out/"; fi; done
else
  echo "run $run left no reports to download" >&2
fi
rm -rf "$dl"
echo "reports in $out:"; find "$out" -maxdepth 1 -name "full-*-decisions.txt"
exit "$status"
