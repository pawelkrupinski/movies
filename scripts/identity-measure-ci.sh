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
#   COUNTRIES=pl,de,uk,us,es  ROBUSTNESS=off  FOCUS="phrase,phrase"  BASE_ONLY=1 (measure the base, no patch)
#   RECORDING=<Record scrape fixtures run id>: replay that recording in every country, so measurements compare
#   on one input (the default, each country's pinned pair, moves whenever a new recording is pinned)
#   MAIN_IDLE_MINUTES=120 how long to wait for Main to go idle; MEASURE_MINUTES=360 how long for the run
# Exit: 0 the run succeeded, 1 it did not; 2 it could not be dispatched, found or waited on, or its reports could not
# be downloaded (gh failing, not a run that uploaded none).
set -euo pipefail
here=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
# shellcheck source=scripts/tool-conventions.sh
. "$here/tool-conventions.sh" # bounded, kinowo_countries_csv, die, usage_error
. "$here/ci/dispatch.sh"
TOOL_NAME=identity-measure-ci
# Exit 2 is "it could not be dispatched, found or waited on" (above): tool-conventions.sh's die, at that status.
DIE_STATUS=2
[ $# -ge 1 ] || usage_error "usage: identity-measure-ci.sh <variant> [out-dir]"
variant=$1
out=${2:-target/identity-measure/$variant}
workflow=identity-measure.yml
# One scratch directory for the throwaway index and the download, removed however this ends (Ctrl-C included).
scratch=$(mktemp -d "${TMPDIR:-/tmp}/identity-measure-ci.XXXXXX") || die "could not make a scratch directory"
trap 'rm -rf "$scratch"' EXIT
trap 'exit 130' INT TERM

bounded 120 git fetch -q origin main || die "could not fetch origin main"
base=$(git merge-base HEAD origin/main)
patch=""
if [ -z "${BASE_ONLY:-}" ]; then
  # A throwaway index sees new files too, without touching the worktree's own index.
  index=$scratch/index
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
  -f countries="${COUNTRIES:-$(kinowo_countries_csv)}" -f robustness="${ROBUSTNESS:-off}" -f focus="${FOCUS:-}" -f recording="${RECORDING:-}" \
  || die "could not dispatch $workflow"
find_dispatched_run "$workflow" "$since" ".displayTitle == \"identity measure $variant\"" \
  || die "dispatched, but no run named 'identity measure $variant' was found"
run=$DISPATCHED_RUN
echo "run $run: $(bounded 60 gh run view "$run" --json url --jq .url || echo "$run")"

wait_for_run "$run" $((${MEASURE_MINUTES:-360} * 60)) || die "run $run gave no conclusion within ${MEASURE_MINUTES:-360} minutes"
echo "run $run: $RUN_CONCLUSION"
status=0
[ "$RUN_CONCLUSION" = success ] || status=1

rm -rf "$out"; mkdir -p "$out"
dl=$scratch/download
mkdir -p "$dl"
# A run that failed or was cancelled before uploading has nothing to download (gh_download's 3): say so and still
# exit with the RUN's status below. A download that failed (asked again first) is no answer: reports it never read
# are not "none".
download_status=0
gh_download 600 run download "$run" --pattern "identity-measure-$variant-*" --dir "$dl" || download_status=$?
case $download_status in
  0) for d in "$dl"/*/; do if [ -d "$d" ]; then cp -R "$d". "$out/"; fi; done ;;
  3) echo "run $run left no reports to download" >&2 ;;
  *) die "could not download run $run's reports (gh failed or timed out)" ;;
esac
echo "reports in $out:"; find "$out" -maxdepth 1 -name "full-*-decisions.txt"
exit "$status"
