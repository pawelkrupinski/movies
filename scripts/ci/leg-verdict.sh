#!/usr/bin/env bash
# Post ONE leg's verdict the moment that leg ends: a line in the job summary and a commit
# status on the run's commit, named after the leg.
#
# WHY. A matrix workflow's result arrives when its SLOWEST leg does. "Record scrape fixtures"
# ran for 50-60 minutes behind the United States' full convergence leg, so a Spanish leg
# that failed at minute 10 stayed invisible for another 40-50 minutes. Cancelling the others
# is not an option: every leg publishes its own recorded fixture tree, pass or fail, and a
# cancelled leg throws its recording away. So each leg reports itself instead, and the
# workflow's `report` job still aggregates at the end.
#
# A commit status rather than an issue: one per leg (its context is unique), so five legs
# failing in the same second cannot race each other into five issues, re-posting is
# idempotent, and it needs only `statuses: write`.
#
# Usage (needs GH_TOKEN with `statuses: write`; GitHub sets the rest):
#   leg-verdict.sh <job.status: success|failure|cancelled> "<leg name>"
# Never fails the job it reports on: a status API hiccup is a warning, not a red leg.
# Tested by scripts/ci/leg-verdict-test.sh against a stub `gh`.
set -uo pipefail

job_status="${1:?job status}"
leg="${2:?leg name}"

case "$job_status" in
  success)   state=success; verdict="passed" ;;
  failure)   state=failure; verdict="FAILED" ;;
  cancelled) state=error;   verdict="was cancelled" ;;
  *)         state=error;   verdict="ended $job_status" ;;
esac

run_url="${GITHUB_SERVER_URL}/${GITHUB_REPOSITORY}/actions/runs/${GITHUB_RUN_ID}"
context="${GITHUB_WORKFLOW} / ${leg}"

if [ -n "${GITHUB_STEP_SUMMARY:-}" ]; then
  printf '### %s %s\n\n[Run](%s) — posted as commit status `%s` when this leg ended, without waiting for the others.\n' \
    "$leg" "$verdict" "$run_url" "$context" >> "$GITHUB_STEP_SUMMARY"
fi
[ "$state" = success ] || echo "::error title=${leg} ${verdict}::${leg} ${verdict} — the other legs keep running and publish their own recordings"

gh api --silent "repos/${GITHUB_REPOSITORY}/statuses/${GITHUB_SHA}" \
  -f state="$state" -f context="$context" -f target_url="$run_url" \
  -f description="${leg} ${verdict}" \
  || echo "::warning::could not post the commit status for ${leg}"
exit 0
