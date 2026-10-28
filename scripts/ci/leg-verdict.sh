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
# THE CAUSE, WHEN A STEP NAMED ONE: a failing leg's step may write one line to $RUNNER_TEMP/leg-verdict-why (the
# suite's "timed out under load" verdict does), and the status and the annotation carry it after "FAILED: ". The
# status is the channel that outlives the job log: in run #2301 (2026-10-28) the runner stopped shipping the germany
# rows' logs minutes before they ended, their timeout verdicts with them, while every leg's status still arrived.
# Cut to 140 characters, a github.com status description's limit.
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

why_file="${RUNNER_TEMP:-/nonexistent}/leg-verdict-why"
said="$verdict"
if [ "$state" != success ] && [ -s "$why_file" ]; then
  said="${verdict}: $(head -n 1 "$why_file")"
fi
description="${leg} ${said}"
description="${description:0:140}"

run_url="${GITHUB_SERVER_URL}/${GITHUB_REPOSITORY}/actions/runs/${GITHUB_RUN_ID}"
context="${GITHUB_WORKFLOW} / ${leg}"

if [ -n "${GITHUB_STEP_SUMMARY:-}" ]; then
  printf '### %s %s\n\n[Run](%s) — posted as commit status `%s` when this leg ended, without waiting for the others.\n' \
    "$leg" "$said" "$run_url" "$context" >> "$GITHUB_STEP_SUMMARY"
fi
[ "$state" = success ] || echo "::error title=${leg} ${verdict}::${leg} ${said} — the other legs keep running and publish their own recordings"

gh api --silent "repos/${GITHUB_REPOSITORY}/statuses/${GITHUB_SHA}" \
  -f state="$state" -f context="$context" -f target_url="$run_url" \
  -f description="$description" \
  || echo "::warning::could not post the commit status for ${leg}"
exit 0
