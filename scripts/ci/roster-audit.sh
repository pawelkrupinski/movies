#!/usr/bin/env bash
# The roster-audit workflow's one step: run tools.RosterAudit and turn its verdict into the job's.
#
# A source disagreeing with the roster (exit 3, RosterAudit.DriftFound) is data drift -- a fix to the roster, the
# owner's task -- so the run stays green, warns, and writes overdue=true to $GITHUB_OUTPUT; the workflow's `report`
# job hands it to report-scheduled-run, which opens the owner-assigned issue (OverdueIsNotRedSpec). Any other non-zero
# exit -- the audit could not judge (a chain list unread through the residential proxy, too few pages read), or sbt
# itself failed -- stays red with that exit.
#
# Usage: roster-audit.sh <report file>   (the workflow passes $GITHUB_STEP_SUMMARY)
# Tested by scripts/ci/roster-audit-test.sh against a stub `sbt`.
set -uo pipefail

report="${1:?report file}"
output="${GITHUB_OUTPUT:-/dev/null}"

sbt "worker/runMain tools.RosterAudit $report"
status=$?

case "$status" in
  0)
    echo "overdue=false" >> "$output"
    ;;
  3)
    echo "overdue=true" >> "$output"
    echo "::warning::the cinema roster disagrees with its sources (see the report above): the owner's issue tracks the fix"
    ;;
  *)
    echo "overdue=false" >> "$output"
    exit "$status"
    ;;
esac
