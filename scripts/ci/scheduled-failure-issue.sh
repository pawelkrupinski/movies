#!/bin/sh
# Purpose: Turn a failed scheduled workflow run into a GitHub issue assigned to the repo owner, and close it again when the workflow next passes
# The name .github/actions/report-scheduled-run runs: scripts/ci/scheduled_failure_issue.py is the tool (its header says what it does).
exec python3 "$(dirname "$0")/scheduled_failure_issue.py" "$@"
