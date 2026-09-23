#!/usr/bin/env bash
# Turn a failed SCHEDULED workflow run into a GitHub issue assigned to the repo owner, and close
# it again when the workflow next passes.
#
# WHY. A scheduled workflow that fails notifies, at most, whoever last edited its `cron:` line --
# by email, if their settings allow, with nothing anyone tracks. That is how the OG-card generator
# ran with BLANK proxy credentials and nobody heard: it now exits non-zero on them, and a red run
# that reaches nobody is the same silence with a different colour. An issue assigned to the owner
# is a notification GitHub always sends (web, mobile, email) and a record that stays open until the
# workflow is green again. One issue per workflow: a failure while one is open adds a comment,
# never a second issue.
#
# Usage (needs GH_TOKEN with `issues: write` and GH_REPO=<owner>/<repo>):
#   scheduled-failure-issue.sh failed   "<workflow name>" "<run url>" "<assignee>"
#   scheduled-failure-issue.sh recovered "<workflow name>" "<run url>"
# Tested by scripts/ci/scheduled-failure-issue-test.sh against a stub `gh`.
set -euo pipefail

verdict="${1:?failed|recovered}"
workflow="${2:?workflow name}"
run_url="${3:?run url}"
assignee="${4:-}"
title="Scheduled workflow failing: $workflow"

# `--search` is fuzzy, so the exact title is re-checked here; an issue that merely mentions the
# workflow is not this one.
open_issue="$(gh issue list --state open --search "\"$title\" in:title" --json number,title \
  --jq ".[] | select(.title == \"$title\") | .number" | head -n 1)"

case "$verdict" in
  failed)
    if [ -n "$open_issue" ]; then
      gh issue comment "$open_issue" --body "Failed again: $run_url"
    else
      gh issue create --title "$title" ${assignee:+--assignee "$assignee"} --body \
"The scheduled workflow **$workflow** failed: $run_url

This issue stays open until the workflow next passes on its schedule, which closes it; further failures are added as comments. Opened by \`scripts/ci/scheduled-failure-issue.sh\`."
    fi
    ;;
  recovered)
    if [ -n "$open_issue" ]; then
      gh issue close "$open_issue" --comment "Passed again: $run_url"
    fi
    ;;
  *)
    echo "unknown verdict '$verdict' (failed|recovered)" >&2
    exit 2
    ;;
esac
