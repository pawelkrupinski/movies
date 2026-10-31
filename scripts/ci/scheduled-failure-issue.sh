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
# OVERDUE is the same issue for a run that passed but found work past its deadline (a check whose
# lateness is a task, not a regression: the owner, 2026-10-27, "CI red should mean only a
# regression"): it opens or comments the same way, and the next pass with nothing overdue closes it.
#
# ALERT is the same issue for a run that found a THIRD PARTY's or a BACKLOG's problem -- a vendor down, a quota spent,
# untriaged findings piling up -- which no commit of ours made or can mend (ci-failures-2026-10-30, recommendation 2:
# the Decodo probe went red 21 times in a row for an outage only the owner could act on). The run stays green; the issue
# opens once, its body is REFRESHED (no comment, no new notification) while the condition lasts, and the next clean run
# closes it -- the probe that raised it clears it.
#
# Usage (needs GH_TOKEN with `issues: write` and GH_REPO=<owner>/<repo>):
#   scheduled-failure-issue.sh failed   "<workflow name>" "<run url>" "<assignee>"
#   scheduled-failure-issue.sh overdue  "<workflow name>" "<run url>" "<assignee>"
#   scheduled-failure-issue.sh alert    "<workflow name>" "<run url>" "<assignee>" ["<what it found>"]
#   scheduled-failure-issue.sh recovered "<workflow name>" "<run url>"
# Tested by scripts/ci/scheduled-failure-issue-test.sh against a stub `gh`.
set -euo pipefail

verdict="${1:?failed|overdue|alert|recovered}"
workflow="${2:?workflow name}"
run_url="${3:?run url}"
assignee="${4:-}"
found="${5:-}"
title="Scheduled workflow failing: $workflow"

. "$(dirname "${BASH_SOURCE[0]}")/open-issue.sh"
open_issue="$(open_issue_titled "$title")"

case "$verdict" in
  overdue)
    if [ -n "$open_issue" ]; then
      gh issue comment "$open_issue" --body "Still overdue: $run_url"
    else
      gh issue create --title "$title" ${assignee:+--assignee "$assignee"} --body \
"The scheduled workflow **$workflow** passed, but found work past its deadline: $run_url (its log's warnings name it). Not a regression, so not a red run.

This issue stays open until a run finds nothing overdue, which closes it. Opened by \`scripts/ci/scheduled-failure-issue.sh\`."
    fi
    ;;
  alert)
    [ -n "$found" ] || found="see the run's warnings"
    body="The workflow **$workflow** found a problem that is not a regression of ours (a third party's, or a backlog): $found.

Latest run: $run_url (refreshed while it lasts). The run itself is green: nothing a commit made or can mend, so no red build and no fixer. This issue closes itself at the next run that finds nothing. Opened by \`scripts/ci/scheduled-failure-issue.sh\`."
    if [ -n "$open_issue" ]; then
      gh issue edit "$open_issue" --body "$body"
    else
      gh issue create --title "$title" ${assignee:+--assignee "$assignee"} --body "$body"
    fi
    ;;
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
    echo "unknown verdict '$verdict' (failed|overdue|alert|recovered)" >&2
    exit 2
    ;;
esac
