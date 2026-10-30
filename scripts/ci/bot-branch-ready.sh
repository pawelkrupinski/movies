#!/usr/bin/env bash
# A bot's branch is ready for review: open (or refresh) ONE issue for it, assigned to the owner, carrying the report
# the bot wrote -- what the PR body was on github.com.
#
# WHY. The weekly bots (discover-cinemas, decoration-discovery, identity-refit, identity-capture) publish their work on
# a fixed chore/* branch, and on github.com opened a PR from it as the "Kinowo Sync Bot" App so Branch CI ran on it.
# Forgejo has neither: no App (its SYNC_PR_APP_* secrets exist only on github.com -- Discover UK cinemas run 2893 and
# Identity capture run 2915 went red asking for them, 2026-10-26) and no `gh pr` (the job image's gh shim refuses it),
# and movies takes no PRs: a change lands on main by fast-forward. So on Forgejo the job pushes the branch with its own
# token (.github/actions/publish-bot-branch) and this names it to the owner. The next run's refresh replaces the body
# and comments, so the owner hears of it; the owner closes the issue on landing.
#
# Usage (needs GH_TOKEN with `issues: write`, GH_REPO=<owner>/<repo> and GITHUB_SERVER_URL):
#   bot-branch-ready.sh <branch> <title> <report file> <run url> [assignee]
# Tested by scripts/ci/bot-branch-ready-test.sh against a stub `gh`.
set -euo pipefail

# SHADOW (scripts/ci/ci-writes.sh): no ready-to-land issue from a github.com shadow run.
. "$(dirname "${BASH_SOURCE[0]}")/ci-writes.sh"
ci_writes || { ci_writes_skipped "the ready-to-land issue for ${1:-}"; exit 0; }

branch="${1:?branch}"
title="${2:?title}"
report="${3:?report file}"
run_url="${4:?run url}"
assignee="${5:-}"
: "${GH_REPO:?}" "${GITHUB_SERVER_URL:?}"
[ -f "$report" ] || { echo "bot-branch-ready: no report file $report" >&2; exit 2; }

issue_title="Ready to land: $title ($branch)"
body="$(mktemp "${RUNNER_TEMP:-${TMPDIR:-/tmp}}/bot-branch-ready.XXXXXX")"
trap 'rm -f "$body"' EXIT
{
  printf '**%s** is on branch [`%s`](%s/%s/src/branch/%s), from %s.\n\n' \
    "$title" "$branch" "$GITHUB_SERVER_URL" "$GH_REPO" "$branch" "$run_url"
  printf 'Review it, then land it by fast-forwarding main to it (movies takes no PRs) and close this issue. '
  printf 'The next run with something new replaces the branch and this text. Opened by `scripts/ci/bot-branch-ready.sh`.\n\n'
  cat "$report"
} > "$body"

. "$(dirname "${BASH_SOURCE[0]}")/open-issue.sh"
open_issue="$(open_issue_titled "$issue_title")"

if [ -n "$open_issue" ]; then
  gh issue edit "$open_issue" --body-file "$body"
  gh issue comment "$open_issue" --body "Refreshed: $run_url"
else
  gh issue create --title "$issue_title" ${assignee:+--assignee "$assignee"} --body-file "$body"
fi
