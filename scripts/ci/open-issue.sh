#!/usr/bin/env bash
# The one open issue a CI reporter keeps per subject, found by its EXACT title.
#
# Sourced by scheduled-failure-issue.sh, which keeps a single open issue per failing scheduled
# workflow, comments on it while it is open, and opens a new one only when none is.
#
# `gh issue list --search` is fuzzy, so the title is re-checked exactly here: an issue that merely
# mentions it is not this one. The comparison is jq's `--arg`, not the title spliced into a filter,
# so a workflow name carrying a quote or a backslash is compared rather than breaking the filter.
#
# Needs GH_TOKEN and GH_REPO. Tested through scheduled-failure-issue-test.sh, whose stub `gh`
# answers `issue list` with JSON.

# open_issue_titled <title>  -> prints the issue number, or nothing when none is open
open_issue_titled() {
    gh issue list --state open --search "\"$1\" in:title" --json number,title \
        | jq -r --arg title "$1" 'map(select(.title == $title)) | first | .number // empty'
}
