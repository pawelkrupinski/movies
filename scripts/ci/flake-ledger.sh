#!/usr/bin/env bash
# Append this run's FLAKY tests to the flaky-test ledger: ONE open issue, "Flaky test ledger",
# with a comment per run that found any. Opened (assigned to the owner) the first time.
#
# WHY AN ISSUE, not a checked-in file. A file needs a commit to grow, and the only thing that can
# commit it is either this run (a write token inside a build that runs arbitrary project code) or
# a scheduled job reading back every run's artefacts. An issue grows with one API call from a job
# that runs no project code, notifies the owner on its first entry, and is searchable
# (`is:issue "Flaky test ledger" <test name>`) the next time the same test goes red.
#
# Usage (needs GH_TOKEN with `issues: write` and GH_REPO):
#   flake-ledger.sh --run-url <url> --sha <commit> [--assignee <login>] <flaky.tsv>...
# Each flaky.tsv line is: suite <TAB> class <TAB> test <TAB> passes/reruns (flake_verdict.py).
# Missing or empty files are fine: no flaky test, no comment. Tested by flake-ledger-test.sh.
set -euo pipefail

Title="Flaky test ledger"

run_url="" sha="" assignee=""
files=()
while [ $# -gt 0 ]; do
    case "$1" in
        --run-url)  run_url="${2:?}"; shift 2 ;;
        --sha)      sha="${2:?}"; shift 2 ;;
        --assignee) assignee="${2:-}"; shift 2 ;;
        *)          files+=("$1"); shift ;;
    esac
done
[ -n "$run_url" ] && [ -n "$sha" ] || { echo "usage: $0 --run-url U --sha S [--assignee A] <flaky.tsv>..." >&2; exit 2; }

rows=""
for f in "${files[@]+"${files[@]}"}"; do
    [ -s "$f" ] || continue
    while IFS=$'\t' read -r suite cls name passes; do
        [ -n "$cls" ] || continue
        rows+="| ${suite//|/\\|} | \`${cls//|/\\|}\` | ${name//|/\\|} | $passes |"$'\n'
    done < "$f"
done
if [ -z "$rows" ]; then
    echo "no flaky tests in this run — ledger untouched"
    exit 0
fi

body="Flaky in ${sha:0:9} — $run_url (the build failed regardless; a rerun is never a fix)

| Suite | Class | Test | Passed on rerun |
|---|---|---|---|
$rows"

issue="$(gh issue list --state open --search "\"$Title\" in:title" --json number,title \
    --jq ".[] | select(.title == \"$Title\") | .number" | head -n 1)"
if [ -n "$issue" ]; then
    gh issue comment "$issue" --body "$body"
else
    gh issue create --title "$Title" ${assignee:+--assignee "$assignee"} --body \
"Every test a Main run found FLAKY — failed, then passed at least once of three reruns in the same job on the same commit — one comment per run. Written by \`scripts/ci/flake-ledger.sh\`; see \`scripts/ci/flake_verdict.py\` for the verdict. Close it when the entries are fixed; the next flake opens a fresh one.

$body"
fi
