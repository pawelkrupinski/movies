#!/usr/bin/env bash
# The identity capture's report (.github/workflows/identity-capture.yml, "Write the report"), as Markdown on stdout:
# what the run was, whether the ratchet is green and what to do next, each country's capture lines, and the
# ratchet's findings.
#
#   scripts/ci/identity-capture-report.sh <captures dir> <ratchet-takes.tsv> <ratchet sbt log>
#   env: OPEN_PR (true: a re-check of the judged branch), GREEN (true: the ratchet passed), BRANCH, GITHUB_RUN_ID
#
# The findings are read from the ratchet's TSV through scripts/identity-ratchet-report, the table every reader of a
# capture's findings uses; only what no TSV carries -- a spec that FAILED, a fixture that lacks answers -- is taken
# from the sbt log. A captures dir that is absent (a re-check captures nothing) lists no countries.
# Tested by scripts/ci/identity-capture-report-test.sh.
set -euo pipefail
captures=${1:?usage: identity-capture-report.sh <captures dir> <ratchet-takes.tsv> <ratchet log>}
takes=${2:?usage: identity-capture-report.sh <captures dir> <ratchet-takes.tsv> <ratchet log>}
log=${3:?usage: identity-capture-report.sh <captures dir> <ratchet-takes.tsv> <ratchet log>}
here=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
workflow="[identity-capture.yml](../blob/main/.github/workflows/identity-capture.yml)"

if [ "${OPEN_PR:-}" = true ]; then
    echo "The judged recapture of the unmatched-cluster fixture on \`$BRANCH\`, re-checked by $workflow run ${GITHUB_RUN_ID}."
else
    echo "The weekly recapture of the unmatched-cluster fixture ($workflow, run ${GITHUB_RUN_ID}), on \`$BRANCH\`."
fi
echo
if [ "${GREEN:-}" = true ]; then
    echo "The ratchet is green: check \`expected-matches.tsv\`, then land it by fast-forwarding main once Branch CI passes."
else
    echo "The ratchet is red, so no PR yet: judge every UNJUDGED take into \`identity-judgements.tsv\` (and any WRONG one there as known-wrong, scope capture, with why) on \`$BRANCH\`, push, then \`gh workflow run identity-capture.yml -f open-pr=true\` -- it re-runs the ratchet there and opens the PR once it is green."
fi
if [ -d "$captures" ]; then
    echo
    echo "### Per country"
    echo
    echo '```'
    cat "$captures"/identity-capture-*/identity-capture-*.txt 2>/dev/null \
        | grep -E '^\[identity-capture\] [a-z]{2}: (CAPTURE|FILL|FAILED)|captured [0-9]+ listings|filled after|^  [a-z ]+ +[0-9hms]+' || echo "no country reported"
    echo '```'
fi
echo
echo "### The ratchet"
echo
echo "Every finding, untruncated and with its decision's explanation: \`scripts/identity-ratchet-report ${GITHUB_RUN_ID} --explain\`; evidence cards for the UNJUDGED takes: \`scripts/identity-judge-cards --fixtures origin/$BRANCH\`."
echo
echo '```'
if [ -f "$takes" ]; then
    python3 "$here/../identity-ratchet-report" "$takes" --limit 400
else
    echo "the ratchet wrote no findings ($takes)"
fi
if [ -f "$log" ]; then
    sed -E 's/^\[info\] ?//' "$log" | grep -E 'FAILED|lacks answers' | head -100 || true
fi
echo '```'
