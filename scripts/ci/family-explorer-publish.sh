#!/usr/bin/env bash
# The nightly family explorer's publish step (.github/workflows/identity-family-explorer.yml): join the walk rows' shards
# (scripts/ci/family_flips_report.py) and publish, as assets of the release `identity-family-explorer`,
#   family-flips-<cc>.tsv     a country's flips (listing, flip chain in `takes`, shape, rules in `note`), the whole country
#   family-flips-ranked.tsv   every country's flips by deciding rule -- structural fix 2's list (rule burn-down)
#   family-flips-new.tsv      the flips last night's file did not hold
# and a dated copy of the ranked list (the newest 30 kept). NEVER A GATE, and it files no backlog row: production rarely
# reaches these states (docs/design/identity-no-drift.md, "The family explorer"). The new-since-last-night flips are in the
# job's summary.
#
# usage: family-explorer-publish.sh <shards dir> <expect cc=n,...>
# Reads RUNNER_TEMP, GITHUB_STEP_SUMMARY, GITHUB_RUN_ID, GH_REPO. Tested by family-explorer-publish-test.sh against a stub `gh`.
set -euo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")/../.."
shards=${1:?shards dir}
expect=${2:?expected shards, cc=n,...}
rel=identity-family-explorer
previous="$RUNNER_TEMP/family-previous"
out="$RUNNER_TEMP/family-published"
mkdir -p "$previous" "$out"

if gh release view "$rel" > /dev/null 2>&1; then
    # allow-silenced: a country with no earlier file is the report's baseline night; a download that fails reads the same
    gh release download "$rel" -p 'family-flips-??.tsv' --dir "$previous" --clobber > /dev/null 2>&1 || true
else
    scripts/ci/gh-release.sh create "$rel" --prerelease --title "Identity family explorer (rolling)" \
        --notes "The nightly family explorer's flips (.github/workflows/identity-family-explorer.yml): per country, ranked by deciding rule, and new since last night. Not a version of anything."
fi
python3 scripts/ci/family_flips_report.py --shards "$shards" --expect "$expect" --previous "$previous" --out "$out" > "$RUNNER_TEMP/family-summary.md"
cat "$RUNNER_TEMP/family-summary.md"
[ -z "${GITHUB_STEP_SUMMARY:-}" ] || cat "$RUNNER_TEMP/family-summary.md" >> "$GITHUB_STEP_SUMMARY"
dated="$out/family-flips-ranked-$(date -u +%F)-${GITHUB_RUN_ID:-0}.tsv"
cp "$out/family-flips-ranked.tsv" "$dated"
scripts/ci/gh-release.sh upload "$rel" "$out"/family-flips-*.tsv --clobber
# the dated copies: the newest 30
gh release view "$rel" --json assets --jq '.assets | map(select(.name | startswith("family-flips-ranked-"))) | sort_by(.createdAt) | reverse | .[30:][] | .name' \
    | while read -r stale; do scripts/ci/gh-release.sh delete-asset "$rel" "$stale" -y; done
