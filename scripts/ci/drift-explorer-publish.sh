#!/usr/bin/env bash
# The nightly drift explorer's publish step (.github/workflows/identity-drift-explorer.yml): merge the explore-<cc>.tsv
# its `drift` job's takes-drift rows wrote (Identity takes' rows, CALLED drift-only on random seeds off the gate,
# `drift-explore: auto`; downloaded into <dir>, an artifact a directory) -- scripts/ci/drift_explorer.py merge -- and
# publish them as `new-flips.tsv` (and a dated copy, the newest 30 kept) on the release `identity-flip-explorer`, for
# agentctl to file.
#
# Whatever the rows ended: a row that failed may still have written its explore-<cc>.tsv (Explorer #1, run 41987059524,
# published nothing when the run's failure ended the step). What fails this step is a run that wrote none.
#
# Usage: drift-explorer-publish.sh <dir>
# Reads GITHUB_RUN_ID, RUNNER_TEMP, GITHUB_STEP_SUMMARY and GH_REPO, as the job sets them.
# Tested by scripts/ci/drift-explorer-publish-test.sh against a stub `gh`.
set -euo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")/../.."

dir=${1:?usage: drift-explorer-publish.sh <dir of the artifacts>}
run=$GITHUB_RUN_ID
found=()  # bash 3 has no mapfile (the hook runs every script test under /bin/bash too)
while IFS= read -r f; do found+=("$f"); done < <(find "$dir" -name 'explore-*.tsv' | sort)
[ "${#found[@]}" -gt 0 ] || { echo "::error title=drift explorer::run $run's rows wrote no explore-<cc>.tsv -- nothing explored"; exit 1; }
python3 scripts/ci/drift_explorer.py merge "${found[@]}" > "$RUNNER_TEMP/new-flips.tsv"
n=$(($(wc -l < "$RUNNER_TEMP/new-flips.tsv") - 1))
echo "run $run: $n flip(s) no backlog row names, from ${#found[@]} row(s)"
{ echo "### Drift explorer, run $run: $n new flip(s)"; echo; echo '```'; cat "$RUNNER_TEMP/new-flips.tsv"; echo '```'; } >> "$GITHUB_STEP_SUMMARY"
rel=identity-flip-explorer
if ! gh release view "$rel" > /dev/null; then
    scripts/ci/gh-release.sh create "$rel" --prerelease --title "Identity drift explorer (rolling)" \
        --notes "The nightly drift explorer's flips no backlog row names (.github/workflows/identity-drift-explorer.yml). agentctl files each as a backlog row and a gate seed. Not a version of anything."
fi
dated="$RUNNER_TEMP/new-flips-$(date -u +%F)-$run.tsv"
cp "$RUNNER_TEMP/new-flips.tsv" "$dated"
scripts/ci/gh-release.sh upload "$rel" "$RUNNER_TEMP/new-flips.tsv" "$dated" --clobber
# the dated copies: the newest 30
gh release view "$rel" --json assets --jq '.assets | map(select(.name | startswith("new-flips-"))) | sort_by(.createdAt) | reverse | .[30:][] | .name' \
    | while read -r stale; do scripts/ci/gh-release.sh delete-asset "$rel" "$stale" -y; done
