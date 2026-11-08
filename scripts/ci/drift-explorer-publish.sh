#!/usr/bin/env bash
# The nightly drift explorer's one step (.github/workflows/identity-drift-explorer.yml): dispatch main's takes-drift rows
# (Identity takes', identity-takes.yml, since the owner split them off convergence on 2026-11-08) drift-only on random seeds off the gate (`drift-explore: auto`), wait for that run, merge its rows' explore-<cc>.tsv
# (scripts/ci/drift_explorer.py merge) and publish them as `new-flips.tsv` (and a dated copy, the newest 30 kept) on the
# release `identity-flip-explorer`, for agentctl to file.
#
# THE DISPATCHED RUN ENDS `failure` BY DESIGN: a drift-only run's `verdict complete` fails because its takes rows did not
# run (G7). That is no failure of the explorer -- the rows' artifacts are its answer -- so the run's conclusion is only
# said; what fails this step is a run that wrote no explore-<cc>.tsv. (Explorer #1, run 41987059524: a bare `wait` in
# wait_for_run ended this step under `set -e` at the dispatched run's failure, and nothing was published.)
#
# Reads GITHUB_SHA, RUNNER_TEMP, GITHUB_STEP_SUMMARY, GITHUB_SERVER_URL and GH_REPO, as the job sets them.
# Tested by scripts/ci/drift-explorer-publish-test.sh against a stub `gh`.
set -euo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")/../.."
. scripts/ci/dispatch.sh

# `--ref main` runs main's head AT THE DISPATCH, which is not GITHUB_SHA once main moved while this job queued (Explorer
# #5, run 42455836290: queued on 4d8307317e, main at c903f3384d by the dispatch, "no run appeared"). So the run is
# matched on any of this job's commit and main's head just before and just after the dispatch.
main_head() { _dispatch_gh api "repos/$GH_REPO/commits/main" --jq .sha 2> /dev/null || true; }
since=$(date -u +%Y-%m-%dT%H:%M:%SZ)
before=$(main_head)
dispatch_workflow identity-takes.yml main -f drift-only=true -f drift-explore=auto
after=$(main_head)
find_dispatched_run identity-takes.yml "$since" \
  "(.headSha == \"$GITHUB_SHA\" or .headSha == \"$before\" or .headSha == \"$after\")" --branch main
run=$DISPATCHED_RUN
echo "explorer run: $GITHUB_SERVER_URL/$GH_REPO/actions/runs/$run"
# the drift rows' 85-minute ceiling, behind main's queued convergence runs in the lane (the job's 300-minute bound)
wait_for_run "$run" "${DRIFT_EXPLORER_WAIT_SECONDS:-17400}"
echo "run $run: $RUN_CONCLUSION"
dir="$RUNNER_TEMP/explore"
mkdir -p "$dir"
gh run download "$run" --pattern 'identity-drift-*' --dir "$dir"
found=()  # bash 3 has no mapfile (the hook runs every script test under /bin/bash too)
while IFS= read -r f; do found+=("$f"); done < <(find "$dir" -name 'explore-*.tsv' | sort)
[ "${#found[@]}" -gt 0 ] || { echo "::error title=drift explorer::run $run wrote no explore-<cc>.tsv -- nothing explored"; exit 1; }
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
