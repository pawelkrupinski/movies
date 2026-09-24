#!/usr/bin/env bash
# Dispatch each convergence suite whose verdict this push could change — and ONLY those.
#
# WHY. Each suite runs in one lane that finishes the run in flight and keeps a single run
# pending behind it; every newer dispatch replaces the pending one. Main used to dispatch
# on EVERY green push, so a push that touched only the iOS app or a Grafana panel evicted
# the pending run of the pipeline commit before it — measured 2026-08-31..09-24: 239 of 398
# `Country convergence` runs and 253 of 388 `US convergence` runs ended cancelled, ~78% of
# them without ever reaching a runner, while only 44% of main's commits (525 of 1,201)
# touched a path that can change a verdict. A push that cannot change the answer now leaves
# the pending run alone, and costs nothing.
#
# "Since when?" is asked of the SUITE, not of the push: the base is the head of the suite's
# newest run. Main's own lane cancels superseded pushes, so a push's `before` can skip over
# commits whose Main run never reached this job — diffing from the suite's last run cannot.
# No base, or a base this checkout does not contain, dispatches: an extra verification run
# is cheap next to a pipeline change that never gets one.
#
# Usage: kick-convergence.sh <head-sha> <ref> <workflow name>...
# Tested by ConvergenceDispatchGateSpec against a scratch repository and a stub `gh`.
set -uo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
paths_file="${CONVERGENCE_PATHS_FILE:-$here/../convergence-paths.txt}"
matcher="${CHANGED_PATHS_MATCHER:-$here/../actions/changed-paths/matches.sh}"

head_sha="$1"; ref="$2"; shift 2

# A refused dispatch fails the job (after the other suites are tried): a green job that
# dispatched nothing reads as "nothing to verify" when a pipeline change went unverified.
failed=0
dispatch() {
    gh workflow run "$1" --ref "$ref" || { echo "::error::could not dispatch $1"; failed=1; }
}

for workflow in "$@"; do
    base=$(gh run list --workflow "$workflow" --limit 1 --json headSha --jq '.[0].headSha // ""' 2>/dev/null || true)
    if [ -z "$base" ] || ! git cat-file -e "$base^{commit}" 2>/dev/null; then
        echo "$workflow: no earlier run to diff against (base '${base:-none}') — dispatching"
        dispatch "$workflow"
        continue
    fi
    # A failed diff is not "nothing changed": dispatch, as for a missing base.
    if ! changed=$(git diff --name-only "$base" "$head_sha" | "$matcher" "$paths_file"); then
        echo "$workflow: could not diff ${base:0:9}..${head_sha:0:9} — dispatching"
        dispatch "$workflow"
    elif [ "$changed" = "true" ]; then
        echo "$workflow: pipeline paths changed since ${base:0:9} — dispatching for ${head_sha:0:9}"
        dispatch "$workflow"
    else
        echo "$workflow: nothing since ${base:0:9} can change its verdict — not dispatched, so the run already pending keeps its place"
    fi
done
exit "$failed"
