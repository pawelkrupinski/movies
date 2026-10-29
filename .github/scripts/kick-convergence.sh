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
# KICK_CONVERGENCE_GH_TIMEOUT (60 s) bounds the read of each suite's last run, KICK_CONVERGENCE_REUSE_TIMEOUT (180 s) the
# search for a run to reuse.
# Tested by ConvergenceDispatchGateSpec and .github/scripts/kick-convergence-test.sh against a scratch repository and a
# stub `gh`.
set -uo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
paths_file="${CONVERGENCE_PATHS_FILE:-$here/../convergence-paths.txt}"
matcher="${CHANGED_PATHS_MATCHER:-$here/../actions/changed-paths/matches.sh}"

head_sha="$1"; ref="$2"; shift 2

# A refused dispatch fails the job (after the other suites are tried): a green job that
# dispatched nothing reads as "nothing to verify" when a pipeline change went unverified.
failed=0
# A 5xx is GitHub's own blip (2026-10-12: one HTTP 500 left a pipeline push unverified), and a spent API
# rate limit (2026-10-15: run 38818736115) resets within the hour, so
# dispatch_workflow (scripts/ci/dispatch.sh) retries it, twice, after a pause
# (DISPATCH_RETRY_SECONDS), and waits a near rate-limit reset out; any other refusal (403, 404, 422) fails at once.
. "$here/../../scripts/ci/dispatch.sh"
# bounded (tool-conventions.sh, which dispatch.sh sources), not timeout(1): stock macOS has none, and there every read
# of a suite's last run failed, so a local run dispatched every suite whatever the push touched.
# A VERDICT ALREADY PROVEN (scripts/ci/convergence_reuse.py): the identity model's suite is dispatched naming the newest
# green run whose facts carry this commit's tree key -- a landing check's, usually of this very commit -- and that run's
# preflight decides whether it holds; this is only the hint. No hint, a failed or slow search: the plain dispatch.
reuse_hint() {
    [ "$1" = "Identity model convergence" ] || return 0
    # allow-silenced: no hint is the full suite, the safe direction
    bounded "${KICK_CONVERGENCE_REUSE_TIMEOUT:-180}" "${CONVERGENCE_REUSE_FINDER:-$here/../../scripts/ci/convergence_reuse.py}" find --sha "$head_sha" 2>/dev/null || true
}
dispatch() {
    local reuse
    # A RUN OF THIS COMMIT ALREADY QUEUED OR RUNNING judges it (scripts/ci/dispatch.sh's run_in_flight): a second one only
    # waits behind it in the lane, then runs in full (run 3656: 54 min queued behind 3653, the same commit, which the
    # daily run had dispatched 39 s earlier). An unreadable list dispatches, the safe direction.
    if run_in_flight "$1" "$ref" "$head_sha" 2>/dev/null && [ -n "$IN_FLIGHT_RUN" ]; then
        echo "$1: run $IN_FLIGHT_RUN for ${head_sha:0:9} is already queued or running -- not dispatched again"
        return 0
    fi
    reuse=$(reuse_hint "$1")
    case $reuse in *[!0-9]*) reuse="" ;; esac
    [ -z "$reuse" ] || echo "$1: run $reuse ran this tree's whole suite -- dispatching it as reuse-run=$reuse (the run's preflight checks it)"
    # main's convergence takes the gate tier (identity-model-convergence.yml's `tier` input; RunnerClassSpec)
    dispatch_workflow "$1" "$ref" -f tier=gate-lane ${reuse:+-f reuse-run="$reuse"} || { echo "::error::could not dispatch $1"; failed=1; }
}

for workflow in "$@"; do
    # THE SUITE'S NEWEST RUN ON THIS BRANCH: never a measurement's (measure/*) or another branch's run, whose head can sit
    # past every pipeline change since main's last run (2026-10-28: a measure run's head made six hours of main's pushes
    # look unchanged, and main got no convergence verdict at all).
    # allow-silenced: an unreadable last run leaves no base, which DISPATCHES — the safe direction.
    base=$(bounded "${KICK_CONVERGENCE_GH_TIMEOUT:-60}" gh run list --workflow "$workflow" --branch "$ref" --limit 1 --json headSha --jq '.[0].headSha // ""' 2>/dev/null || true)
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
