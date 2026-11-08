#!/usr/bin/env bash
# WHATEVER STARTS IDENTITY MODEL CONVERGENCE STARTS IDENTITY TAKES (the owner, 2026-11-08: "make whatever kicks off
# convergence kick off both"). Convergence's preflight runs this first: it dispatches identity-takes.yml on the same ref,
# handing it every input of the convergence dispatch that means something to a takes row, and `parent-run` -- the
# convergence run's id, which the takes run carries in its title (`Identity takes (convergence run <id>)`), so a reader
# finds one from the other (scripts/ci/convergence_reuse.py takes-run, scripts/ci/dispatch.sh find_takes_run).
#
# One entry point, so no caller of convergence (Main's kick, convergence-daily, identity-measure --full, a hand dispatch)
# can start one suite without the other. IdentityTakesWiringSpec holds the forwarded inputs to the two workflows' own.
#
# Usage: dispatch-takes.sh <ref> <convergence run id>
# env: the convergence dispatch's inputs, INPUT_<NAME> (INPUT_TAKES_BASE, ...); empty ones are not passed (the takes
# workflow's default). GH_TOKEN / GH_REPO as gh reads them.
# Exit: 0 dispatched; 1 GitHub refused it through dispatch_workflow's retries (the convergence run fails: half a gate).
# Tested by scripts/ci/dispatch-takes-test.sh against a stub gh.
set -uo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# a rate-limit reset is waited out only within the preflight's own budget
DISPATCH_RATE_LIMIT_MAX_WAIT="${DISPATCH_RATE_LIMIT_MAX_WAIT:-120}"
# shellcheck source=scripts/ci/dispatch.sh
. "$here/dispatch.sh"

ref=${1:?usage: dispatch-takes.sh <ref> <convergence run id>}
parent=${2:?usage: dispatch-takes.sh <ref> <convergence run id>}

# The inputs both workflows take (identity-takes.yml's names), as the convergence dispatch set them.
FORWARDED="profile jvm-options runner takes-base recording takes-no-store takes-skip drift-twice reuse-run"
args=(-f parent-run="$parent")
for name in $FORWARDED; do
  var="INPUT_$(tr 'a-z-' 'A-Z_' <<< "$name")"
  value="${!var:-}"
  [ -z "$value" ] || args+=(-f "$name=$value")
done

if dispatch_workflow identity-takes.yml "$ref" "${args[@]}"; then
  echo "dispatched Identity takes on $ref for convergence run $parent (${args[*]})"
  exit 0
fi
echo "::error title=no takes run::could not dispatch identity-takes.yml on $ref -- this convergence run has no Identity takes run beside it, so it fails rather than gate half a verdict"
exit 1
