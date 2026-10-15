#!/usr/bin/env bash
#
# One `git bisect run` step of .github/workflows/convergence-bisect.yml: replay the country's sample
# at whatever commit bisect checked out, over the pinned corpus.
#
#   exit 0 green, 1 red, 125 untestable — a commit older than hermetic replay, a timeout, or a
#   request that commit makes and the pinned recording never saw.
#
# Runs COPIED OUT of the checkout (the workflow copies it to $RUNNER_TEMP/bisect): `git bisect`
# rewrites the working tree under a running script, and bash reads a script as it goes. Run from the
# repository root with CODE, STEP_MINUTES, HEAP and SAMPLE_COMMAND set, and
# scrape-archive/scrapes-$CODE.tar.gz present (convergence-setup restores it). RUNNER_TEMP defaults
# to $TMPDIR, so a local `git bisect run` can drive it too.
# Tested by .github/scripts/convergence-bisect-step-test.sh against a stub `sbt`.
set -uo pipefail
log="${RUNNER_TEMP:-${TMPDIR:-/tmp}}/bisect-step.log"
git grep -q KINOWO_CONVERGENCE_HERMETIC HEAD -- worker/src/fixtures/scala/tools/ArchiveReplayWiring.scala || exit 125
# The pinned corpus over whatever the commit carries — the pair is the constant.
tar -xzf "scrape-archive/scrapes-$CODE.tar.gz"
timeout "$((STEP_MINUTES * 60))" sbt -J-Xmx"$HEAP" "$SAMPLE_COMMAND" > "$log" 2>&1
status=$?
git checkout -q -- test/resources
echo "$(git rev-parse --short HEAD): sample exit $status"
grep -q 'HERMETIC run needed' "$log" && exit 125
[ "$status" -eq 124 ] && exit 125
[ "$status" -eq 0 ] && exit 0
exit 1
