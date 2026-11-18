#!/usr/bin/env bash
# THE LABEL A MEASUREMENT-SIZED JOB ASKS FOR on ci-runner: `label=<label>` on stdout, for a plan job's $GITHUB_OUTPUT.
#
#   scripts/ci/measure-lane.sh <lane>     lane: red-fix | landing | measure | '' (measure) | explore
#
# ci-runner's scheduler (scripts/ci/runner_scheduler.py, MEASURE_LANES) starts measure-slice runners for three labels,
# the highest first: movies-red-fix (a gate-red fix's landing check), movies-landing (any other landing check),
# movies-measure (everything optional), then movies-explore (the nightly drift and family explorers' rows: none of
# them starts while a landing check waits; the owner, 2026-11-18). 2026-11-16: a red fix's pre-push drift check (Identity takes 43816825023) waited
# 80+ min with 14-27 optional measurement jobs on the one movies-measure label, which GitHub hands to any of them.
#
# A lane's label is asked for only where the scheduler serves it: the plan job runs on a light runner, and a scheduler
# serving the lanes registers its light runners with LANES_MARKER beside `movies` (RUNNER_LABELS, which the job
# inherits). Anything else -- an older scheduler, a hosted runner -- gets movies-measure, said on stderr: a label no
# runner is started for would leave the job queued for ever. The explorer lane is newer than the others: it is asked for
# only where the light runner also carries explore_marker (runner_scheduler.py EXPLORE_MARKER), else movies-measure.
# Tested by measure-lane-test.sh.
set -euo pipefail
marker=movies-lanes # runner_scheduler.py LANES_MARKER (measure-lane-test.sh holds the two alike)
explore_marker=movies-lanes-explore # runner_scheduler.py EXPLORE_MARKER (likewise)

case "${1:-}" in
  red-fix|landing) wanted=movies-$1 ;;
  explore) wanted=movies-explore marker=$explore_marker ;;
  measure|'') echo "label=movies-measure"; exit 0 ;;
  *) echo "measure-lane.sh: unknown lane '$1' (red-fix, landing, measure or explore)" >&2; exit 2 ;;
esac
if [[ ",${RUNNER_LABELS:-}," == *",$marker,"* ]]; then
  echo "label=$wanted"
else
  echo "measure-lane.sh: this runner's scheduler serves no lanes (RUNNER_LABELS '${RUNNER_LABELS:-}'): $wanted asked as movies-measure" >&2
  echo "label=movies-measure"
fi
