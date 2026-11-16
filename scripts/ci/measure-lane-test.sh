#!/usr/bin/env bash
# measure-lane.sh: which label a measurement-sized job asks for, from the lane its dispatch names and what the light
# runner running the plan job registered (RUNNER_LABELS: a scheduler serving the lanes adds runner_scheduler.py's
# LANES_MARKER to it).
# Run: bash scripts/ci/measure-lane-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m measure-lane.sh\n'

lane() { RUNNER_LABELS=$1 bash "$REPO_ROOT/scripts/ci/measure-lane.sh" "$2" 2> /dev/null; }
marker=$(python3 -c "import sys; sys.path.insert(0, '$REPO_ROOT/scripts/ci'); import runner_scheduler as r; print(r.LANES_MARKER)")

check "a red fix's rows ask for the red-fix lane where the scheduler serves it" "label=movies-red-fix" "$(lane "movies,$marker" red-fix)"
check "a landing check's rows ask for the landing lane" "label=movies-landing" "$(lane "movies,$marker" landing)"
check "a measurement asks for movies-measure" "label=movies-measure" "$(lane "movies,$marker" measure)"
check "no lane named is a measurement" "label=movies-measure" "$(lane "movies,$marker" "")"
# a scheduler that starts no runner for a lane's label would leave its jobs queued for ever
check "a scheduler serving no lanes: every lane is movies-measure" "label=movies-measure" "$(lane movies red-fix)"
check "...and so is a runner that says nothing" "label=movies-measure" "$(lane "" landing)"
check "...said on stderr" "1" "$(RUNNER_LABELS=movies bash "$REPO_ROOT/scripts/ci/measure-lane.sh" landing 2>&1 > /dev/null | grep -c 'movies-measure')"
check "an unknown lane is a usage error" "2" "$(lane "movies,$marker" urgent > /dev/null; echo $?)"
# the labels are the scheduler's own: a lane it starts runners for is a lane this can name
check "every lane this names is one the scheduler starts runners for" "movies-red-fix movies-landing movies-measure" \
  "$(python3 -c "import sys; sys.path.insert(0, '$REPO_ROOT/scripts/ci'); import runner_scheduler as r; print(' '.join(r.MEASURE_LANES))")"

spec_summary
