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
marker=$(python3 -c "import sys; sys.path.insert(0, '$REPO_ROOT/scripts/ci'); import runner_scheduling as r; print(r.LANES_MARKER)")
explore_marker=$(python3 -c "import sys; sys.path.insert(0, '$REPO_ROOT/scripts/ci'); import runner_scheduling as r; print(r.EXPLORE_MARKER)")
stage_marker=$(python3 -c "import sys; sys.path.insert(0, '$REPO_ROOT/scripts/ci'); import runner_scheduling as r; print(r.STAGE_MARKER)")
consumer_marker=$(python3 -c "import sys; sys.path.insert(0, '$REPO_ROOT/scripts/ci'); import runner_scheduling as r; print(r.CONSUMER_MARKER)")

check "a red fix's rows ask for the red-fix lane where the scheduler serves it" "label=movies-red-fix" "$(lane "movies,$marker" red-fix)"
check "a landing check's rows ask for the landing lane" "label=movies-landing" "$(lane "movies,$marker" landing)"
check "a measurement asks for movies-measure" "label=movies-measure" "$(lane "movies,$marker" measure)"
check "no lane named is a measurement" "label=movies-measure" "$(lane "movies,$marker" "")"
# a scheduler that starts no runner for a lane's label would leave its jobs queued for ever
check "a scheduler serving no lanes: every lane is movies-measure" "label=movies-measure" "$(lane movies red-fix)"
check "...and so is a runner that says nothing" "label=movies-measure" "$(lane "" landing)"
check "...said on stderr" "1" "$(RUNNER_LABELS=movies bash "$REPO_ROOT/scripts/ci/measure-lane.sh" landing 2>&1 > /dev/null | grep -c 'movies-measure')"
check "an explorer's rows ask for the explorer lane where the scheduler serves it" "label=movies-explore" \
  "$(lane "movies,$marker,$explore_marker" explore)"
# a scheduler from before the explorer lane serves the other lanes and starts no runner for movies-explore
check "...and movies-measure where it serves the other lanes only" "label=movies-measure" "$(lane "movies,$marker" explore)"
check "...or none" "label=movies-measure" "$(lane movies explore)"
# Stage NixOS closures #102: deploy staging waited 2.5 h on movies-measure; the owner put it between landing and measure
check "deploy staging asks for the stage lane where the scheduler serves it" "label=movies-stage" \
  "$(lane "movies,$marker,$stage_marker" stage)"
check "...and movies-measure where it serves the older lanes only" "label=movies-measure" "$(lane "movies,$marker,$explore_marker" stage)"
# Record scrape fixtures #198: an order row took a runner before the convergence row whose upload it waits for
check "a consumer row asks for the consumer lane where the scheduler serves it" "label=movies-consumer" \
  "$(lane "movies,$marker,$consumer_marker" consumer)"
check "...and movies-measure where it serves the older lanes only" "label=movies-measure" "$(lane "movies,$marker,$stage_marker" consumer)"
check "an unknown lane is a usage error" "2" "$(lane "movies,$marker" urgent > /dev/null; echo $?)"
# the labels are the scheduler's own: a lane it starts runners for is a lane this can name
check "every lane this names is one the scheduler starts runners for" "movies-red-fix movies-landing movies-stage movies-measure movies-consumer movies-explore" \
  "$(python3 -c "import sys; sys.path.insert(0, '$REPO_ROOT/scripts/ci'); import runner_scheduling as r; print(' '.join(r.MEASURE_LANES))")"

spec_summary
