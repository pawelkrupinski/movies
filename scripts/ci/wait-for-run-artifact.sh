#!/bin/sh
# Purpose: Wait until this workflow run has uploaded artifact <name>, produced by another job of the run
# The name country-convergence-leg.yml runs: scripts/ci/wait_for_run_artifact.py is the tool (its header says what it does).
exec python3 "$(dirname "$0")/wait_for_run_artifact.py" "$@"
