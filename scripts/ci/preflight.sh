#!/bin/sh
# Purpose: Fail a workflow in seconds when it lacks configuration (an unset secret or variable), not code
# The name the workflows run: scripts/ci/preflight.py is the tool (its header says what it does).
exec python3 "$(dirname "$0")/preflight.py" "$@"
