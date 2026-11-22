#!/bin/sh
# Purpose: Check in seconds that the pair every convergence leg will replay exists, before any leg takes a runner
# The name the convergence workflows run: scripts/ci/convergence_preflight.py is the tool (its header says what it does).
exec python3 "$(dirname "$0")/convergence_preflight.py" "$@"
