#!/bin/sh
# Purpose: Whatever starts identity model convergence starts identity takes too (a name for dispatch_takes.py)
# The name identity-model-convergence.yml's preflight runs: scripts/ci/dispatch_takes.py is the tool (its header says
# what it dispatches, and why).
exec python3 "$(dirname "$0")/dispatch_takes.py" "$@"
