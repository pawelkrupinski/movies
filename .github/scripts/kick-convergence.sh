#!/bin/sh
# Purpose: Dispatch each convergence suite whose verdict this push could change — and only those
# The name main.yml's kick-convergence job runs: .github/scripts/kick_convergence.py is the tool (its header says which
# suites it dispatches, and why only those).
exec python3 "$(dirname "$0")/kick_convergence.py" "$@"
