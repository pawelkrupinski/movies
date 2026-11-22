#!/bin/sh
# Purpose: Run a country's convergence leg locally against the same recorded inputs CI uses
# The name the skills and agents run: scripts/convergence_local.py is the tool, its header says what it does, and how.
exec python3 "$(dirname "$0")/convergence_local.py" "$@"
