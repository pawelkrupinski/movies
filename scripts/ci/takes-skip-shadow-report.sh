#!/bin/sh
# Purpose: The takes skip gate shadow's tally: every skip-gate status of the newest commits (a name for takes_skip_shadow_report.py)
# The name the workflows run: scripts/ci/takes_skip_shadow_report.py is the tool, its header says what it does.
exec python3 "$(dirname "$0")/takes_skip_shadow_report.py" "$@"
