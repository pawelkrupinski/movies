#!/bin/sh
# Purpose: The landing check's shares (.github/workflows/precommit-check.yml)
# The name precommit-check.yml runs: scripts/ci/precommit_share.py is the tool (its header says what it does).
exec python3 "$(dirname "$0")/precommit_share.py" "$@"
