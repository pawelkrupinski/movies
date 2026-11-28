#!/bin/sh
# Purpose: One check beside the rest (.github/workflows/precommit-check.yml's `check (rest)`; a name for hook_beside.py)
# The name the workflow runs: scripts/ci/hook_beside.py is the tool, its header says what it does.
exec python3 "$(dirname "$0")/hook_beside.py" "$@"
