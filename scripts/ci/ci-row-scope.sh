#!/bin/sh
# Purpose: Can this push move ci.yml's slow rows? Prints true when a changed path can change what they test
# The name ci.yml's `scope` job runs: scripts/ci/ci_row_scope.py is the tool (its header says what it does).
exec python3 "$(dirname "$0")/ci_row_scope.py" "$@"
