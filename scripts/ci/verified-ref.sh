#!/bin/sh
# Purpose: The last green base of a main row, kept in git as refs/verified/<name>
# The name main.yml and commit-rules-ci.sh run: scripts/ci/verified_ref.py is the tool (its header says what it does).
exec python3 "$(dirname "$0")/verified_ref.py" "$@"
