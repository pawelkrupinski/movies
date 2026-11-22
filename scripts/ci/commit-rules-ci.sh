#!/bin/sh
# Purpose: The drift gate's commit rules, on CI (scripts/ci/commit_rules.py: `allowlists` and `driftguards`)
# The name ci.yml and precommit-check.yml run: scripts/ci/commit_rules_ci.py is the tool (its header says what it does).
exec python3 "$(dirname "$0")/commit_rules_ci.py" "$@"
