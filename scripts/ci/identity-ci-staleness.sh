#!/bin/sh
# Purpose: Is the identity CI still telling us anything? Names each check that has gone stale, as overdue, not as a red run
# The name identity-ci-staleness.yml runs: scripts/ci/identity_ci_staleness.py is the tool (its header says what it does).
exec python3 "$(dirname "$0")/identity_ci_staleness.py" "$@"
