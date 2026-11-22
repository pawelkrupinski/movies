#!/bin/sh
# THE INCREMENTAL = WHOLE GATE's entry, the name its callers run (workflows, CLAUDE.md, the path lists):
# scripts/ci/identity_drift_check.py is the check -- its header says what it judges, and how.
exec python3 "$(dirname "$0")/identity_drift_check.py" "$@"
