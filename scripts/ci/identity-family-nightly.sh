#!/bin/sh
# Purpose: The family explorer's walk row (identity-family-explorer.yml)
# The name identity-family-explorer.yml runs: scripts/ci/identity_family_nightly.py is the tool (its header says what it does).
exec python3 "$(dirname "$0")/identity_family_nightly.py" "$@"
