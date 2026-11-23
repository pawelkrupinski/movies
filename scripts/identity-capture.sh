#!/bin/sh
# Purpose: Re-capture the unmatched-cluster fixture (test/resources/fixtures/identity-unmatched/<cc>.json.gz) in one command
# The name its callers run (identity-capture.yml, the skills): scripts/identity_capture.py is the tool (`--help`).
exec python3 "$(dirname "$0")/identity_capture.py" "$@"
