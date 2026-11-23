#!/bin/sh
# Purpose: Regenerate the identity resolver's calibrated evidence model from data
# The name its callers run (identity-decorations.yml, the design doc): scripts/identity_calibrate.py is the tool (`--help`).
exec python3 "$(dirname "$0")/identity_calibrate.py" "$@"
