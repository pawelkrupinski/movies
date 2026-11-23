#!/bin/sh
# Purpose: Lay out one "Record scrape fixtures" run's recording beside the checkout, not in it — each country's scrape corpus
# The name its callers run (identity-tools.sh's identity_recording, the identity workflows): scripts/identity_fetch_recording.py is the tool (`--help`).
exec python3 "$(dirname "$0")/identity_fetch_recording.py" "$@"
