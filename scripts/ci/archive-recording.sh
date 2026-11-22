#!/bin/sh
# Purpose: Copy "Record scrape fixtures" recordings into the permanent archive release (.github/workflows/archive-recording.yml)
# The name archive-recording.yml runs: scripts/ci/archive_recording.py is the tool (its header says what it does).
exec python3 "$(dirname "$0")/archive_recording.py" "$@"
