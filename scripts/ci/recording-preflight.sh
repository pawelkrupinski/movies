#!/bin/sh
# Purpose: Recording preflight: before a `Record scrape fixtures` run spends 5 legs x 30-125 min, ask OMDb and TMDB, once each
# The name record-scrape-fixtures.yml runs: scripts/ci/recording_preflight.py is the tool (its header says what it does).
exec python3 "$(dirname "$0")/recording_preflight.py" "$@"
