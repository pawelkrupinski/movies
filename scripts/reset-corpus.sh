#!/bin/sh
# Purpose: Full corpus reset: wipe the scrape, enrichment, staging and read-model collections so the worker rebuilds them (incidents)
# The name its callers run: reset_corpus.py beside it is the tool, its header (`--help`) says what it does.
exec python3 "$(dirname "$0")/reset_corpus.py" "$@"
