#!/usr/bin/env bash
# The name the convergence-publish action and verify-scrape-recording.yml run: convergence_publish.py beside it is the
# tool (its header says what each subcommand does).
exec python3 -B "$(dirname "${BASH_SOURCE[0]}")/convergence_publish.py" "$@"
