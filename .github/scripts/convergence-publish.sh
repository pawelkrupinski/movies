#!/usr/bin/env bash
# Purpose: The rolling-release writes a recording convergence leg makes (.github/actions/convergence-publish) (a name for convergence_publish.py)
# The name the convergence-publish action and verify-scrape-recording.yml run: convergence_publish.py beside it is the
# tool (its header says what each subcommand does).
exec python3 -B "$(dirname "${BASH_SOURCE[0]}")/convergence_publish.py" "$@"
