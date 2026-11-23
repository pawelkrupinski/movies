#!/bin/sh
# Purpose: Run the race / concurrency harnesses many times over, to surface the interleavings a single CI run almost never meets
# The name nightly-stress.yml runs: scripts/ci/stress_races.py is the tool (its header says what it does, and lists the harnesses).
exec python3 "$(dirname "$0")/stress_races.py" "$@"
