#!/bin/sh
# Purpose: Runner preflight for an Order independence leg: before a minute of tests, check that this runner can run them
# The name order-independence.yml runs: scripts/ci/order_preflight.py is the tool (its header says what it does).
exec python3 "$(dirname "$0")/order_preflight.py" "$@"
