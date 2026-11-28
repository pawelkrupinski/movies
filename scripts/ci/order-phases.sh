#!/bin/sh
# Purpose: How many of a country's split order-independence rows a convergence run runs (a name for order_phases.py)
# The name the workflows run: scripts/ci/order_phases.py is the tool, its header says what it does.
exec python3 "$(dirname "$0")/order_phases.py" "$@"
