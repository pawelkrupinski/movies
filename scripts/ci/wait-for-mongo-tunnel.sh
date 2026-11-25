#!/bin/sh
# Purpose: Open the CI end of the route to prod Mongo and return only once bytes flow through it
# The name the workflows run: scripts/ci/wait_for_mongo_tunnel.py is the tool (its header says what it does).
exec python3 "$(dirname "$0")/wait_for_mongo_tunnel.py" "$@"
