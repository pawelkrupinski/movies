#!/bin/sh
# Purpose: Close what wait-for-mongo-tunnel.sh opened, and prove it is closed
# The name the workflows run: scripts/ci/close_mongo_tunnel.py is the tool (its header says what it does).
exec python3 "$(dirname "$0")/close_mongo_tunnel.py" "$@"
