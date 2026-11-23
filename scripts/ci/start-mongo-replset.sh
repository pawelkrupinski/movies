#!/bin/sh
# Purpose: Start a throwaway MongoDB in Docker as a single-node replica set on 127.0.0.1:27017, and return once it is PRIMARY.
# The name the workflows and actions run: scripts/ci/start_mongo_replset.py is the tool (its header says what it does).
exec python3 "$(dirname "$0")/start_mongo_replset.py" "$@"
