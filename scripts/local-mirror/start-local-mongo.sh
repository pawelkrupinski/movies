#!/bin/sh
# Purpose: Ensure the native (Homebrew) single-node replica-set Mongo that backs local dev is up on :28017 (a name for start_local_mongo.py)
exec python3 "$(dirname "$0")/start_local_mongo.py" "$@"
