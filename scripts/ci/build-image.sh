#!/bin/sh
# Purpose: Build one of the Dockerfile's images for linux/amd64 and push it under the commit's SHA tag only (main.yml's publish jobs give it the tags that ship)
# The name ci.yml runs: scripts/ci/build_image.py is the tool (its header says what it does).
exec python3 "$(dirname "$0")/build_image.py" "$@"
