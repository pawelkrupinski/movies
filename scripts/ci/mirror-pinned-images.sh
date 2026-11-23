#!/bin/sh
# Purpose: Copy every image .github/pinned-images.tsv pins to its GHCR mirror by digest, and check that GHCR serves exactly the pin
# The name mirror-pinned-images.yml runs: scripts/ci/mirror_pinned_images.py is the tool (its header says what it does).
exec python3 "$(dirname "$0")/mirror_pinned_images.py" "$@"
