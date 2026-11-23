#!/bin/sh
# Purpose: Pull one of .github/pinned-images.tsv's images by digest, from the first source that serves it, and refuse any other
# The name workflows and actions run: scripts/ci/pull_pinned_image.py is the tool (its header says what it does).
exec python3 "$(dirname "$0")/pull_pinned_image.py" "$@"
