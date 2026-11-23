#!/bin/sh
# Purpose: Generate App Store screenshots for the Kinowo / Showtimes iOS app
# The name its users type: ios/scripts/store_screenshots.py is the tool, and its header says what it does, and how.
exec python3 "$(dirname "$0")/store_screenshots.py" "$@"
