#!/bin/sh
# Purpose: Generate Google Play phone screenshots for the Kinowo / Showtimes app
# The name its users type: android/scripts/store_screenshots.py is the tool, and its header says what it does, and how.
exec python3 "$(dirname "$0")/store_screenshots.py" "$@"
