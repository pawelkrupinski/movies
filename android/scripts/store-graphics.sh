#!/bin/sh
# Purpose: Generate the per-language feature graphic (1024×500) for the Play listing, and fan the one shared app icon out to every locale
# The name its users type: android/scripts/store_graphics.py is the tool, and its header says what it does, and how.
exec python3 "$(dirname "$0")/store_graphics.py" "$@"
