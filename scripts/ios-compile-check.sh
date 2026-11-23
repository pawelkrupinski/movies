#!/bin/sh
# Purpose: Compile the iOS code the ways CI does and a Mac's `swift test` does not (a name for ios_compile_check.py)
exec python3 "$(dirname "$0")/ios_compile_check.py" "$@"
