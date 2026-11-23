#!/usr/bin/env bash
# Purpose: Run the local `/debug` mirror sync as a macOS launchd user agent, so it starts at login and restarts on failure (service.py beside it; this name is what the plist and the README run)
exec python3 "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/service.py" "$@"
