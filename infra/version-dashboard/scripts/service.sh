#!/bin/sh
# Purpose: One entry point for the version dashboard's launchd service (the shape of bitcashier's version-dashboard) (a name for service.py)
# THE VERSION DASHBOARD'S SERVICE ENTRY POINT -- the name its callers run (README.md, the launchd plists):
# infra/version-dashboard/scripts/service.py is the tool, its header says what it does.
exec python3 "$(dirname "$0")/service.py" "$@"
