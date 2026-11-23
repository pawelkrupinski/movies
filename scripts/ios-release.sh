#!/bin/sh
# Purpose: Archive, export and upload the iOS app to App Store Connect (a name for ios_release.py)
# The name its callers run (the /mobile dashboard's release lane, mobile-release.sh's notes): scripts/ios_release.py is
# the tool, its header (`--help` is not taken: it would start a release) says what it does, and how.
exec python3 "$(dirname "$0")/ios_release.py" "$@"
