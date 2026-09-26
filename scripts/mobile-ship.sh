#!/usr/bin/env bash
#
# Release main's app code to the App Store and Google Play with one command.
#
#   scripts/mobile-ship.sh --dry-run     # inspect both stores, print the plan, change nothing
#   scripts/mobile-ship.sh               # do it
#   scripts/mobile-ship.sh --help        # --version, --force, --notes-dir
#
# Always ships BOTH stores under ONE version: main's, when it is newer than anything either
# store has released; otherwise the smallest version above both stores' newest release (the
# next patch of the higher one), committed and pushed to main. An unsubmitted or in-review App
# Store version is reused and renamed to that number rather than steering it. Both apps are
# built in parallel from that one commit; iOS is submitted for review, then Android is promoted
# to production. A rerun reuses builds already uploaded for the version from the same commit,
# and leaves an App Store review alone when it already holds that version and build.
#
# The logic is TypeScript beside the /mobile dashboard, which already holds the App Store Connect
# and Play clients: infra/version-dashboard/src/mobile-release/.
set -euo pipefail

DASHBOARD="$(cd "$(dirname "${BASH_SOURCE[0]}")/../infra/version-dashboard" && pwd)"
[ -d "$DASHBOARD/node_modules" ] || npm --prefix "$DASHBOARD" ci --silent
exec "$DASHBOARD/node_modules/.bin/tsx" "$DASHBOARD/src/mobile-release/main.ts" "$@"
