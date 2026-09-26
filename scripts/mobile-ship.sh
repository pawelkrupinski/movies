#!/usr/bin/env bash
#
# Release main's app code to the App Store and Google Play with one command.
#
#   scripts/mobile-ship.sh --dry-run     # inspect both stores, print the plan, change nothing
#   scripts/mobile-ship.sh               # do it
#   scripts/mobile-ship.sh --help        # --version, --force, --notes-dir
#
# Picks the version from what main and both stores say (main's version if neither store has
# released it; an unreleased App Store draft or in-review version if there is one; else the next
# patch, committed and pushed to main), builds both apps in parallel from that one commit,
# submits iOS for review and promotes Android to production. A rerun after a failure resumes:
# already-uploaded builds are reused and a platform that already shipped the version is skipped.
#
# The logic is TypeScript beside the /mobile dashboard, which already holds the App Store Connect
# and Play clients: infra/version-dashboard/src/mobile-release/.
set -euo pipefail

DASHBOARD="$(cd "$(dirname "${BASH_SOURCE[0]}")/../infra/version-dashboard" && pwd)"
[ -d "$DASHBOARD/node_modules" ] || npm --prefix "$DASHBOARD" ci --silent
exec "$DASHBOARD/node_modules/.bin/tsx" "$DASHBOARD/src/mobile-release/main.ts" "$@"
