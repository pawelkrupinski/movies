#!/usr/bin/env bash
#
# Assert the Xcode project carries the shared mobile version: every
# MARKETING_VERSION in project.pbxproj equals mobile-version.txt, and there are
# exactly four of them — Kinowo and KinowoUITests, each in Debug and Release.
#
# Nothing in the build fails when they disagree (Xcode is happy with any
# MARKETING_VERSION), so this is the only thing standing between a bump that
# touched one copy and two store pages that disagree. Run by the iOS workflow
# on every PR and by scripts/mobile-release-test.sh locally.
#
#   scripts/mobile-version-check.sh [mobile-version.txt] [project.pbxproj]
#
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
VERSION_FILE="${1:-$REPO_ROOT/mobile-version.txt}"
PBXPROJ="${2:-$REPO_ROOT/ios/Kinowo.xcodeproj/project.pbxproj}"
EXPECTED_ENTRIES=4

expected="$(tr -d '\n' < "$VERSION_FILE")"
entries="$(sed -n 's/.*MARKETING_VERSION = \([^;]*\);.*/\1/p' "$PBXPROJ")"
count="$(printf '%s\n' "$entries" | grep -c . || true)"
drifted="$(printf '%s\n' "$entries" | grep -vxF "$expected" || true)"

echo "mobile-version.txt: $expected; MARKETING_VERSION entries ($count): $(printf '%s' "$entries" | tr '\n' ' ')"

if [ "$count" -ne "$EXPECTED_ENTRIES" ] || [ -n "$drifted" ]; then
  echo "::error::$PBXPROJ must carry exactly $EXPECTED_ENTRIES 'MARKETING_VERSION = $expected;' entries (Kinowo + KinowoUITests, Debug + Release); run scripts/mobile-release.sh --sync" >&2
  exit 1
fi
