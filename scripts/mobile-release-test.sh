#!/usr/bin/env bash
#
# Guards the one thing this mechanism exists for: that Play and the App Store ship
# the SAME version. Nothing in either build fails when they disagree — Gradle is
# happy with any versionName, Xcode with any MARKETING_VERSION — so without a check
# the drift is only visible on two store pages after release.
#
#   scripts/mobile-release-test.sh
#
set -uo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$HERE/.." && pwd)"
# shellcheck source=mobile-release.sh
source "$HERE/mobile-release.sh"          # sourcing must not cut a release

. "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/shell-spec.sh"

printf '\033[36m▸\033[0m mobile release versioning\n'

# ── the build number is derived, identically everywhere ──────────────────────
# The same arithmetic lives in three places (here, build.gradle.kts,
# models.ClientVersion). Monotonic and readable back: 10400 is 1.4.0.
check "1.0 → 10000"    "10000" "$(version_code 1.0)"
check "1.4.0 → 10400"  "10400" "$(version_code 1.4.0)"
check "1.10.2 → 11002" "11002" "$(version_code 1.10.2)"
check "2.0.0 → 20000"  "20000" "$(version_code 2.0.0)"
# The ordering that matters: a later version must never derive a lower code, or
# Play rejects the upload.
check "1.10 outranks 1.9" "1" \
  "$([ "$(version_code 1.10.0)" -gt "$(version_code 1.9.0)" ] && echo 1 || echo 0)"

# ── versions the script refuses ──────────────────────────────────────────────
# A non-numeric version would build fine and then break both the derived code and
# the server-side gate comparison, so it is rejected up front.
for bad in "1.2.beta" "" "v1.0" "1.0." "abc"; do
  check "rejects '$bad'" "1" "$(valid_version "$bad"; echo $?)"
done
for good in "1.0" "1.0.0" "1.10.2" "2"; do
  check "accepts '$good'" "0" "$(valid_version "$good"; echo $?)"
done

# ── the two stores agree ─────────────────────────────────────────────────────
# The actual guard. mobile-version.txt is the source of truth; iOS carries a copy
# because an Xcode project cannot read a file, so the copy must match.
VERSION="$(current_version)"
check "iOS marketing version matches mobile-version.txt" "$VERSION" "$(ios_marketing_version)"
check "iOS build number is the derived code" "$(version_code "$VERSION")" "$(ios_build_number)"
# Every build configuration, not just the first: Kinowo and KinowoUITests each
# carry a Debug and a Release copy, and syncing one is how a release ships a stale
# Release version. The same script the iOS workflow runs, so CI and this spec
# cannot disagree about what "in sync" means.
CHECK="$HERE/mobile-version-check.sh"
check "every Xcode config carries the shared marketing version" "0" \
  "$("$CHECK" >/dev/null 2>&1; echo $?)"
# ...and the check actually bites: one drifted copy, or one copy missing, fails it.
SCRATCH="$(mktemp -d)"
trap 'rm -rf "$SCRATCH"' EXIT
awk -v v="$VERSION" '!done && index($0, "MARKETING_VERSION = " v ";") { sub(/= [^;]*;/, "= 0.0.1;"); done = 1 } 1' \
  "$PBXPROJ" > "$SCRATCH/drifted.pbxproj"
check "a single drifted MARKETING_VERSION fails the check" "1" \
  "$("$CHECK" "$VERSION_FILE" "$SCRATCH/drifted.pbxproj" >/dev/null 2>&1; echo $?)"
awk -v v="$VERSION" '!done && index($0, "MARKETING_VERSION = " v ";") { done = 1; next } 1' \
  "$PBXPROJ" > "$SCRATCH/missing.pbxproj"
check "a missing MARKETING_VERSION entry fails the check" "1" \
  "$("$CHECK" "$VERSION_FILE" "$SCRATCH/missing.pbxproj" >/dev/null 2>&1; echo $?)"

# Android must READ the file rather than carry its own literal — a literal is the
# drift this whole mechanism removes.
GRADLE="$REPO_ROOT/android/app/build.gradle.kts"
check "Android takes versionName from the shared file" "1" \
  "$(grep -c 'versionName = mobileVersion' "$GRADLE")"
check "Android hardcodes no versionName" "0" \
  "$(grep -cE 'versionName = "' "$GRADLE")"
check "Android reads mobile-version.txt" "1" \
  "$(grep -c 'rootProject.file("../mobile-version.txt")' "$GRADLE")"

spec_summary
