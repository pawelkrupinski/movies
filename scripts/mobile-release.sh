#!/usr/bin/env bash
#
# Cut a mobile release: one version, one branch, both platforms.
#
#   scripts/mobile-release.sh 1.1.0     # bump, sync both projects, branch + commit
#   scripts/mobile-release.sh --sync    # just re-sync both projects to the current
#                                       # mobile-version.txt (no branch, no commit)
#   scripts/mobile-release.sh --show    # print what each side currently says
#
# /mobile-version.txt is the single source of truth. Android reads it directly at
# configuration time; the iOS project cannot read a file, so its MARKETING_VERSION
# and CURRENT_PROJECT_VERSION are written into project.pbxproj here. That asymmetry
# is the whole reason this script exists — without it the two stores drift the
# first time someone bumps one and forgets the other, and there is no build error
# to catch it.
#
# ONE branch covers both platforms (release/mobile-<version>): they ship the same
# version from the same commit, so two branches would only create a way for them to
# disagree.
#
# The build NUMBER (Android versionCode, iOS CURRENT_PROJECT_VERSION) is derived,
# never hand-set: major*10000 + minor*100 + patch. Monotonic for any sane version
# and readable back — 10400 is 1.4.0. CI still overrides Android's with the run
# number (KINOWO_VERSION_CODE), because Play rejects a re-used code and a rebuild of
# the same version needs a fresh one.
#
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
VERSION_FILE="$REPO_ROOT/mobile-version.txt"
PBXPROJ="$REPO_ROOT/ios/Kinowo.xcodeproj/project.pbxproj"

say()  { printf '\033[36m▸\033[0m %s\n' "$*"; }
ok()   { printf '\033[32m✓\033[0m %s\n' "$*"; }
warn() { printf '\033[33m!\033[0m %s\n' "$*" >&2; }
die()  { printf '\033[31m✗\033[0m %s\n' "$*" >&2; exit 1; }

# Dotted numerics only. A version that is not purely numeric would still build but
# would break the derived build number and the server-side gate comparison.
valid_version() { # $1
  case "$1" in
    ''|*[!0-9.]*) return 1;;
    *.) return 1;;
  esac
  [ -n "${1%%.*}" ]
}

# major*10000 + minor*100 + patch, missing components read as 0 (so "1.0" is 10000).
# Mirrors the same arithmetic in android/app/build.gradle.kts and models.ClientVersion.
version_code() { # $1 version
  local IFS=.; set -- $1
  echo $(( ${1:-0} * 10000 + ${2:-0} * 100 + ${3:-0} ))
}

current_version()      { tr -d '\n' < "$VERSION_FILE"; }
ios_marketing_version() { grep -m1 -E "MARKETING_VERSION = " "$PBXPROJ" | sed 's/.*= *//;s/;.*//'; }
ios_build_number()      { grep -m1 -E "CURRENT_PROJECT_VERSION = " "$PBXPROJ" | sed 's/.*= *//;s/;.*//'; }

# Write the version into EVERY build configuration in the Xcode project. Every
# occurrence, not the first: Debug and Release each carry their own copy, and
# syncing only one is how a release ships with a stale Release version.
sync_ios() { # $1 version
  local version="$1" code; code="$(version_code "$version")"
  [ -f "$PBXPROJ" ] || die "no Xcode project at $PBXPROJ"
  perl -pi -e "s/MARKETING_VERSION = [^;]+;/MARKETING_VERSION = $version;/g" "$PBXPROJ"
  perl -pi -e "s/CURRENT_PROJECT_VERSION = [^;]+;/CURRENT_PROJECT_VERSION = $code;/g" "$PBXPROJ"
  ok "iOS → MARKETING_VERSION $version, CURRENT_PROJECT_VERSION $code"
}

show() {
  local v; v="$(current_version)"
  printf '  %-28s %s\n' "mobile-version.txt" "$v"
  printf '  %-28s %s (derived %s)\n' "android versionName" "$v" "$(version_code "$v")"
  printf '  %-28s %s\n' "ios MARKETING_VERSION" "$(ios_marketing_version)"
  printf '  %-28s %s\n' "ios CURRENT_PROJECT_VERSION" "$(ios_build_number)"
}

# Dispatch only when executed — mobile-release-test.sh sources this file to check
# the pure helpers (version arithmetic, validation), and must not cut a release or
# print usage by doing so.
if [ "${BASH_SOURCE[0]}" != "$0" ]; then return 0; fi

case "${1:-}" in
  --show) show; exit 0;;
  --sync)
    sync_ios "$(current_version)"
    ok "both platforms now on $(current_version)"
    exit 0;;
  -h|--help|"")
    awk 'NR > 2 && /^#/ { sub(/^#+ ?/, ""); print; next } NR > 2 { exit }' "${BASH_SOURCE[0]}"
    exit 0;;
esac

VERSION="$1"
valid_version "$VERSION" || die "'$VERSION' is not a dotted numeric version (e.g. 1.1.0)"

CURRENT="$(current_version)"
[ "$VERSION" != "$CURRENT" ] || die "already on $VERSION — nothing to release"
# Refuse to go backwards: Play rejects a lower versionCode outright, and the App
# Store rejects a lower marketing version, so catching it here beats finding out
# at upload.
[ "$(version_code "$VERSION")" -gt "$(version_code "$CURRENT")" ] ||
  die "$VERSION is not newer than the current $CURRENT"

BRANCH="release/mobile-$VERSION"
git -C "$REPO_ROOT" rev-parse --verify --quiet "$BRANCH" >/dev/null &&
  die "$BRANCH already exists — delete it or pick another version"
[ -z "$(git -C "$REPO_ROOT" status --porcelain)" ] ||
  die "working tree is dirty; commit or stash before cutting a release"

say "$CURRENT → $VERSION"
git -C "$REPO_ROOT" checkout -q -b "$BRANCH"
printf '%s\n' "$VERSION" > "$VERSION_FILE"
sync_ios "$VERSION"
ok "android → versionName $VERSION (reads mobile-version.txt at build time)"

git -C "$REPO_ROOT" add mobile-version.txt ios/Kinowo.xcodeproj/project.pbxproj
git -C "$REPO_ROOT" commit -q -m "Release mobile $VERSION

Both stores ship this one version: Android reads mobile-version.txt at
configuration time, the Xcode project is synced from it here."
ok "branch $BRANCH, committed"
echo
show
