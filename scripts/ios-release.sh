#!/usr/bin/env bash
#
# Archive, export and upload the iOS app to App Store Connect.
#
#   scripts/ios-release.sh              # test → archive → export → validate → upload
#   scripts/ios-release.sh --no-upload  # stop after validate (dry run)
#
# This lane had no script for three releases running: each one was rebuilt by
# hand from a memory note, which is how the ExportOptions plist and the four
# signing flags kept having to be rediscovered. `xcodebuild archive` needs manual
# signing here — the automatic style picks a development certificate and the
# export then fails on a mismatch — and `altool` lives inside the Xcode toolchain
# rather than on PATH.
#
# The version comes from `mobile-version.txt` via `mobile-release.sh`; this
# script never sets it, so run the bump first and this only builds what is
# already committed.
#
# Credentials: APP_STORE_KEY_ID + APP_STORE_ISSUER_ID out of `.env.local`, with
# the .p8 at ~/.appstoreconnect/private_keys/AuthKey_<KEY_ID>.p8. NEVER `source
# .env.local` — one value contains `&` and zsh dies parsing it, so the two vars
# are grepped out individually.
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
BUILD_DIR="${IOS_RELEASE_BUILD_DIR:-$REPO_ROOT/ios/build/release}"
TEAM_ID="CQ4YC43YDM"
BUNDLE_ID="dev.kinowo.Kinowo"
PROFILE="Kinowo App Store"

. "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/log.sh"

upload=1
[ "${1:-}" = "--no-upload" ] && upload=0

# `.env.local` is gitignored, so it exists ONLY in the main checkout — a release
# cut from a worktree (which is the normal way here) would not find it beside
# its own root. `--git-common-dir` points at the main checkout's `.git` from any
# worktree, so its parent is where to look.
env_file() {
  if [ -f "$REPO_ROOT/.env.local" ]; then printf '%s' "$REPO_ROOT/.env.local"; return; fi
  local common
  common=$(git -C "$REPO_ROOT" rev-parse --path-format=absolute --git-common-dir 2>/dev/null || true)
  [ -n "$common" ] && [ -f "$(dirname "$common")/.env.local" ] \
    && printf '%s' "$(dirname "$common")/.env.local"
}

# Grep one var out of .env.local (see the header for why not `source`).
env_var() { # $1 name
  local f v
  f=$(env_file)
  [ -n "$f" ] || die ".env.local not found (looked beside $REPO_ROOT and in the main checkout)"
  # `sed -E`: BSD sed (which is what macOS ships, and this lane is macOS-only)
  # has no `\?` or `\+` in basic regex, so the optional `export` prefix and the
  # optional quotes have to be written as ERE.
  v=$(sed -E -n "s/^[[:space:]]*(export[[:space:]]+)?$1[[:space:]]*=[[:space:]]*\"?([^\"]*)\"?[[:space:]]*$/\2/p" \
        "$f" | head -1 | tr -d '\r\n')
  [ -n "$v" ] || die "$1 not found in $f"
  printf '%s' "$v"
}

version=$(tr -d '\n' < "$REPO_ROOT/mobile-version.txt")
say "iOS release $version"

# Every log lands under BUILD_DIR, so make it before the first one is written.
rm -rf "$BUILD_DIR"
mkdir -p "$BUILD_DIR"

# The SPM target excludes every SwiftUI file, so this proves the MODEL/parser
# layer only — `xcodebuild archive` below is what type-checks Views/.
# See reference_swift_test_excludes_swiftui.
say "unit tests"
swift test --package-path "$REPO_ROOT/ios" > "$BUILD_DIR/test.log" 2>&1 \
  || die "swift test failed — see $BUILD_DIR/test.log"
ok "tests green"

cat > "$BUILD_DIR/ExportOptions.plist" <<PLIST
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
  <key>method</key><string>app-store-connect</string>
  <key>teamID</key><string>$TEAM_ID</string>
  <key>signingStyle</key><string>manual</string>
  <key>provisioningProfiles</key>
  <dict><key>$BUNDLE_ID</key><string>$PROFILE</string></dict>
  <key>uploadSymbols</key><true/>
  <key>destination</key><string>export</string>
</dict>
</plist>
PLIST

say "archive"
xcodebuild archive \
    -project "$REPO_ROOT/ios/Kinowo.xcodeproj" -scheme Kinowo -configuration Release \
    -destination 'generic/platform=iOS' -archivePath "$BUILD_DIR/Kinowo.xcarchive" \
    CODE_SIGN_STYLE=Manual PROVISIONING_PROFILE_SPECIFIER="$PROFILE" \
    CODE_SIGN_IDENTITY="Apple Distribution" DEVELOPMENT_TEAM="$TEAM_ID" \
    > "$BUILD_DIR/archive.log" 2>&1 || die "archive failed — see $BUILD_DIR/archive.log"
ok "archived"

say "export"
xcodebuild -exportArchive -archivePath "$BUILD_DIR/Kinowo.xcarchive" \
    -exportOptionsPlist "$BUILD_DIR/ExportOptions.plist" -exportPath "$BUILD_DIR/export" \
    > "$BUILD_DIR/export.log" 2>&1 || die "export failed — see $BUILD_DIR/export.log"
ipa="$BUILD_DIR/export/Kinowo.ipa"
[ -f "$ipa" ] || die "no ipa at $ipa"
ok "exported $(du -h "$ipa" | cut -f1)"

# altool ships inside the Xcode toolchain and is not on PATH.
altool="$(xcode-select -p)/usr/bin/altool"
key=$(env_var APP_STORE_KEY_ID); iss=$(env_var APP_STORE_ISSUER_ID)

say "validate"
"$altool" --validate-app -f "$ipa" -t ios --apiKey "$key" --apiIssuer "$iss" \
  > "$BUILD_DIR/validate.log" 2>&1 || die "validate failed — see $BUILD_DIR/validate.log"
ok "validated"

if [ "$upload" -eq 0 ]; then
  ok "--no-upload: stopping before upload"
  exit 0
fi

say "upload"
"$altool" --upload-app -f "$ipa" -t ios --apiKey "$key" --apiIssuer "$iss" \
  > "$BUILD_DIR/upload.log" 2>&1 || die "upload failed — see $BUILD_DIR/upload.log"
ok "uploaded"

# Build processing has taken anywhere from ~1 to ~10 minutes; the build is not
# in /v1/builds at all until it finishes, so an absent build is not a failure.
printf '\nBuild %s is processing. It appears in App Store Connect within ~1-10 min;\n' \
  "$(sed -n 's/.*CURRENT_PROJECT_VERSION = \([0-9]*\);.*/\1/p' "$REPO_ROOT/ios/Kinowo.xcodeproj/project.pbxproj" | head -1)"
printf 'poll /v1/builds rather than assuming the upload failed.\n'
