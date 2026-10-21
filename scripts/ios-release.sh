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
# Credentials: the App Store Connect API key from the Touch ID vault — run
# `secrets unlock movies` first (see app-store-credentials.sh).
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
BUILD_DIR="${IOS_RELEASE_BUILD_DIR:-$REPO_ROOT/ios/build/release}"
TEAM_ID="CQ4YC43YDM"
BUNDLE_ID="dev.kinowo.Kinowo"
PROFILE="Kinowo App Store"

. "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/log.sh"

upload=1
[ "${1:-}" = "--no-upload" ] && upload=0

# The iOS SDK an app is built against decides how much of the unfolded iPhone Duo's
# inner display it gets: older than 27.0 runs letterboxed in a black frame, 27.0
# stops short of the status bar, 27.1 reaches the edges. `SDKROOT = iphoneos` means
# "whatever the selected Xcode ships", so check it rather than trust it.
MIN_IOS_SDK=27.1
sdk=$(xcrun --sdk iphoneos --show-sdk-version 2>/dev/null) || die "no iOS SDK found — is full Xcode selected?"
[ "$(printf '%s\n%s\n' "$MIN_IOS_SDK" "$sdk" | sort -V | head -1)" = "$MIN_IOS_SDK" ] \
  || die "iOS SDK $sdk is older than $MIN_IOS_SDK — the build would not fill the iPhone Duo screen; select Xcode $MIN_IOS_SDK or newer"

# Before anything expensive: a locked vault should fail now, not after the archive.
. "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/app-store-credentials.sh"
load_app_store_credentials

version=$(tr -d '\n' < "$REPO_ROOT/mobile-version.txt")
say "iOS release $version"

# The tag this script pushes after upload (see tag-mobile-release.sh) names HEAD as the
# commit that produced the build. A dirty tree makes that a lie: xcodebuild compiles
# whatever's on disk, so an uncommitted change would ship while the tag points at a
# commit that doesn't contain it. See require_clean_tree in log.sh.
require_clean_tree "$REPO_ROOT" "working tree has uncommitted changes — commit or stash them first, or the pushed mobile-ios-$version tag won't match what this build actually contains"

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
# Xcode's IPA packaging spawns `rsync` off PATH and expects the system one: a
# Homebrew rsync 3.x ahead of /usr/bin dies mid-copy, which the export reports
# only as "exportArchive Copy failed" (the rsync error is in the
# .xcdistributionlogs). Pin just rsync, so every other tool still resolves as usual.
system_rsync="$BUILD_DIR/system-rsync"
mkdir -p "$system_rsync"
ln -sf /usr/bin/rsync "$system_rsync/rsync"
PATH="$system_rsync:$PATH" xcodebuild -exportArchive -archivePath "$BUILD_DIR/Kinowo.xcarchive" \
    -exportOptionsPlist "$BUILD_DIR/ExportOptions.plist" -exportPath "$BUILD_DIR/export" \
    > "$BUILD_DIR/export.log" 2>&1 || die "export failed — see $BUILD_DIR/export.log"
ipa="$BUILD_DIR/export/Kinowo.ipa"
[ -f "$ipa" ] || die "no ipa at $ipa"
ok "exported $(du -h "$ipa" | cut -f1)"

# altool ships inside the Xcode toolchain and is not on PATH.
altool="$(xcode-select -p)/usr/bin/altool"
key="$APP_STORE_KEY_ID"; iss="$APP_STORE_ISSUER_ID"

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

# Marks the commit this specific upload was actually built from — see
# tag-mobile-release.sh. Force-move the tag rather than refusing a re-tag: a
# later successful upload of the same version supersedes whichever commit the
# tag last pointed to.
"$REPO_ROOT/scripts/tag-mobile-release.sh" ios "$version"

# Build processing has taken anywhere from ~1 to ~10 minutes; the build is not
# in /v1/builds at all until it finishes, so an absent build is not a failure.
printf '\nBuild %s is processing. It appears in App Store Connect within ~1-10 min;\n' \
  "$(sed -n 's/.*CURRENT_PROJECT_VERSION = \([0-9]*\);.*/\1/p' "$REPO_ROOT/ios/Kinowo.xcodeproj/project.pbxproj" | head -1)"
printf 'poll /v1/builds rather than assuming the upload failed.\n'
