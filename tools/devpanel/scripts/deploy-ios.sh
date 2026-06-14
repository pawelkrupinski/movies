#!/usr/bin/env bash
# Build the iOS app, install it on the cabled iPhone/iPad, and launch it.
#
# There's no project task for this, so we resolve the first connected physical
# device's UDID and drive xcodebuild + devicectl directly:
#   1. xcodebuild  — build the Kinowo scheme for the device
#   2. devicectl install — copy the .app onto the device
#   3. devicectl launch  — start it (so the button installs *and* runs)
#
# Automatic signing is allowed to provision (-allowProvisioningUpdates); the
# device must be paired/trusted and your team set on the Kinowo target once.
set -euo pipefail
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/lib.sh"

PROJECT="$REPO_ROOT/ios/Kinowo.xcodeproj"
SCHEME="Kinowo"
DERIVED="$REPO_ROOT/ios/build/devpanel"
PRODUCTS="$DERIVED/Build/Products/Debug-iphoneos"

# First UDID listed under "== Devices ==" (physical only; simulators sit under
# a later "== Simulators ==" header). UDIDs are the last parenthesised token.
resolve_device_udid() {
  xcrun xctrace list devices 2>/dev/null \
    | sed -n '/== Devices ==/,/== Simulators ==/p' \
    | grep -iE 'iphone|ipad' \
    | grep -oE '\(([0-9A-Fa-f-]{25,})\)$' \
    | tr -d '()' \
    | head -1
}

if [[ "${DEVPANEL_PRINT_ONLY:-}" == "1" ]]; then
  UDID="<connected-device-udid>"
else
  UDID="$(resolve_device_udid || true)"
  if [[ -z "$UDID" ]]; then
    echo "✗ No cabled iOS device found." >&2
    echo "  Plug in an iPhone/iPad, unlock it, and tap 'Trust'." >&2
    xcrun xctrace list devices 2>/dev/null | sed -n '/== Devices ==/,/== Simulators ==/p' >&2 || true
    exit 1
  fi
  echo "device: $UDID"
fi

wait_for_ios_unlock "$UDID"

step xcodebuild -project "$PROJECT" -scheme "$SCHEME" -configuration Debug \
  -destination "id=$UDID" -derivedDataPath "$DERIVED" \
  -allowProvisioningUpdates build

# Resolve the freshly built bundle + its id (read, not hard-coded, so a
# team-namespace change to the bundle id doesn't break the launch step).
if [[ "${DEVPANEL_PRINT_ONLY:-}" == "1" ]]; then
  APP="$PRODUCTS/Kinowo.app"
  BUNDLE_ID="dev.kinowo.Kinowo"
else
  APP="$(find "$PRODUCTS" -maxdepth 1 -name '*.app' | head -1)"
  [[ -n "$APP" ]] || { echo "✗ build produced no .app under $PRODUCTS" >&2; exit 1; }
  BUNDLE_ID="$(/usr/libexec/PlistBuddy -c 'Print :CFBundleIdentifier' "$APP/Info.plist")"
fi

# Install + launch wait for the device to be unlocked (devicectl can't write to
# the data partition / foreground an app on a locked device).
ios_run_unlocked xcrun devicectl device install app --device "$UDID" "$APP"
ios_run_unlocked xcrun devicectl device process launch --device "$UDID" "$BUNDLE_ID"
