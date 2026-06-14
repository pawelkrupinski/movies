#!/usr/bin/env bash
# Build the iOS app and install it on the cabled iPhone/iPad.
#
# Unlike Android there is no project task for this, so we resolve the first
# connected physical device's UDID and drive xcodebuild directly. Automatic
# signing is allowed to provision (-allowProvisioningUpdates); the device must
# be paired/trusted and your team must be set on the Kinowo target in Xcode.
set -euo pipefail
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/lib.sh"

PROJECT="$REPO_ROOT/ios/Kinowo.xcodeproj"
SCHEME="Kinowo"

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
    echo "  Connected devices:" >&2
    xcrun xctrace list devices 2>/dev/null | sed -n '/== Devices ==/,/== Simulators ==/p' >&2 || true
    exit 1
  fi
  echo "  device: $UDID"
fi

dispatch "$REPO_ROOT/ios" "Deploy to iOS (cable)" \
  xcodebuild -project "$PROJECT" -scheme "$SCHEME" \
  -destination "platform=iOS,id=$UDID" \
  -allowProvisioningUpdates \
  build install
