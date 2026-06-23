#!/usr/bin/env bash
# Compile DevPanel into a .app bundle under build/ and (unless --no-open) launch
# it. The absolute scripts directory is baked into Info.plist so the bundle keeps
# working even if copied to /Applications. Re-run after moving the repo.
set -euo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SCRIPTS_DIR="$HERE/scripts"
APP="$HERE/build/DevPanel.app"
MACOS="$APP/Contents/MacOS"

echo "▶ Compiling DevPanel.app"
rm -rf "$APP"
mkdir -p "$MACOS"

swiftc -O -o "$MACOS/DevPanel" "$HERE/DevPanel/main.swift"

cat > "$APP/Contents/Info.plist" <<PLIST
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>CFBundleName</key><string>DevPanel</string>
    <key>CFBundleDisplayName</key><string>movies DevPanel</string>
    <key>CFBundleIdentifier</key><string>dev.kinowo.DevPanel</string>
    <key>CFBundleVersion</key><string>1</string>
    <key>CFBundleShortVersionString</key><string>1.0</string>
    <key>CFBundlePackageType</key><string>APPL</string>
    <key>CFBundleExecutable</key><string>DevPanel</string>
    <key>LSUIElement</key><true/>
    <key>LSMinimumSystemVersion</key><string>12.0</string>
    <key>DevPanelScriptsDir</key><string>$SCRIPTS_DIR</string>
</dict>
</plist>
PLIST

echo "  built: $APP"
echo "  scripts: $SCRIPTS_DIR"

if [[ "${1:-}" != "--no-open" ]]; then
  open "$APP"
  echo "▶ Launched (look top-right of the screen; also a ☰ menu-bar icon)."
fi
