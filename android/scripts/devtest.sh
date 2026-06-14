#!/usr/bin/env bash
#
# Run Android instrumented (on-device) tests with the recurring setup gotchas
# handled for you. Without this, three things bite every time:
#
#   1. adb isn't on PATH (the SDK lives at a homebrew path, not /usr/local).
#   2. A fresh git worktree has no local.properties, so Gradle can't find the
#      SDK ("SDK location not found").
#   3. A release-signed pl.kinowo already installed (runOnDevice installs the
#      signed `releaseFast`) blocks the debug/test install with a signature
#      mismatch (INSTALL_FAILED_UPDATE_INCOMPATIBLE).
#
# This script puts platform-tools on PATH, writes a local.properties pointing at
# the SDK if one is missing, and runs connectedDebugAndroidTest under the opt-in
# `.debug` applicationId (-PdebugSuffix) so the test build coexists with the
# release-signed app instead of fighting it.
#
# Usage:
#   android/scripts/devtest.sh                                  # whole suite
#   android/scripts/devtest.sh pl.kinowo.ui.list.FiltersSheetDragDismissTest
#   android/scripts/devtest.sh 'pl.kinowo.ui.list.*'            # package glob
#
set -euo pipefail

SDK_DIR="${ANDROID_SDK_ROOT:-${ANDROID_HOME:-/opt/homebrew/share/android-commandlinetools}}"
if [ ! -x "$SDK_DIR/platform-tools/adb" ]; then
    echo "devtest: no adb under $SDK_DIR/platform-tools — set ANDROID_SDK_ROOT to your SDK." >&2
    exit 1
fi
export PATH="$SDK_DIR/platform-tools:$PATH"

ANDROID_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
if [ ! -f "$ANDROID_DIR/local.properties" ]; then
    echo "sdk.dir=$SDK_DIR" > "$ANDROID_DIR/local.properties"
    echo "devtest: wrote $ANDROID_DIR/local.properties (sdk.dir=$SDK_DIR)"
fi

if ! adb get-state >/dev/null 2>&1; then
    echo "devtest: no device. Connect a phone with USB debugging on (check: adb devices)." >&2
    exit 1
fi

GRADLE_ARGS=(-PdebugSuffix)
if [ "${1:-}" != "" ]; then
    GRADLE_ARGS+=("-Pandroid.testInstrumentationRunnerArguments.class=$1")
fi

cd "$ANDROID_DIR"
exec ./gradlew :app:connectedDebugAndroidTest "${GRADLE_ARGS[@]}"
