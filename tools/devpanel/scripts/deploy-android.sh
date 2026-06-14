#!/usr/bin/env bash
# Build the signed releaseFast APK and install+launch it on the cabled Android
# device (pl.kinowo/.MainActivity). See android/app/build.gradle.kts:runOnDevice.
set -euo pipefail
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/lib.sh"

wait_for_android_unlock
dispatch "$REPO_ROOT/android" "Deploy to Android (cable)" ./gradlew runOnDevice
