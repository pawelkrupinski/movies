#!/usr/bin/env bash
# Build the signed releaseFast APK and install+launch it on the cabled Android
# device (pl.kinowo/.MainActivity). See android/app/build.gradle.kts:runOnDevice.
set -euo pipefail
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/lib.sh"

# Pick a device when several are attached (runOnDevice needs -Pserial then).
# Skipped under PRINT_ONLY so the test sees the bare command.
serial=""
[[ "${DEVPANEL_PRINT_ONLY:-}" != "1" ]] && serial="$(android_serial)"

cmd=(./gradlew runOnDevice)
[[ -n "$serial" ]] && cmd+=("-Pserial=$serial")

wait_for_android_unlock "$serial"
dispatch "$REPO_ROOT/android" "Deploy to Android (cable)" "${cmd[@]}"
