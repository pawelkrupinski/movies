#!/usr/bin/env bash
# Build the signed releaseFast APK and install+launch it on the attached Android
# device — USB or WiFi (`adb pair` + `adb connect`), adb doesn't distinguish.
# See android/app/build.gradle.kts:runOnDevice.
set -euo pipefail
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/lib.sh"

# Wait for the device FIRST, then pick the serial. Resolving it before the wait
# reads adb while the phone may still be detached (not plugged in yet, WiFi not
# connected yet) — android_serial comes back empty, we pass no -Pserial, and
# runOnDevice is left to pick for itself once the device shows up. An explicit
# DEVPANEL_ANDROID_SERIAL pins which device we wait for.
# Serial resolution is skipped under PRINT_ONLY so the test sees the bare command.
serial="${DEVPANEL_ANDROID_SERIAL:-}"
wait_for_android_unlock "$serial"
[[ "${DEVPANEL_PRINT_ONLY:-}" != "1" ]] && serial="$(android_serial)"

cmd=(./gradlew runOnDevice)
[[ -n "$serial" ]] && cmd+=("-Pserial=$serial")

dispatch "$REPO_ROOT/android" "Deploy to Android (USB or WiFi)" "${cmd[@]}"
