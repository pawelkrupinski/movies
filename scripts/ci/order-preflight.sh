#!/usr/bin/env bash
# RUNNER PREFLIGHT for an Order independence leg: before a minute of tests, check that this runner can run them -- the
# toolchain versions the leg pins, the simulator or emulator it drives, and the tool flavours its scripts assume (GNU
# sed/date/timeout on Linux; on macOS the BSD ones the scripts were written to cope with). A runner that cannot is
# INFRASTRUCTURE, never a test red: each failure is a `::error title=Runner preflight::UNKNOWN: ...` line, which
# agentctl's classifier reads as infra (its UNKNOWN row; the step's own name, `Runner preflight ...`, says so too), and
# the leg stops there with nothing of the tests run. ~12 of the ~66 failing jobs of runs #1-#100 were the environment
# (a missing simulator, a Maven 429, macOS's openrsync and BSD tools), each read as a code red.
#
#   scripts/ci/order-preflight.sh <leg>      scala | android | android-instrumented | device | swift | ios
#
# `device` is the booted emulator, checked from inside the emulator step (adb answers, boot completed).
# Every check runs (a runner missing two things names both); exit 1 when any failed. Also writes
# `preflight=ok|failed` to $GITHUB_OUTPUT and the network-cutting mode the runner allows (scripts/ci/no-network
# --probe) as a notice. (scripts/ci/order-preflight-test.sh)
set -uo pipefail
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
leg="${1:?usage: order-preflight.sh scala|android|android-instrumented|device|swift|ios}"
bad=0

fail() { echo "::error title=Runner preflight::UNKNOWN: runner preflight ($leg): $* -- the runner, not a test red"; bad=1; }
ok() { echo "preflight ($leg): $*"; }
have() { command -v "$1" > /dev/null 2>&1 || { fail "\`$1\` is not on PATH"; return 1; }; }
# the version line wherever it is: ci-runner's job image sets JAVA_TOOL_OPTIONS, so the JVM's first line is its
# "Picked up JAVA_TOOL_OPTIONS: ..." banner (and JDK_JAVA_OPTIONS' "NOTE: Picked up ..." likewise), never the version
# (Order independence #111: "java is unreadable" on every self-hosted leg)
java_major() { java -version 2>&1 | sed -nE 's/^[^ ]+ version "([0-9]+)[."+].*/\1/p' | head -1; }
java_is() { # <major>
  have java || return 0
  local m; m="$(java_major)"
  if [ "$m" = "$1" ]; then ok "java $m"; else fail "java is ${m:-unreadable}, the leg pins $1"; fi
}
gnu() { # <tool>: GNU's, as Linux CI and the scripts expect
  have "$1" || return 0
  if "$1" --version 2> /dev/null | head -1 | grep -q 'GNU\|coreutils'; then ok "$1 is GNU's"
  else fail "\`$1\` is not GNU's ($(command -v "$1")): a BSD tool on a Linux leg"; fi
}
linux_tools() { gnu sed; gnu date; gnu timeout; have git && ok "git"; }
macos_tools() {
  [ "$(uname -s)" = Darwin ] || { fail "a macOS leg on $(uname -s)"; return 0; }
  have xcodebuild || return 0
  local v; v="$(xcodebuild -version 2> /dev/null | head -1)"
  if [ -n "$v" ]; then ok "$v"; else fail "xcodebuild -version says nothing (no Xcode selected?)"; fi
  have swift && ok "$(swift --version 2> /dev/null | head -1)"
  if xcrun simctl list devices available > /dev/null 2>&1; then ok "simctl lists the simulators"
  else fail "\`xcrun simctl list devices\` failed: CoreSimulator is unusable"; fi
}
android_sdk() {
  local sdk="${ANDROID_SDK_ROOT:-${ANDROID_HOME:-}}"
  if [ -n "$sdk" ] && [ -x "$sdk/platform-tools/adb" ]; then ok "adb under $sdk"; else fail "no adb under the SDK (${sdk:-ANDROID_HOME unset})"; fi
  [ -x "$here/../../android/gradlew" ] || fail "android/gradlew is not executable"
}

case "$leg" in
  scala) linux_tools; java_is "${ORDER_PREFLIGHT_JAVA:-27}"; have sbt && ok "sbt" ;;
  android) linux_tools; java_is 17; android_sdk ;;
  android-instrumented)
    linux_tools; java_is 17; android_sdk
    if [ -r /dev/kvm ] && [ -w /dev/kvm ]; then ok "/dev/kvm"; else fail "/dev/kvm is not read-writable: no hardware acceleration for the emulator"; fi ;;
  device)
    have adb || exit 1
    state="$(adb get-state 2> /dev/null)"
    [ "$state" = device ] || fail "adb sees no device (state: ${state:-none})"
    booted="$(adb shell getprop sys.boot_completed 2> /dev/null | tr -d '\r')"
    [ "$booted" = 1 ] || fail "the emulator has not finished booting (sys.boot_completed=${booted:-unset})"
    [ "$bad" -ne 0 ] || ok "the emulator is booted" ;;
  swift) macos_tools ;;
  ios)
    macos_tools
    dest="$(bash "$here/ios-sim-destination.sh" 2> /dev/null)"
    case "$dest" in *id=?*) ok "simulator $dest" ;; *) fail "no iPhone simulator on this image (ios-sim-destination.sh: ${dest:-nothing})" ;; esac ;;
  *) echo "order-preflight.sh: unknown leg $leg" >&2; exit 64 ;;
esac

case "$leg" in
  scala|android|android-instrumented) echo "::notice title=Runner preflight::network for the tests: $("$here/no-network" --probe 2>&1)" ;;
esac
if [ "$bad" -ne 0 ]; then
  echo "preflight=failed" >> "${GITHUB_OUTPUT:-/dev/null}"
  echo "### Runner preflight ($leg) FAILED: the runner, not the tests -- nothing of the leg ran" >> "${GITHUB_STEP_SUMMARY:-/dev/null}"
  exit 1
fi
echo "preflight=ok" >> "${GITHUB_OUTPUT:-/dev/null}"
