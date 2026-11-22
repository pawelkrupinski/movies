#!/usr/bin/env bash
# order-preflight.sh: a runner that cannot run a leg fails at its preflight as INFRASTRUCTURE -- each finding a
# `::error title=Runner preflight::UNKNOWN: ...` line (agentctl's infra row), every check reported, exit 1 -- and a
# runner that can passes. Stubbed tools (java, sed, date, timeout, adb, xcodebuild, xcrun, swift) on PATH.
# Run: bash scripts/ci/order-preflight-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"
printf '\033[36m▸\033[0m order-preflight.sh\n'
tool="$REPO_ROOT/scripts/ci/order-preflight.sh"
work="$(mktemp -d)"; trap 'rm -rf "$work"' EXIT
bin="$work/bin"; mkdir -p "$bin" "$work/sdk/platform-tools"

# GNU or BSD by $FLAVOUR, otherwise the real tool
for t in sed date timeout; do
  real="$(command -v "$t" || echo /usr/bin/false)"
  printf '#!/usr/bin/env bash\nif [ "${1:-}" = --version ]; then [ "$FLAVOUR" = gnu ] && echo "%s (GNU coreutils) 9.4" || exit 1; exit 0; fi\nexec %s "$@"\n' "$t" "$real" > "$bin/$t"
done
printf '#!/usr/bin/env bash\necho "openjdk version \\"$JAVA\\" 2026-09-15" >&2\n' > "$bin/java"
printf '#!/usr/bin/env bash\ntrue\n' > "$bin/sbt"
printf '#!/usr/bin/env bash\ncase "$1" in get-state) echo "$ADB_STATE";; shell) echo "$BOOTED";; esac\n' > "$work/sdk/platform-tools/adb"
chmod +x "$bin"/* "$work/sdk/platform-tools/adb"

pre() { # <leg>: the preflight's exit; its output in $work/out
  env PATH="$bin:$PATH" GITHUB_OUTPUT="$work/gh-out" GITHUB_STEP_SUMMARY=/dev/null ANDROID_HOME="$work/sdk" "$tool" "$1" > "$work/out" 2>&1
  echo $?
}
: > "$work/gh-out"
check "scala on a runner with GNU tools, java 27 and sbt: passes, preflight=ok" "0|1" \
  "$(FLAVOUR=gnu JAVA=27 pre scala)|$(grep -c '^preflight=ok$' "$work/gh-out")"
: > "$work/gh-out"
check "scala with java 17 and BSD sed/date/timeout: fails, naming each, every finding an UNKNOWN annotation, preflight=failed" \
  "1|4|4|1" \
  "$(FLAVOUR=bsd JAVA=17 pre scala)|$(grep -c '^::error title=Runner preflight::UNKNOWN: runner preflight (scala): ' "$work/out")|$(grep -c -e 'not GNU' -e 'java is 17, the leg pins 27' "$work/out")|$(grep -c '^preflight=failed$' "$work/gh-out")"
check "android: java 17 and adb under the SDK: passes" "0" "$(FLAVOUR=gnu JAVA=17 pre android)"
check "android with no adb under the SDK: fails" "1|1" \
  "$(FLAVOUR=gnu JAVA=17 env ANDROID_HOME="$work/none" PATH="$bin:$PATH" GITHUB_OUTPUT=/dev/null "$tool" android > "$work/out" 2>&1; echo $?)|$(grep -c 'no adb under the SDK' "$work/out")"
check "device: adb sees the booted emulator: passes" "0" "$(PATH="$work/sdk/platform-tools:$PATH" ADB_STATE=device BOOTED=1 pre device)"
check "device: still booting: fails as the runner" "1|1" \
  "$(PATH="$work/sdk/platform-tools:$PATH" ADB_STATE=device BOOTED=0 pre device)|$(grep -c 'UNKNOWN: runner preflight (device): the emulator has not finished booting' "$work/out")"
if [ "$(uname -s)" = Darwin ]; then
  printf '#!/usr/bin/env bash\necho "Xcode 26.6"\n' > "$bin/xcodebuild"
  printf '#!/usr/bin/env bash\necho "Swift version 6.2"\n' > "$bin/swift"
  printf '#!/usr/bin/env bash\n[ "$SIMCTL" = ok ]\n' > "$bin/xcrun"
  chmod +x "$bin/xcodebuild" "$bin/swift" "$bin/xcrun"
  check "swift: Xcode, swift and a working simctl: passes" "0" "$(SIMCTL=ok pre swift)"
  check "swift: CoreSimulator unusable: fails as the runner" "1|1" "$(SIMCTL=no pre swift)|$(grep -c 'UNKNOWN: runner preflight (swift): .xcrun simctl list devices. failed' "$work/out")"
else
  check "a macOS leg on Linux is the runner's fault" "1" "$(pre swift)"
fi
check "an unknown leg is a usage error" "64" "$(pre nope)"

spec_summary
