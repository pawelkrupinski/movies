#!/usr/bin/env bash
# scripts/ios-compile-check.sh: each part's build run with its output kept aside -- removed when it compiles, kept and
# its errors shown when it does not -- a concurrency diagnostic failing every build (Linux's too), and a part
# whose toolchain is missing skipped with a warning. xcodebuild, swift and docker are stubs on the PATH.
# Run: bash scripts/ios-compile-check-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m ios-compile-check.sh\n'
work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
mkdir -p "$work/bin" "$work/tmp"
# each stub: `-version` / `--version` / `info` answer unless STUB_MISSING names it; a build prints a line, fails when
# STUB_FAIL names it, and also prints STUB_WARNING when STUB_WARN names it
for tool in xcodebuild swift docker; do
  cat > "$work/bin/$tool" <<STUB
#!/usr/bin/env bash
case "\$1" in -version|--version|info) [ "\${STUB_MISSING:-}" != $tool ]; exit ;; esac
echo "$tool: compiling"
[ "\${STUB_WARN:-}" = $tool ] && printf '%b\\n' "\$STUB_WARNING"
[ "\${STUB_FAIL:-}" = $tool ] && { echo "Foo.swift:1: error: no such thing"; exit 65; }
exit 0
STUB
  chmod +x "$work/bin/$tool"
done
compile() { TMPDIR="$work/tmp" PATH="$work/bin:$PATH" bash "$REPO_ROOT/scripts/ios-compile-check.sh" "$@" > "$work/out" 2>&1; }
left() { find "$work/tmp" -type f | wc -l | tr -d ' '; }

said() { grep -q "$1" "$work/out" && echo yes || echo no; }

compile; code=$?
check "all three compile: exit 0, each said" "0 yes yes yes" \
  "$code $(said 'app target compiles') $(said 'compiles on the Mac') $(said 'compiles on Linux')"
check "...and no build output is left behind" "0" "$(left)"

STUB_FAIL=docker compile; code=$?
check "the Linux build failing: exit 1, its errors shown" "1 yes" "$code $(grep -q 'error: no such thing' "$work/out" && echo yes || echo no)"
check "...its full log kept where it says" "1 yes" \
  "$(left) $(log=$(sed -n 's/.*full log: //p' "$work/out"); [ -s "$log" ] && echo yes || echo no)"
rm -f "$work/tmp"/*

race="/w/ios/Kinowo/Foo.swift:12:5: warning: capture of 'self' with non-Sendable type 'Foo' in a '@Sendable' closure"
STUB_WARN=xcodebuild STUB_WARNING="$race\n$race" compile --app-only; code=$?
check "a concurrency warning in the app build: exit 1, shown once" "1 1" "$code $(grep -c "non-Sendable type 'Foo'" "$work/out")"
check "...its full log kept where it says" "1 yes" \
  "$(left) $(log=$(sed -n 's/.*full log: //p' "$work/out"); [ -s "$log" ] && echo yes || echo no)"
rm -f "$work/tmp"/*

STUB_WARN=swift STUB_WARNING="\033[1;33m/w/ios/Kinowo/Bar.swift:3:9: warning: main actor-isolated property 'x' can not be referenced from a nonisolated context\033[0m" \
  compile --spm-only; code=$?
check "one in the Mac SwiftPM build, coloured: exit 1, shown" "1 yes" "$code $(said "Bar.swift:3:9: warning: main actor-isolated")"
rm -f "$work/tmp"/*

STUB_WARN=xcodebuild STUB_WARNING="/w/ios/Kinowo/Foo.swift:3:9: warning: 'foo()' is deprecated" compile --app-only; code=$?
check "any other warning: exit 0, nothing left behind" "0 0" "$code $(left)"

STUB_WARN=docker STUB_WARNING="$race" compile --linux-only; code=$?
check "a concurrency warning in the Linux build (Swift 6 mode there too): exit 1" "1" "$code"
rm -f "$work/tmp"/*

STUB_MISSING=xcodebuild compile --app-only; code=$?
check "no full Xcode: the app build skipped with a warning, exit 0" "0 yes" "$code $(grep -q 'SKIPPED the app-target build' "$work/out" && echo yes || echo no)"

# a project/manifest pair under IOS_DIR: floor <manifest major> <project value per configuration...>
floor() {
  local manifest=$1; shift
  rm -rf "$work/ios"; mkdir -p "$work/ios/Kinowo.xcodeproj"
  printf '        .macOS(.v15),\n        .iOS(.v%s),\n' "$manifest" > "$work/ios/Package.swift"
  for value in "$@"; do printf '\t\t\t\tIPHONEOS_DEPLOYMENT_TARGET = %s;\n' "$value"; done > "$work/ios/Kinowo.xcodeproj/project.pbxproj"
}
floor 18 18.0 18.0 18.0
IOS_DIR="$work/ios" compile --spm-only; code=$?
check "the project and the package on one floor: exit 0, the floor said" "0 yes" "$code $(said 'iOS deployment target 18.0 in the project and the package')"
floor 18 18.0 16.0 18.0
IOS_DIR="$work/ios" compile --spm-only; code=$?
check "one build configuration left behind: exit 1, both named" "1 yes" "$code $(said 'project.pbxproj says 16.0 18.0 , Package.swift 18.0')"
floor 16 18.0 18.0
IOS_DIR="$work/ios" compile --app-only; code=$?
check "the package left behind: exit 1" "1 yes" "$code $(said 'Package.swift 16.0')"
check "the real tree agrees" "0" "$(compile --spm-only; echo $?)"

compile --wat; code=$?
check "an unknown mode is a usage error" "2" "$code"

spec_summary
