#!/usr/bin/env bash
# scripts/ios-compile-check.sh: each half's build run with its output kept aside -- removed when it compiles, kept and
# its errors shown when it does not -- and a half whose toolchain is missing skipped with a warning. xcodebuild and
# docker are stubs on the PATH.
# Run: bash scripts/ios-compile-check-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m ios-compile-check.sh\n'
work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
mkdir -p "$work/bin" "$work/tmp"
# each stub: `-version` / `info` answer unless STUB_MISSING names it; a build prints a line and fails when STUB_FAIL does
for tool in xcodebuild docker; do
  cat > "$work/bin/$tool" <<STUB
#!/usr/bin/env bash
case "\$1" in -version|info) [ "\${STUB_MISSING:-}" != $tool ]; exit ;; esac
echo "$tool: compiling"
[ "\${STUB_FAIL:-}" = $tool ] && { echo "Foo.swift:1: error: no such thing"; exit 65; }
exit 0
STUB
  chmod +x "$work/bin/$tool"
done
compile() { TMPDIR="$work/tmp" PATH="$work/bin:$PATH" bash "$REPO_ROOT/scripts/ios-compile-check.sh" "$@" > "$work/out" 2>&1; }
left() { find "$work/tmp" -type f | wc -l | tr -d ' '; }

compile; code=$?
check "both halves compile: exit 0, each said" "0 yes yes" \
  "$code $(grep -q 'app target compiles' "$work/out" && echo yes || echo no) $(grep -q 'compiles on Linux' "$work/out" && echo yes || echo no)"
check "...and no build output is left behind" "0" "$(left)"

STUB_FAIL=docker compile; code=$?
check "the Linux build failing: exit 1, its errors shown" "1 yes" "$code $(grep -q 'error: no such thing' "$work/out" && echo yes || echo no)"
check "...its full log kept where it says" "1 yes" \
  "$(left) $(log=$(sed -n 's/.*full log: //p' "$work/out"); [ -s "$log" ] && echo yes || echo no)"
rm -f "$work/tmp"/*

STUB_MISSING=xcodebuild compile --app-only; code=$?
check "no full Xcode: the app build skipped with a warning, exit 0" "0 yes" "$code $(grep -q 'SKIPPED the app-target build' "$work/out" && echo yes || echo no)"

compile --wat; code=$?
check "an unknown mode is a usage error" "2" "$code"

spec_summary
