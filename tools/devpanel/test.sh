#!/usr/bin/env bash
# DevPanel commit gate. Asserts each action script dispatches the exact command
# we intend (via DEVPANEL_PRINT_ONLY), bash-syntax-checks every script, and
# type-checks the Swift app. Run: bash tools/devpanel/test.sh
set -uo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SCRIPTS="$HERE/scripts"
ROOT="$(cd "$HERE/../.." && pwd)"
fails=0

check() { # <desc> <expected> <actual>
  if [[ "$2" == "$3" ]]; then
    printf '  ok   %s\n' "$1"
  else
    printf '  FAIL %s\n    expected: %s\n    actual:   %s\n' "$1" "$2" "$3"
    fails=$((fails + 1))
  fi
}
contains() { # <desc> <needle> <haystack>
  if [[ "$3" == *"$2"* ]]; then
    printf '  ok   %s\n' "$1"
  else
    printf '  FAIL %s\n    missing %q in: %s\n' "$1" "$2" "$3"
    fails=$((fails + 1))
  fi
}

echo "▶ dispatched commands (DEVPANEL_PRINT_ONLY)"
check "android → runOnDevice on cable" \
  "cd $ROOT/android && ./gradlew runOnDevice" \
  "$(DEVPANEL_PRINT_ONLY=1 bash "$SCRIPTS/deploy-android.sh")"
check "web → sbt web/run" \
  "cd $ROOT && sbt web/run" \
  "$(DEVPANEL_PRINT_ONLY=1 bash "$SCRIPTS/run-web.sh")"
check "local stack → sbt localStack" \
  "cd $ROOT && sbt localStack" \
  "$(DEVPANEL_PRINT_ONLY=1 bash "$SCRIPTS/run-local-stack.sh")"

ios="$(DEVPANEL_PRINT_ONLY=1 bash "$SCRIPTS/deploy-ios.sh")"
contains "ios → from ios dir"        "cd $ROOT/ios" "$ios"
contains "ios → xcodebuild build install" "xcodebuild" "$ios"
contains "ios → Kinowo scheme"       "-scheme Kinowo" "$ios"
contains "ios → device destination"  "platform=iOS,id=" "$ios"

echo "▶ free_port kills a listener"
tport=9099
python3 -m http.server "$tport" --bind 127.0.0.1 >/dev/null 2>&1 &
tpid=$!
disown "$tpid" 2>/dev/null || true
for _ in 1 2 3 4 5 6 7 8 9 10; do
  sleep 0.2
  [[ -n "$(lsof -ti "tcp:$tport" -sTCP:LISTEN 2>/dev/null)" ]] && break
done
before="$(lsof -ti "tcp:$tport" -sTCP:LISTEN 2>/dev/null)"
( SCRIPT_DIR="$SCRIPTS"; source "$SCRIPTS/lib.sh"; free_port "$tport" >/dev/null )
after="$(lsof -ti "tcp:$tport" -sTCP:LISTEN 2>/dev/null)"
kill "$tpid" 2>/dev/null || true
if [[ -n "$before" && -z "$after" ]]; then
  printf '  ok   freed the listener (pid %s)\n' "$before"
else
  printf '  FAIL before=%q after=%q\n' "$before" "$after"; fails=$((fails + 1))
fi

echo "▶ bash syntax"
for f in "$SCRIPTS"/*.sh; do
  if bash -n "$f"; then printf '  ok   %s\n' "$(basename "$f")"
  else printf '  FAIL %s\n' "$(basename "$f")"; fails=$((fails + 1)); fi
done

echo "▶ swift type-check"
if swiftc -typecheck "$HERE/DevPanel/main.swift" 2>/tmp/devpanel-swiftc.log; then
  printf '  ok   main.swift\n'
else
  printf '  FAIL main.swift\n'; sed 's/^/    /' /tmp/devpanel-swiftc.log; fails=$((fails + 1))
fi

echo
if [[ $fails -eq 0 ]]; then echo "✓ all DevPanel checks passed"; exit 0
else echo "✗ $fails check(s) failed"; exit 1; fi
