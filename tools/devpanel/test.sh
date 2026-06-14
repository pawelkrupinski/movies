#!/usr/bin/env bash
# DevPanel commit gate. Asserts each action script dispatches the commands we
# intend (via DEVPANEL_PRINT_ONLY), checks free_port actually frees a port,
# bash-syntax-checks every script, then compiles the Swift app and runs its
# headless self-test (the real subprocess-streaming path). Run:
#   bash tools/devpanel/test.sh
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
android="$(DEVPANEL_PRINT_ONLY=1 bash "$SCRIPTS/deploy-android.sh")"
contains "android → waits for unlock"   "wait_for_android_unlock" "$android"
contains "android → runOnDevice"        "cd $ROOT/android && ./gradlew runOnDevice" "$android"
check "web → sbt web/run" \
  "cd $ROOT && sbt web/run" \
  "$(DEVPANEL_PRINT_ONLY=1 bash "$SCRIPTS/run-web.sh")"
check "local stack → sbt localStack" \
  "cd $ROOT && sbt localStack" \
  "$(DEVPANEL_PRINT_ONLY=1 bash "$SCRIPTS/run-local-stack.sh")"

ios="$(DEVPANEL_PRINT_ONLY=1 bash "$SCRIPTS/deploy-ios.sh")"
contains "ios → waits for unlock"        "wait_for_ios_unlock <connected-device-udid>" "$ios"
contains "ios → xcodebuild build"        "xcodebuild" "$ios"
contains "ios → Kinowo scheme"           "-scheme Kinowo" "$ios"
contains "ios → device destination"      "id=<connected-device-udid>" "$ios"
contains "ios → installs the .app"       "devicectl device install app" "$ios"
contains "ios → launches the app"        "devicectl device process launch" "$ios"

echo "▶ worktree override (DEVPANEL_REPO_ROOT)"
contains "web honours override"  "cd /tmp/wt && sbt web/run" \
  "$(DEVPANEL_REPO_ROOT=/tmp/wt DEVPANEL_PRINT_ONLY=1 bash "$SCRIPTS/run-web.sh")"
contains "android honours override" "cd /tmp/wt/android && ./gradlew runOnDevice" \
  "$(DEVPANEL_REPO_ROOT=/tmp/wt DEVPANEL_PRINT_ONLY=1 bash "$SCRIPTS/deploy-android.sh")"

echo "▶ ios lock detection"
(
  SCRIPT_DIR="$SCRIPTS"; source "$SCRIPTS/lib.sh"
  # Verbatim from a real `devicectl process launch` against a locked iPhone.
  locked='The request was denied by service delegate (SBMainWorkspace) for reason: Locked ("Unable to launch dev.kinowo.Kinowo because the device was not, or could not be, unlocked"). BSErrorCodeDescription = Locked'
  ios_is_lock_error "$locked" \
    && echo "  ok   classifies the real launch lock error" || { echo "  FAIL locked text not matched"; exit 9; }
  ios_is_lock_error 'error: Signing for "Kinowo" requires a development team.' \
    && { echo "  FAIL signing error misclassified as lock"; exit 9; } || echo "  ok   ignores a non-lock error"

  # unlockedSinceBoot precheck against the captured lockState shape.
  printf '{"result":{"passcodeRequired":true,"unlockedSinceBoot":true}}'  > /tmp/devpanel-ls-ok.json
  printf '{"result":{"passcodeRequired":true,"unlockedSinceBoot":false}}' > /tmp/devpanel-ls-locked.json
  printf '{"result":{"passcodeRequired":false,"unlockedSinceBoot":false}}'> /tmp/devpanel-ls-nopass.json
  ios_unlocked_enough /tmp/devpanel-ls-ok.json     && echo "  ok   unlocked-since-boot ⇒ proceed"     || { echo "  FAIL ok-state blocked"; exit 9; }
  ios_unlocked_enough /tmp/devpanel-ls-nopass.json && echo "  ok   no-passcode ⇒ proceed"             || { echo "  FAIL nopass blocked"; exit 9; }
  ios_unlocked_enough /tmp/devpanel-ls-locked.json && { echo "  FAIL locked-since-boot not blocked"; exit 9; } || echo "  ok   locked-since-boot ⇒ wait"
) || fails=$((fails + 1))

echo "▶ android unlock wait (adb resolution)"
(
  SCRIPT_DIR="$SCRIPTS"; source "$SCRIPTS/lib.sh"
  out="$(DEVPANEL_ADB=/no/such/adb wait_for_android_unlock 2>&1)"; rc=$?
  [[ $rc -eq 0 && "$out" == *"skipping unlock wait"* ]] \
    && echo "  ok   skips gracefully when adb is missing" || { echo "  FAIL adb-missing rc=$rc out=$out"; exit 9; }
  fake="$(mktemp)"
  printf '#!/bin/sh\ncase "$1" in wait-for-device) exit 0;; shell) echo "mDreamingLockscreen=false";; esac\n' > "$fake"
  chmod +x "$fake"
  out="$(DEVPANEL_ADB="$fake" wait_for_android_unlock 2>&1)"; rc=$?
  rm -f "$fake"
  [[ $rc -eq 0 ]] && echo "  ok   proceeds when device reports unlocked" || { echo "  FAIL unlocked rc=$rc out=$out"; exit 9; }
) || fails=$((fails + 1))

echo "▶ runDevPanel.sh"
contains "runDevPanel → calls build.sh" "tools/devpanel/build.sh" "$(cat "$ROOT/runDevPanel.sh")"

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
for f in "$SCRIPTS"/*.sh "$ROOT/runDevPanel.sh"; do
  if bash -n "$f"; then printf '  ok   %s\n' "$(basename "$f")"
  else printf '  FAIL %s\n' "$(basename "$f")"; fails=$((fails + 1)); fi
done

echo "▶ swift compile + headless self-test"
bin="$(mktemp -t devpanel-bin)"
if swiftc -O -o "$bin" "$HERE/DevPanel/main.swift" 2>/tmp/devpanel-swiftc.log; then
  printf '  ok   compiles\n'
  out="$(DEVPANEL_SELFTEST=1 "$bin" 2>&1)"; rc=$?
  if [[ $rc -eq 0 && "$out" == *"SELFTEST_OK"* ]]; then
    printf '  ok   CommandRunner streams subprocess output (%s)\n' "$out"
  else
    printf '  FAIL self-test rc=%s out=%q\n' "$rc" "$out"; fails=$((fails + 1))
  fi
else
  printf '  FAIL compile\n'; sed 's/^/    /' /tmp/devpanel-swiftc.log; fails=$((fails + 1))
fi
rm -f "$bin"

echo
if [[ $fails -eq 0 ]]; then echo "✓ all DevPanel checks passed"; exit 0
else echo "✗ $fails check(s) failed"; exit 1; fi
