# Shared helpers for the DevPanel action scripts.
# Sourced (not executed) by each script; expects SCRIPT_DIR to be set first.

# A GUI-launched .app inherits a minimal PATH (no Homebrew, no Android SDK),
# so sbt / adb / gradle aren't found the way they are in a terminal. Append the
# usual dev-tool locations (existing dirs not already on PATH).
_devpanel_extra_paths=(/opt/homebrew/bin /usr/local/bin "$HOME/Library/Android/sdk/platform-tools")
[[ -n "${ANDROID_HOME:-}" ]] && _devpanel_extra_paths+=("$ANDROID_HOME/platform-tools")
[[ -n "${ANDROID_SDK_ROOT:-}" ]] && _devpanel_extra_paths+=("$ANDROID_SDK_ROOT/platform-tools")
for _p in "${_devpanel_extra_paths[@]}"; do
  [[ -d "$_p" && ":$PATH:" != *":$_p:"* ]] && PATH="$PATH:$_p"
done
export PATH

# Repo root: a worktree path the panel passes via DEVPANEL_REPO_ROOT (the
# long-press "run on worktree" menu), else three levels up from this script.
REPO_ROOT="${DEVPANEL_REPO_ROOT:-$(cd "$SCRIPT_DIR/../../.." && pwd)}"

# free_port <port>
#
# Kill whatever is LISTENing on <port> so a fresh `sbt web/run` can bind it
# (Play refuses to start if :9000 is already taken). SIGTERM first, escalate
# to SIGKILL if it's still holding the port. No-op under DEVPANEL_PRINT_ONLY.
free_port() {
  local port="$1"
  [[ "${DEVPANEL_PRINT_ONLY:-}" == "1" ]] && return 0

  local pids
  pids="$(lsof -ti "tcp:$port" -sTCP:LISTEN 2>/dev/null || true)"
  [[ -z "$pids" ]] && return 0

  echo "  freeing :$port (killing $(echo "$pids" | tr '\n' ' '))"
  kill $pids 2>/dev/null || true
  for _ in 1 2 3 4 5 6; do
    sleep 0.3
    pids="$(lsof -ti "tcp:$port" -sTCP:LISTEN 2>/dev/null || true)"
    [[ -z "$pids" ]] && return 0
  done
  kill -9 $pids 2>/dev/null || true
}

# dispatch <workdir> <label> <cmd...>
#
# Normal mode: announce, cd into <workdir>, and `exec` the command so a
# long-running `sbt`/`gradle` process owns the terminal (Ctrl-C reaches it).
#
# DEVPANEL_PRINT_ONLY=1: print the exact `cd <dir> && <cmd>` line and return
# without running anything. This is what test.sh asserts against, so the
# printed form is stable and unquoted (commands here never contain spaces in
# a single argument except the dynamic iOS destination, which is excluded).
dispatch() {
  local workdir="$1"; shift
  local label="$1"; shift

  if [[ "${DEVPANEL_PRINT_ONLY:-}" == "1" ]]; then
    printf 'cd %s && %s\n' "$workdir" "$*"
    return 0
  fi

  printf '\033[1m▶ %s\033[0m\n' "$label"
  printf '  dir: %s\n  cmd: %s\n\n' "$workdir" "$*"
  cd "$workdir"
  exec "$@"
}

# step <cmd...>
#
# Announce and run one command in a multi-step script (no `exec`, so several
# steps can run in sequence). DEVPANEL_PRINT_ONLY=1 prints the command instead
# of running it — test.sh asserts on those lines.
step() {
  if [[ "${DEVPANEL_PRINT_ONLY:-}" == "1" ]]; then
    printf '%s\n' "$*"
    return 0
  fi
  printf '\n\033[1m▶ %s\033[0m\n' "$*"
  "$@"
}

# wait_for_android_unlock
#
# Block until the cabled Android device is present and its keyguard is
# dismissed. Uses dumpsys window's lockscreen/keyguard flag; if the device
# exposes no recognised flag we don't block (degrade to "assume unlocked")
# rather than hang forever. No-op under DEVPANEL_PRINT_ONLY.
wait_for_android_unlock() {
  [[ "${DEVPANEL_PRINT_ONLY:-}" == "1" ]] && { echo "wait_for_android_unlock"; return 0; }
  local adb="${DEVPANEL_ADB:-adb}"
  if ! command -v "$adb" >/dev/null 2>&1; then
    echo "  (adb not on PATH — skipping unlock wait; set ANDROID_HOME if your SDK is elsewhere)"
    return 0
  fi
  "$adb" wait-for-device
  local announced= win lock
  while :; do
    win="$("$adb" shell dumpsys window 2>/dev/null)"
    lock="$(printf '%s' "$win" | grep -oE 'mDreamingLockscreen=(true|false)' | head -1)"
    [[ -z "$lock" ]] && lock="$(printf '%s' "$win" | grep -oE 'mKeyguardShowing=(true|false)' | head -1)"
    case "$lock" in
      *=false|"") [[ -n "$announced" ]] && echo "  unlocked."; return 0 ;;
    esac
    if [[ -z "$announced" ]]; then echo "🔒 waiting for Android unlock…"; announced=1; fi
    sleep 2
  done
}

# ios_unlocked_enough <lockState-json-file>  → exit 0 if devicectl can write to
# the device: either no passcode is set, or it has been unlocked at least once
# since boot (the data partition is available). This is the one documented gate
# that actually blocks `devicectl install`; a screen lock *after* first unlock
# does not. Captured field shape (xcrun devicectl device info lockState):
#   result: { passcodeRequired: bool, unlockedSinceBoot: bool }
ios_unlocked_enough() {
  /usr/bin/python3 -c 'import json,sys
d=json.load(open(sys.argv[1])).get("result",{})
sys.exit(0 if (not d.get("passcodeRequired",False)) or d.get("unlockedSinceBoot",False) else 1)' "$1" 2>/dev/null
}

# wait_for_ios_unlock <udid>
#
# Block until the cabled iPhone has been unlocked since boot (see above). On a
# device set to never auto-lock this returns immediately. No-op under
# DEVPANEL_PRINT_ONLY.
wait_for_ios_unlock() {
  local udid="$1"
  [[ "${DEVPANEL_PRINT_ONLY:-}" == "1" ]] && { echo "wait_for_ios_unlock $udid"; return 0; }
  local announced= tmp; tmp="$(mktemp)"
  while :; do
    if xcrun devicectl device info lockState --device "$udid" --json-output "$tmp" -q 2>/dev/null \
       && ios_unlocked_enough "$tmp"; then
      [[ -n "$announced" ]] && echo "  unlocked."
      rm -f "$tmp"; return 0
    fi
    [[ -z "$announced" ]] && { echo "🔒 waiting for iPhone to be unlocked…"; announced=1; }
    sleep 2
  done
}

# ios_is_lock_error <text>  → exit 0 if <text> is devicectl's device-locked
# error. `devicectl device process launch` is the step that fails on a locked
# iPhone (install succeeds); patterns below are taken verbatim from a real
# captured failure:
#   "Unable to launch … because the device was not, or could not be, unlocked."
#   "BSErrorCodeDescription = Locked" / "Reason: Locked"
ios_is_lock_error() {
  printf '%s' "$1" | grep -qiE 'could not be,? unlocked|was not,? or could not be|BSErrorCodeDescription = Locked|reason: locked|device is locked|unlock the device'
}

# ios_run_unlocked <cmd...>
#
# Run a devicectl command, retrying for as long as it fails with a
# device-locked error — i.e. waiting for the iPhone to be unlocked. Any other
# failure aborts immediately. Under DEVPANEL_PRINT_ONLY it just prints the step.
ios_run_unlocked() {
  if [[ "${DEVPANEL_PRINT_ONLY:-}" == "1" ]]; then step "$@"; return 0; fi
  local announced= out rc
  while :; do
    printf '\n\033[1m▶ %s\033[0m\n' "$*"
    out="$("$@" 2>&1)"; rc=$?
    printf '%s\n' "$out"
    [[ $rc -eq 0 ]] && { [[ -n "$announced" ]] && echo "  unlocked."; return 0; }
    if ios_is_lock_error "$out"; then
      if [[ -z "$announced" ]]; then echo "🔒 waiting for iPhone unlock…"; announced=1; fi
      sleep 2
    else
      return $rc
    fi
  done
}
