# Shared helpers for the DevPanel action scripts.
# Sourced (not executed) by each script; expects SCRIPT_DIR to be set first.

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
  adb wait-for-device
  local announced= win lock
  while :; do
    win="$(adb shell dumpsys window 2>/dev/null)"
    lock="$(printf '%s' "$win" | grep -oE 'mDreamingLockscreen=(true|false)' | head -1)"
    [[ -z "$lock" ]] && lock="$(printf '%s' "$win" | grep -oE 'mKeyguardShowing=(true|false)' | head -1)"
    case "$lock" in
      *=false|"") [[ -n "$announced" ]] && echo "  unlocked."; return 0 ;;
    esac
    if [[ -z "$announced" ]]; then echo "🔒 waiting for Android unlock…"; announced=1; fi
    sleep 2
  done
}

# ios_is_lock_error <text>  → exit 0 if <text> looks like a device-locked
# error from devicectl. Kept separate so test.sh can assert the classification
# against real captured output.
ios_is_lock_error() {
  printf '%s' "$1" | grep -qiE 'is locked|unlock the device|passcode|device.*lock|NoConnection.*lock'
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
