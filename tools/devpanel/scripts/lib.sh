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

# kill_pattern <pgrep-pattern> <label>
#
# SIGTERM (then SIGKILL) every process whose FULL command line matches
# <pgrep-pattern>, except this script's own process tree. The companion to
# free_port for processes that bind no port: a stale `sbt localStack` worker
# is a forked JVM with no listening socket, so lsof/free_port can't see it.
# We can't track it by pid file either — the worker is a grandchild forked by
# sbt's bgRunMain, so the launcher script never learns its pid; the one stable
# handle is its unique main-class on the JVM command line. No-op under
# DEVPANEL_PRINT_ONLY.
kill_pattern() {
  local pattern="$1" label="$2"
  [[ "${DEVPANEL_PRINT_ONLY:-}" == "1" ]] && return 0

  # pgrep -f matches the whole command line; exclude our own pid + parent so a
  # launcher whose argv happens to contain the pattern can't kill itself.
  local pids
  pids="$(pgrep -f "$pattern" 2>/dev/null | grep -vx -e "$$" -e "$PPID" || true)"
  [[ -z "$pids" ]] && return 0

  echo "  killing stale $label ($(echo "$pids" | tr '\n' ' '))"
  kill $pids 2>/dev/null || true
  for _ in 1 2 3 4 5 6; do
    sleep 0.3
    pids="$(pgrep -f "$pattern" 2>/dev/null | grep -vx -e "$$" -e "$PPID" || true)"
    [[ -z "$pids" ]] && return 0
  done
  kill -9 $pids 2>/dev/null || true
}

# kill_stale_worker
#
# Reap a stale local-stack fixture worker (the forked JVM `sbt localStack`
# leaves behind). Both the web-server and web+worker actions call this so a
# previous run's worker can't keep projecting fixtures into the local Mongo
# under a fresh stack. No-op under DEVPANEL_PRINT_ONLY.
kill_stale_worker() {
  kill_pattern "modules.LocalFixtureWorkerMain" "local fixture worker"
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

# resolve_adb — echo a usable adb path, or nothing. Order: $DEVPANEL_ADB, an
# adb on PATH, the SDK from android/local.properties' sdk.dir, then the common
# default SDK locations.
resolve_adb() {
  if [[ -n "${DEVPANEL_ADB:-}" ]]; then echo "$DEVPANEL_ADB"; return 0; fi
  command -v adb 2>/dev/null && return 0
  local sdk cand
  sdk="$(sed -n 's/^sdk\.dir=//p' "$REPO_ROOT/android/local.properties" 2>/dev/null | head -1)"
  for cand in "$sdk/platform-tools/adb" "${ANDROID_HOME:-}/platform-tools/adb" \
              "${ANDROID_SDK_ROOT:-}/platform-tools/adb" "$HOME/Library/Android/sdk/platform-tools/adb"; do
    [[ -n "$cand" && -x "$cand" ]] && { echo "$cand"; return 0; }
  done
  return 0
}

# android_serial — echo the device serial to target. $DEVPANEL_ANDROID_SERIAL
# wins; else the single attached device; else the first of several (with a note
# to stderr so you can override). Empty if adb/no device.
android_serial() {
  [[ -n "${DEVPANEL_ANDROID_SERIAL:-}" ]] && { echo "$DEVPANEL_ANDROID_SERIAL"; return 0; }
  local adb; adb="$(resolve_adb)"
  command -v "$adb" >/dev/null 2>&1 || return 0
  local serials n
  serials="$("$adb" devices 2>/dev/null | awk '$2=="device"{print $1}')"
  n="$(printf '%s\n' "$serials" | grep -c .)"
  if [[ "$n" -gt 1 ]]; then
    echo "  multiple devices attached: $(echo $serials) — using the first; set DEVPANEL_ANDROID_SERIAL to pick" >&2
    printf '%s\n' "$serials" | head -1
  elif [[ "$n" -eq 1 ]]; then
    printf '%s\n' "$serials"
  fi
}

# android_device_state [serial] — echo the adb connection state of the target
# device: "device" (authorized + ready), "unauthorized" (USB-debugging prompt
# not yet accepted), "offline" (reconnecting), or empty when no matching device
# is attached. With a serial, reports that device; without, the single/first
# attached one. Empty when adb can't be resolved.
android_device_state() {
  local serial="${1:-}" adb; adb="$(resolve_adb)"
  { [[ -z "$adb" ]] || ! command -v "$adb" >/dev/null 2>&1; } && return 0
  "$adb" devices 2>/dev/null | awk -v s="$serial" '
    NR==1 { next }                          # skip "List of devices attached"
    NF < 2 { next }                         # skip blank lines / daemon noise
    s != "" { if ($1 == s) { print $2; exit } next }
    { print $2; exit }                      # no serial: first attached device
  '
}

# wait_for_android_unlock [serial]
#
# Block until the (optionally serial-pinned) cabled Android device is authorized
# AND its keyguard is dismissed. First waits for the device to reach adb's
# "device" state: a plugged-in phone that hasn't had its "Allow USB debugging"
# prompt accepted sits in "unauthorized" (and "offline" while reconnecting), and
# `adb wait-for-device` blocks SILENTLY forever on both — the exact hang that
# leaves the panel stuck at a blank banner. So we poll the state ourselves and
# tell the user what to do instead. Then checks dumpsys window's
# lockscreen/keyguard flag; if the device exposes no recognised flag we don't
# block (degrade to "assume unlocked") rather than hang forever. No-op under
# DEVPANEL_PRINT_ONLY.
wait_for_android_unlock() {
  [[ "${DEVPANEL_PRINT_ONLY:-}" == "1" ]] && { echo "wait_for_android_unlock"; return 0; }
  local serial="${1:-}" adb
  adb="$(resolve_adb)"
  if [[ -z "$adb" ]] || ! command -v "$adb" >/dev/null 2>&1; then
    echo "  (adb not found — skipping unlock wait; set DEVPANEL_ADB or ANDROID_HOME)"
    return 0
  fi

  local state announced_state=
  while :; do
    state="$(android_device_state "$serial")"
    [[ "$state" == device ]] && { [[ -n "$announced_state" ]] && echo "  device ready."; break; }
    case "$state" in
      unauthorized) [[ "$announced_state" != unauthorized ]] && \
        echo "🔒 Android device is unauthorized — accept the “Allow USB debugging” prompt on the device…" ;;
      offline)      [[ "$announced_state" != offline ]] && \
        echo "⏳ Android device is offline — reconnecting…" ;;
      *)            [[ "$announced_state" != absent ]] && \
        echo "🔌 waiting for an Android device to be attached…" ;;
    esac
    announced_state="${state:-absent}"
    sleep 2
  done

  local s=""; [[ -n "$serial" ]] && s="-s $serial"   # serials have no spaces
  local announced= win lock
  while :; do
    win="$("$adb" $s shell dumpsys window 2>/dev/null)"
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
      if [[ -z "$announced" ]]; then echo "🔒 iPhone is locked — the app will launch as soon as you unlock it…"; announced=1; fi
      sleep 2
    else
      return $rc
    fi
  done
}
