# Shared helpers for the DevPanel action scripts.
# Sourced (not executed) by each script; expects SCRIPT_DIR to be set first.

# Repo root is three levels up from tools/devpanel/scripts.
REPO_ROOT="$(cd "$SCRIPT_DIR/../../.." && pwd)"

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
