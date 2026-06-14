# Shared helpers for the DevPanel action scripts.
# Sourced (not executed) by each script; expects SCRIPT_DIR to be set first.

# Repo root is three levels up from tools/devpanel/scripts.
REPO_ROOT="$(cd "$SCRIPT_DIR/../../.." && pwd)"

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
