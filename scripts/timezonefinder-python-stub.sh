# Purpose: A python3 like Homebrew's for the specs of scripts/timezonefinder-python and the pre-push drift check: it has no timezonefinder
# shellcheck shell=bash
# A python3 like Homebrew's for the specs of scripts/timezonefinder-python and the pre-push drift check: it has no
# timezonefinder; `-m venv DIR` makes a venv whose python3 imports it once that venv's pip has installed the lock
# (STUB_PIP_FAIL set: the install fails as a missing wheel would). Every venv build appends a line to $STUB_BUILDS.
# An isolated run (`python3 -I ...`: tool-conventions.sh's shims over scripts/lib) goes to the python3 the PATH had
# when the stub was made, so `bounded` still works under it.
#   timezonefinder_python_stub <dir>   -> <dir>/bin/python3; `: > <dir>/installed` gives that python3 timezonefinder
timezonefinder_python_stub() { # <dir>
  mkdir -p "$1/bin"
  { echo '#!/bin/sh'; echo "real_python3='$(command -v python3)'"; cat; } > "$1/bin/python3" <<'STUB'
case "$1" in
  -I) exec "$real_python3" "$@" ;;
  -c) case "$2" in *version_info*) echo 3.15; exit 0 ;; esac
      [ -f "$(dirname "$0")/../installed" ] && exit 0
      echo "ModuleNotFoundError: No module named 'timezonefinder'" >&2; exit 1 ;;
  -m) case "$2" in
        venv) mkdir -p "$3/bin"; cp "$0" "$3/bin/python3"; echo build >> "${STUB_BUILDS:-/dev/null}"; exit 0 ;;
        pip) [ -n "${STUB_PIP_FAIL:-}" ] && { echo "ERROR: No matching distribution found for numpy==2.5.5" >&2; exit 1; }
             : > "$(dirname "$0")/../installed"; exit 0 ;;
      esac ;;
esac
exit 2
STUB
  chmod +x "$1/bin/python3"
}
