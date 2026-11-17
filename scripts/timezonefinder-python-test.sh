#!/usr/bin/env bash
#
# scripts/timezonefinder-python against a stubbed python3: the PATH's own python3 when it imports timezonefinder; else
# the pinned venv built once and reused, a new one when the lock changes; a build that fails says so and exits 1.
#
#   scripts/timezonefinder-python-test.sh
#
set -uo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
. "$HERE/shell-spec.sh"

printf '\033[36m▸\033[0m timezonefinder-python\n'
base="$(mktemp -d)"
trap 'rm -rf "$base"' EXIT
# The tool's copy beside its own lock file, so a case can change the lock.
mkdir -p "$base/scripts"
cp "$HERE/timezonefinder-python" "$HERE/timezonefinder-requirements.txt" "$HERE/tool-conventions.sh" "$base/scripts/"
tool="$base/scripts/timezonefinder-python"
export KINOWO_TIMEZONEFINDER_VENVS="$base/venvs"
. "$HERE/timezonefinder-python-stub.sh"
timezonefinder_python_stub "$base"
export STUB_BUILDS="$base/builds"
run() { PATH="$base/bin:$PATH" "$tool" 2>"$base/err"; }

out="$(run)"; rc=$?
check "no timezonefinder on PATH's python3: the pinned venv is built, its bin printed" "0 1" \
  "$rc $(case "$out" in "$base/venvs/py3.15-"*/bin) echo 1 ;; *) echo "0 ($out)" ;; esac)"
check "...whose python3 imports timezonefinder" "0" "$("$out/python3" -c 'import timezonefinder' 2>/dev/null; echo $?)"
check "...and no build directory left behind" "0" "$(find "$base/venvs" -maxdepth 1 -name '.build.*' | wc -l | tr -d ' ')"
first="$out"
check "a second run reuses it: no second build" "$first 1" "$(run) $(wc -l < "$base/builds" | tr -d ' ')"
echo "# a bump" >> "$base/scripts/timezonefinder-requirements.txt"
out="$(run)"
check "a changed lock builds a venv of its own" "1 2" "$([ "$out" != "$first" ] && echo 1) $(wc -l < "$base/builds" | tr -d ' ')"
echo "# another bump" >> "$base/scripts/timezonefinder-requirements.txt"
out="$(STUB_PIP_FAIL=1 run)"; rc=$?
check "a build that fails: exit 1, nothing printed, pip's error and the rerun command said" "1 - 1 1" \
  "$rc ${out:--} $(grep -c 'No matching distribution' "$base/err") $(grep -c 'run scripts/timezonefinder-python again' "$base/err")"
check "...and the failed build leaves nothing in place" "2" "$(find "$base/venvs" -mindepth 1 -maxdepth 1 | wc -l | tr -d ' ')"
: > "$base/installed" # the stub python3's own install
check "PATH's python3 imports timezonefinder (CI's): its own directory, no venv" "$base/bin 3" \
  "$(run) $(wc -l < "$base/builds" | tr -d ' ')"
spec_summary
