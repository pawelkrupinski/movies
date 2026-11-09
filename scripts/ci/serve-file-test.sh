#!/usr/bin/env bash
# scripts/ci/serve-file.sh: serves a directory on 127.0.0.1 and prints the URL of the named file once it answers; a
# missing file fails at once.
# Run: bash scripts/ci/serve-file-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m serve-file.sh\n'
work="$(mktemp -d)"
# shellcheck disable=SC2329 # run by the EXIT trap
cleanup() { [ -f "$work/served.http.pid" ] && kill "$(cat "$work/served.http.pid")" 2>/dev/null; rm -rf "$work"; }
trap cleanup EXIT
mkdir -p "$work/served"
printf 'launcher bytes\n' > "$work/served/cs.gz"
url="$(bash "$REPO_ROOT/scripts/ci/serve-file.sh" "$work/served" cs.gz)"; code=$?
local_url() { case "$1" in http://127.0.0.1:*/cs.gz) echo yes ;; *) echo no ;; esac; }
check "it prints a 127.0.0.1 URL of the file" "0 yes" "$code $(local_url "$url")"
check "...which serves the file's bytes" "launcher bytes" "$(curl -fsS --max-time 10 "$url")"
check "...its server's pid kept beside it, a live process" "yes" "$(kill -0 "$(cat "$work/served.http.pid")" 2>/dev/null && echo yes)"
check "a missing file fails, said, and serves nothing" "1 1" \
  "$(bash "$REPO_ROOT/scripts/ci/serve-file.sh" "$work/served" none.gz > /dev/null 2> "$work/err"; echo $?) $(grep -c 'no .*none.gz to serve' "$work/err")"
spec_summary
