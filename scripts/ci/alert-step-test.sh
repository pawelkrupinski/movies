#!/usr/bin/env bash
# alert-step.sh: an alert exit is a green step with alert=true and what it found; a pass is alert=false; any other
# failure fails the step. Run: bash scripts/ci/alert-step-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m alert-step.sh\n'
work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
step() { # <alert codes> <command...> -> "<exit> <outputs on one line>"
    local codes=$1 rc; shift
    : > "$work/out"
    GITHUB_OUTPUT="$work/out" bash "$REPO_ROOT/scripts/ci/alert-step.sh" "$codes" -- "$@" > "$work/log" 2>&1; rc=$?
    echo "$rc $(tr '\n' '|' < "$work/out")"
}

check "a pass is green, no alert" "0 alert=false|" "$(step 1 bash -c 'echo fine')"
check "an alert exit is green, with alert=true and the first annotation it printed" \
    "0 alert=true|found=DECODO DOWN: 0/7 ports|" \
    "$(step 1,2 bash -c 'echo probing; echo "::error::DECODO DOWN: 0/7 ports"; echo "::error::second"; exit 1')"
check "...an annotation with a title too" "0 alert=true|found=pl: 3 overdue|" \
    "$(step 1 bash -c 'echo "::warning title=x::pl: 3 overdue"; exit 1')"
check "...and with no annotation, its last line" "0 alert=true|found=no recording to report|" \
    "$(step 2 bash -c 'echo no recording to report; exit 2')"
check "any other exit is the check's own failure: the step fails with it" "3 alert=false|" \
    "$(step 1,2 bash -c 'echo "::error::Traceback"; exit 3')"
check "the command's output is shown" "1" "$(step 1 bash -c 'echo hello-there; exit 1' >/dev/null; grep -c '^hello-there$' "$work/log")"
check "no command is refused" "2" "$(bash "$REPO_ROOT/scripts/ci/alert-step.sh" 1 -- > /dev/null 2>&1; echo $?)"

spec_summary
