#!/usr/bin/env bash
# check() evals its condition, so it is quoted unexpanded and $code is read there
# shellcheck disable=SC2016,SC2034
# Self-test for scripts/ci/order-passes-present.sh.
set -uo pipefail
here=$(cd "$(dirname "$0")" && pwd)
tool="$here/order-passes-present.sh"
work=$(mktemp -d); trap 'rm -rf "$work"' EXIT
fail=0
check() { if eval "$2"; then echo "ok - $1"; else echo "not ok - $1"; fail=1; fi; }

mkdir -p "$work/both" "$work/one" "$work/rt"
echo x > "$work/both/pass0.bin.gz"; echo x > "$work/both/pass1.bin.gz"
echo x > "$work/one/pass1.bin.gz"     # Verify scrape recording #14: p01-pass0 OOM-killed, only pass1 arrived

RUNNER_TEMP="$work/rt" "$tool" "$work/both" 0,1 order-independence-p01 > "$work/out" 2>&1; code=$?
check "both passes present: the comparison runs (exit 0)" '[ "$code" = 0 ]'
check "both passes present: no verdict written" '[ ! -e "$work/rt/leg-verdict-why" ]'

RUNNER_TEMP="$work/rt" "$tool" "$work/one" 0,1 order-independence-p01 > "$work/out" 2>&1; code=$?
check "a pass missing: refused (exit 1)" '[ "$code" = 1 ]'
check "a pass missing: the ::error:: names the row that did not finish" 'grep -q "^::error::pass row(s) order-independence-p01-pass0 did not finish" "$work/out"'
check "a pass missing: the leg verdict names it" 'grep -q "order-independence-p01-pass0 did not finish" "$work/rt/leg-verdict-why"'
check "a pass missing: the present pass is not named" '! grep -q "pass1" "$work/rt/leg-verdict-why"'

RUNNER_TEMP="$work/rt" "$tool" "$work/none" 1,2 order-independence-p12 > "$work/out" 2>&1; code=$?
check "no directory at all: both passes named" '[ "$code" = 1 ] && grep -q "order-independence-p12-pass1 order-independence-p12-pass2" "$work/out"'

: > "$work/one/pass0.bin.gz"
RUNNER_TEMP="$work/rt" "$tool" "$work/one" 0,1 order-independence-p01 > "$work/out" 2>&1; code=$?
check "an empty pass file counts as missing" '[ "$code" = 1 ]'
exit "$fail"
