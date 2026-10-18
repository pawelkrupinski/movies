#!/usr/bin/env bash
# memory-gate.sh: a JVM start waits while too much memory is paged out, says so once, opens when it drains, goes ahead
# warned after its deadline, and is off on request or where nothing can be read. The paged-out reading is stubbed.
# Run: bash scripts/memory-gate-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"
. "$REPO_ROOT/scripts/memory-gate.sh"

printf '\033[36m▸\033[0m memory-gate\n'
work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
export KINOWO_MEMORY_GATE=1 KINOWO_MEMORY_GATE_POLL=0.05 KINOWO_MEMORY_GATE_GB=20

# the probe reads the next value from a file per call: the paged-out GB as each poll would see it
probe() { printf '%s\n' "$@" > "$work/readings"; : > "$work/calls"; }
export KINOWO_MEMORY_GATE_PROBE='echo x >> "'"$work"'/calls"; n=$(wc -l < "'"$work"'/calls"); sed -n "${n}p" "'"$work"'/readings"'

probe 5
memory_gate t 2> "$work/err"
check "under the budget: starts at once, silently" "1|" "$(wc -l < "$work/calls" | tr -d ' ')|$(cat "$work/err")"

probe 48 48 30 12
memory_gate t 2> "$work/err"
check "over the budget: waits until the paged-out memory drains under it" "4" "$(wc -l < "$work/calls" | tr -d ' ')"
check "...saying so once, and when it opens" "1 1" \
  "$(grep -c 'waiting for other JVMs' "$work/err") $(grep -c 'open after' "$work/err")"

probe 48 48 48 48 48 48 48 48 48 48 48 48 48 48 48 48 48 48 48 48
KINOWO_MEMORY_GATE_MAX_WAIT=0 memory_gate t 2> "$work/err"; status=$?
check "past its deadline: goes ahead, warned, never fails the tool" "0 1" "$status $(grep -c 'starting anyway' "$work/err")"

probe 48
KINOWO_MEMORY_GATE=0 memory_gate t 2> "$work/err"
check "KINOWO_MEMORY_GATE=0: off, the reading never taken" "0" "$(wc -l < "$work/calls" | tr -d ' ')"

probe ""
memory_gate t 2> "$work/err"; status=$?
check "nothing readable (Linux CI): starts at once" "0|" "$status|$(cat "$work/err")"

spec_summary
