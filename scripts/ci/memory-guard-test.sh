#!/usr/bin/env bash
# memory-guard.sh: ends a process tree that would exhaust the runner's memory, naming why; says how it is doing.
# Run: bash scripts/ci/memory-guard-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m memory-guard.sh\n'
guard="$REPO_ROOT/scripts/ci/memory-guard.sh"
dir="$(mktemp -d)"; trap 'kill $(cat "$dir"/*.pid 2>/dev/null) 2>/dev/null; rm -rf "$dir"' EXIT
mkdir -p "$dir/proc"
meminfo() { printf 'MemTotal:       16000000 kB\nMemAvailable:   %s kB\nSwapTotal:       4000000 kB\nSwapFree:        3000000 kB\n' "$1" > "$dir/proc/meminfo"; }
export PROC_ROOT="$dir/proc" MEMORY_GUARD_INTERVAL=0.1 MEMORY_GUARD_STRIKES=2

# A target with a child of its own, as sbt's launcher is java's parent.
start_tree() { # <name>
  bash -c 'sleep 300 & echo $! > "$1.child.pid"; wait' _ "$dir/$1" &
  echo $! > "$dir/$1.pid"
  wait_until "the $1 tree started its child" "[ -s '$dir/$1.child.pid' ]"
}

# Memory runs low: the tree ends, and the reason file names the cause.
meminfo 500000
start_tree low
echo "## us pending [seed 1]: pending at projection" > "$dir/progress.log"
out=$(MEMORY_GUARD_HEARTBEAT=1000 timeout 60 "$guard" "$(cat "$dir/low.pid")" "$dir/low.reason" "$dir/progress.log")
check "low memory: the guard writes why" "1" "$(grep -c 'under 1024 MB available for 2 samples' "$dir/low.reason" 2>/dev/null)"
check "...naming the machine's memory and the replay's last line" "1" \
  "$(grep -c '15137 MB in use of 15625, 488 MB available, 977 MB swap used.*last: ## us pending \[seed 1\]' "$dir/low.reason")"
check "...says it in the log" "1" "$(grep -c '^memory guard: ENDING process' <<< "$out")"
check "...the watched process ends" "gone" "$(kill -0 "$(cat "$dir/low.pid")" 2>/dev/null && echo alive || echo gone)"
check "...and so does its child" "gone" "$(kill -0 "$(cat "$dir/low.child.pid")" 2>/dev/null && echo alive || echo gone)"

# Plenty of memory: the guard prints its heartbeat and ends with the target, writing no reason.
meminfo 9000000
bash -c 'sleep 1' & echo $! > "$dir/fine.pid"
out=$(MEMORY_GUARD_HEARTBEAT=2 MEMORY_GUARD_LABEL="memory guard (us takes-drift-1)" timeout 60 "$guard" "$(cat "$dir/fine.pid")" "$dir/fine.reason" "$dir/progress.log")
check "plenty of memory: no reason written" "no" "$([ -e "$dir/fine.reason" ] && echo yes || echo no)"
check "...a heartbeat names the memory and the last progress line" "yes" \
  "$(grep -q '^memory guard (us takes-drift-1): 0 min, 6836 MB in use of 15625, 8789 MB available, 977 MB swap used -- last: ## us pending' <<< "$out" && echo yes || echo no)"

# No meminfo (macOS): it only waits for the target.
rm -f "$dir/proc/meminfo"
bash -c 'sleep 0.5' & echo $! > "$dir/mac.pid"
timeout 60 "$guard" "$(cat "$dir/mac.pid")" "$dir/mac.reason"
check "no meminfo: the guard returns once the target ends" "0 no" "$? $([ -e "$dir/mac.reason" ] && echo yes || echo no)"

# Stopping the guard, under `bash -e` as a workflow step runs: the guard exits by itself once its target is gone, so a
# stop that finds it already gone must not fail the step (Identity model convergence #1342: a passing US suite ended
# exit 1 there, `kill "$guard"` the failing last command of an `||` list).
stop="$REPO_ROOT/scripts/ci/memory-guard-stop.sh"
stopped() { # <how the guard behaves> <reason file content> -> "exit <status>, guard <alive|gone>"
  bash -e -c '
    . "$1"
    case "$2" in gone) true & ;; idle) sleep 300 & ;; esac
    guard=$!
    if [ "$2" = gone ]; then wait "$guard"; fi
    printf "%s" "$3" > "$4"
    memory_guard_stop "$guard" "$4"
    kill -0 "$guard" 2>/dev/null && echo "guard alive" || echo "guard gone"
  ' _ "$stop" "$1" "$2" "$dir/stop.reason" 2>&1
  echo "exit $?"
}
check "stop: a guard that already exited does not fail a bash -e step" "guard gone exit 0" "$(stopped gone '' | tr '\n' ' ' | sed 's/ $//')"
check "stop: an idle guard is ended" "guard gone exit 0" "$(stopped idle '' | tr '\n' ' ' | sed 's/ $//')"
check "stop: a guard that is ending the tree (a reason written) is waited for, not killed" "guard gone exit 0" \
  "$(stopped gone 'under 1024 MB available' | tr '\n' ' ' | sed 's/ $//')"
for caller in .github/workflows/country-convergence-leg.yml scripts/ci/identity-drift-check.sh; do
  # shellcheck disable=SC2016 # the callers' own text, `$guard` as written there
  check "$caller stops its guard through memory_guard_stop" "1 0" \
    "$(grep -c 'memory_guard_stop "\$guard"' "$REPO_ROOT/$caller") $(grep -c 'kill "\$guard"' "$REPO_ROOT/$caller")"
done

spec_summary
