#!/usr/bin/env bash
# cpu-sampler.sh: the runner-wide CPU line a convergence leg prints beside its phase log.
# Run: bash scripts/ci/cpu-sampler-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m cpu-sampler.sh\n'
sampler="$REPO_ROOT/scripts/ci/cpu-sampler.sh"
proc="$(mktemp -d)"; trap 'rm -rf "$proc"' EXIT

# A 4-core box: cpu user nice system idle iowait irq softirq steal.
printf 'cpu  1000 0 200 3000 100 0 0 0\ncpu0 1 0 0 0 0\ncpu1 1 0 0 0 0\ncpu2 1 0 0 0 0\ncpu3 1 0 0 0 0\nintr 0\n' > "$proc/stat"
# Two JVM threads' worth of processes and one mongod; utime and stime are fields 14 and 15.
stat_line() { echo "$1 ($2) S 1 1 1 0 -1 0 0 0 0 0 $3 $4 0 0 20 0 1 0 0 0 0"; }
mkdir -p "$proc/101" "$proc/102" "$proc/103" "$proc/104"
stat_line 101 java 500 100 > "$proc/101/stat"
stat_line 102 java 50 50   > "$proc/102/stat"
stat_line 103 mongod 300 20 > "$proc/103/stat"
stat_line 104 "Web Content" 999 999 > "$proc/104/stat"

check "a reading names cores, busy, user, system, iowait, total, then java and mongod" \
  "4 1200 1000 200 100 4300 700 320" "$(PROC_ROOT="$proc" "$sampler" read)"

# 10 s on 4 cores at 100 ticks/s is 4000 ticks: 3000 busy (2600 user, 400 system), 200 iowait.
check "a report reads the interval in cores, the machine's and each process's" \
  "[cpu] busy 3.0 of 4 cores (user 2.6, system 0.4, iowait 0.2); java 2.4, mongod 0.5" \
  "$("$sampler" report 10 "4 0 0 0 0 0 0 0" "4 3000 2600 400 200 4000 2400 500")"

check "an interval that is not seconds is refused" "64" "$("$sampler" 1x >/dev/null 2>&1; echo $?)"

spec_summary
