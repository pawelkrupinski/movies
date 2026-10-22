#!/usr/bin/env bash
# cgroup-sampler.sh: the job cgroup's memory and disk writes a convergence row reports last.
# Run: bash scripts/ci/cgroup-sampler-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m cgroup-sampler.sh\n'
sampler="$REPO_ROOT/scripts/ci/cgroup-sampler.sh"
dir="$(mktemp -d)"; trap 'kill "$(cat "$dir/pid" 2>/dev/null)" 2>/dev/null; rm -rf "$dir"' EXIT
cg="$dir/cg" proc="$dir/proc"; mkdir -p "$cg" "$proc/101" "$proc/102" "$proc/103"

printf 'anon 9000000000\nfile 3000000000\nshmem 2000000000\n' > "$cg/memory.stat"
echo 17179869184 > "$cg/memory.max"
echo 16000000000 > "$cg/memory.peak"
printf 'low 0\nhigh 0\nmax 0\noom 0\noom_kill 0\n' > "$cg/memory.events"
printf '259:0 rbytes=1 wbytes=3000000000 rios=1 wios=1\n9:3 rbytes=1 wbytes=1000000000 rios=1 wios=1\n' > "$cg/io.stat"
printf 'Name:\tjava\nVmRSS:\t1000 kB\n' > "$proc/101/status"
printf 'Name:\tjava\nVmRSS:\t2000 kB\n' > "$proc/102/status"
printf 'Name:\tmongod\nVmRSS:\t500 kB\n' > "$proc/103/status"

export CGROUP_ROOT="$cg" PROC_ROOT="$proc" CGROUP_SAMPLER_OUT="$dir/max"
"$sampler" start 1
for _ in 1 2 3 4 5 6 7 8 9 10; do [ -f "$dir/max" ] && break; sleep 0.2; done
pgrep -f "cgroup-sampler.sh start 1" | head -1 > "$dir/pid"
report="$("$sampler" report)"

check "the maxima sum anon and shmem, the memory that cannot be reclaimed" \
  "[cgroup] max anon+shmem 11000000000 (11.00 GB)" "$(grep 'anon+shmem' <<< "$report")"
check "each process's RSS is the sum over its processes" \
  "[cgroup] max java-rss 3072000 (0.00 GB)" "$(grep 'java-rss' <<< "$report")"
check "memory.peak is reported against memory.max" \
  "[cgroup] memory.max 17179869184 (17.18 GB)" "$(grep 'memory.max' <<< "$report")"
check "the md device's writes are reported on a line of their own" \
  "[cgroup] io.stat 9:3 wbytes 1000000000 (1.00 GB)" "$(grep 'io.stat 9:3' <<< "$report")"
check "an unknown command is refused" "2" "$("$sampler" nonsense >/dev/null 2>&1; echo $?)"

spec_summary
