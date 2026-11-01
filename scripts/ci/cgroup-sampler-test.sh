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
# /proc/<pid>/stat: after `(comm)`, state is field 1, utime 12, stime 13, threads 18, rss (pages) 22.
stat_line() { printf '%s (%s) S 0 0 0 0 0 0 0 0 0 0 %s %s 0 0 0 0 %s 0 0 0 %s\n' "$1" "$2" "$3" "$4" "$5" "$6"; }
stat_line 101 java 100 20 30 250 > "$proc/101/stat"
stat_line 102 java 300 40 50 500 > "$proc/102/stat"
stat_line 103 mongod 70 10 9 125 > "$proc/103/stat"
printf '/usr/bin/java\0-Xmx4g\0xsbt.boot.Boot\0' > "$proc/101/cmdline"
printf 'cpu  100 0 50 800 20 0 5 7 0 0\ncpu0 1 0 0 0 0 0 0 0 0 0\n' > "$proc/stat"
printf '1.50 1.00 0.50 3/400 999\n' > "$proc/loadavg"
printf 'MemTotal:       16000000 kB\nMemAvailable:   12000000 kB\nSwapTotal:      4000000 kB\nSwapFree:       4000000 kB\n' > "$proc/meminfo"
mkdir -p "$proc/pressure" "$proc/net" "$proc/sys/fs"
printf 'some avg10=0.00 avg60=0.00 avg300=0.00 total=1234\nfull avg10=0.00 avg60=0.00 avg300=0.00 total=0\n' > "$proc/pressure/cpu"
printf 'some avg10=0.00 avg60=0.00 avg300=0.00 total=5000\nfull avg10=0.00 avg60=0.00 avg300=0.00 total=2000\n' > "$proc/pressure/memory"
printf '   8       0 sda 10 0 400 0 20 0 800 0 0 0 0\n   8       1 sda1 10 0 400 0 20 0 800 0 0 0 0\n' > "$proc/diskstats"
printf 'pgfault 1\noom_kill 2\n' > "$proc/vmstat"
printf 'Inter-|\n face |bytes\n    lo: 99 0 0 0 0 0 0 0 99 0\n  eth0: 1000 0 0 0 0 0 0 0 3000 0\n' > "$proc/net/dev"
printf '1536\t0\t9223372036854775807\n' > "$proc/sys/fs/file-nr"

export CGROUP_ROOT="$cg" PROC_ROOT="$proc" CGROUP_SAMPLER_OUT="$dir/max" SAMPLER_PAGE_SIZE=4096
"$sampler" start 1
wait_until "the sampler wrote its first sample" '[ -f "$dir/max" ]'
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
series_row="$(sed -n 2p "$dir/max.series")"
column() { awk -F'\t' -v name="$1" 'NR == 1 { for (i = 1; i <= NF; i++) if ($i == name) c = i } NR == 2 { print $c }' "$dir/max.series"; }
check "the series' header names every column the row fills" "$(head -1 "$dir/max.series" | awk -F'\t' '{ print NF }')" "$(awk -F'\t' '{ print NF }' <<< "$series_row")"
check "CPU: total and busy ticks, steal counted as busy" "982 162" "$(column cpu_total) $(column cpu_busy)"
check "the run queue is loadavg's running count" "3" "$(column procs_running)"
check "PSI: memory some and full totals" "5000 2000" "$(column psi_mem_some_us) $(column psi_mem_full_us)"
check "disk sectors count whole disks, not their partitions" "400 800" "$(column disk_read_sectors) $(column disk_write_sectors)"
check "network bytes leave out loopback" "1000 3000" "$(column net_rx_bytes) $(column net_tx_bytes)"
check "OOM kills and open files" "2 1536" "$(column oom_kill) $(column open_files)"
check "per command: processes, RSS, CPU ticks, threads" "2 3000 460 80" \
  "$(awk -F'\t' '$2 == "java" { print $3, $4, $5, $6; exit }' "$dir/max.procs")"
check "a JVM's command line is kept once" "1" "$(grep -c 'xsbt.boot.Boot' "$dir/max.jvms")"
check "an unknown command is refused" "2" "$("$sampler" nonsense >/dev/null 2>&1; echo $?)"

spec_summary
