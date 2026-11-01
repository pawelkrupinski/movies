#!/usr/bin/env bash
# The job's own memory and disk writes, read from its cgroup (the job container's, which holds the job's dockerd
# and the mongod container inside it) -- and, beside them, the machine's resources as a time series, for the job's
# ci-metrics artifact (scripts/ci/job_metrics.py, .github/actions/job-telemetry, scripts/ci/ci_trends.py).
#
#   cgroup-sampler.sh start [seconds]   # in the background: the maxima of anon, shmem (tmpfs), anon+shmem, and the
#                                       # java and mongod RSS, every <seconds> (default 5), into $CGROUP_SAMPLER_OUT;
#                                       # and every <seconds> one row of $CGROUP_SAMPLER_OUT.series (the machine:
#                                       # CPU ticks incl. steal, load, run queue, memory, swap, PSI totals, disk
#                                       # sectors and free space, network bytes, open files, OOM kills), rows of
#                                       # .procs (per command: processes, RSS, CPU ticks, threads, for every command
#                                       # over 20 MB) and each new JVM's command line in .jvms
#   cgroup-sampler.sh report            # memory.peak against memory.max, the maxima, memory.events, io.stat, GC logs
#
# OVERHEAD: one sample is two `cat`s over /proc/<pid>/stat and the cgroup files, four awks and a df -- about 10 ms of
# CPU every 5 s, 0.2% of one core of the runner's 4. Nothing it does can fail the job: the loop ignores every error.
#
# memory.peak alone is not the answer: it counts page cache, which the kernel reclaims before it OOM-kills. What
# cannot be reclaimed is anon (the JVM's heap, mongod's cache) plus shmem (a tmpfs data directory) -- that sum is
# what has to stay under memory.max.
#
# Disk writes: io.stat's `9:3` line is the RAID1 md device itself; its two members are counted again on their own
# lines, so summing every line triples a write.
#
# CGROUP_ROOT stands in for /sys/fs/cgroup and PROC_ROOT for /proc, for a test.
set -uo pipefail

cg="${CGROUP_ROOT:-/sys/fs/cgroup}"
proc="${PROC_ROOT:-/proc}"
out="${CGROUP_SAMPLER_OUT:-${RUNNER_TEMP:-/tmp}/cgroup-sampler.max}"

stat_of() { awk -v k="$1" '$1 == k { print $2 }' "$cg/memory.stat" 2>/dev/null; }

# The machine's figures, one tab-separated row (SERIES_HEADER), cumulative counters as the kernel keeps them.
SERIES_HEADER="t	cpu_total	cpu_busy	cpu_user	cpu_sys	cpu_iowait	cpu_steal	load1	procs_running	mem_total_kb	mem_avail_kb	swap_total_kb	swap_free_kb	psi_cpu_some_us	psi_mem_some_us	psi_mem_full_us	psi_io_some_us	psi_io_full_us	disk_read_sectors	disk_write_sectors	disk_free_kb	oom_kill	net_rx_bytes	net_tx_bytes	open_files"

series_line() {
  local files=() f free
  for f in stat loadavg meminfo pressure/cpu pressure/memory pressure/io diskstats vmstat net/dev sys/fs/file-nr; do
    [ -r "$proc/$f" ] && files+=("$proc/$f")
  done
  free=$(df -kP "${GITHUB_WORKSPACE:-/}" 2>/dev/null | awk 'NR == 2 { print $4 }')
  awk -v now="$(date +%s)" -v free="${free:-}" '
    function total(line,   i, n, kv) { n = split(line, kv, " "); for (i = 1; i <= n; i++) if (kv[i] ~ /^total=/) return substr(kv[i], 7); return "" }
    FILENAME ~ /\/stat$/ && $1 == "cpu" { user = $2 + $3; sys = $4 + $7 + $8; idle = $5; iow = $6; steal = $9
                                           tot = user + sys + idle + iow + steal; busy = user + sys + steal }
    FILENAME ~ /\/loadavg$/ { load = $1; split($4, rq, "/"); running = rq[1] }
    FILENAME ~ /\/meminfo$/ { m[$1] = $2 }
    FILENAME ~ /pressure\/cpu$/ && $1 == "some" { pcs = total($0) }
    FILENAME ~ /pressure\/memory$/ { if ($1 == "some") pms = total($0); else pmf = total($0) }
    FILENAME ~ /pressure\/io$/ { if ($1 == "some") pis = total($0); else pif = total($0) }
    FILENAME ~ /\/diskstats$/ && $3 ~ /^(sd[a-z]+|vd[a-z]+|xvd[a-z]+|nvme[0-9]+n[0-9]+)$/ { rd += $6; wr += $10 }
    FILENAME ~ /\/vmstat$/ && $1 == "oom_kill" { oom = $2 }
    FILENAME ~ /\/net\/dev$/ && $1 ~ /:$/ && $1 != "lo:" { rx += $2; tx += $10 }
    FILENAME ~ /\/file-nr$/ { fds = $1 }
    END { printf "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n",
            now, tot, busy, user, sys, iow, steal, load, running, m["MemTotal:"], m["MemAvailable:"], m["SwapTotal:"],
            m["SwapFree:"], pcs, pms, pmf, pis, pif, rd, wr, free, oom, rx, tx, fds }
  ' "${files[@]}" 2>/dev/null
}

# Per command, over every process's /proc/<pid>/stat (one `cat`, which skips a process that just exited): the
# processes, their RSS, their CPU ticks so far and their threads; `java` and `mongod` RSS for the maxima; and the pids
# of the JVMs, whose command lines the loop keeps.
proc_rows() {
  local page="$1"
  # shellcheck disable=SC2086
  cat "$proc"/[0-9]*/stat 2>/dev/null | awk -v now="$(date +%s)" -v page="$page" '
    { open_ = index($0, "("); close_ = 0; for (i = length($0); i > 0; i--) if (substr($0, i, 1) == ")") { close_ = i; break }
      if (!open_ || !close_) next
      comm = substr($0, open_ + 1, close_ - open_ - 1); gsub(/[ \t]/, "_", comm)
      n = split(substr($0, close_ + 2), f, " ")
      # after comm: state(1) ... utime(12) stime(13) ... num_threads(18) ... rss pages(22)
      count[comm]++; ticks[comm] += f[12] + f[13]; threads[comm] += f[18]; rss[comm] += f[22] * page / 1024
      if (comm == "java") print "jvm", substr($0, 1, open_ - 2) }
    END { for (c in count) if (rss[c] >= 20480 || c == "java" || c == "mongod")
            printf "row\t%s\t%s\t%d\t%d\t%d\t%d\n", now, c, count[c], rss[c], ticks[c], threads[c] }'
}

sample_loop() {
  local every="$1" anon=0 shmem=0 both=0 java=0 mongod=0 a s j m page rows pid seen=" "
  page=${SAMPLER_PAGE_SIZE:-$(getconf PAGESIZE 2>/dev/null || echo 4096)}
  [ -s "$out.series" ] || echo "$SERIES_HEADER" > "$out.series"
  [ -s "$out.procs" ] || printf 't\tcomm\tprocs\trss_kb\tticks\tthreads\n' > "$out.procs"
  while :; do
    series_line >> "$out.series"
    rows=$(proc_rows "$page")
    awk -F'\t' '$1 == "row" { print $2 "\t" $3 "\t" $4 "\t" $5 "\t" $6 "\t" $7 }' <<< "$rows" >> "$out.procs"
    for pid in $(awk '$1 == "jvm" { print $2 }' <<< "$rows"); do
      case "$seen" in *" $pid "*) continue ;; esac
      seen="$seen$pid "
      printf '%s\t%s\n' "$pid" "$(tr '\0' ' ' < "$proc/$pid/cmdline" 2>/dev/null | cut -c1-4000)" >> "$out.jvms"
    done
    a=$(stat_of anon); s=$(stat_of shmem)
    j=$(awk -F'\t' '$1 == "row" && $3 == "java" { print $5 * 1024 }' <<< "$rows")
    m=$(awk -F'\t' '$1 == "row" && $3 == "mongod" { print $5 * 1024 }' <<< "$rows")
    a=${a:-0}; s=${s:-0}; j=${j:-0}; m=${m:-0}
    [ "$a" -gt "$anon" ] && anon=$a
    [ "$s" -gt "$shmem" ] && shmem=$s
    [ $((a + s)) -gt "$both" ] && both=$((a + s))
    [ "$j" -gt "$java" ] && java=$j
    [ "$m" -gt "$mongod" ] && mongod=$m
    printf 'anon %s\nshmem %s\nanon+shmem %s\njava-rss %s\nmongod-rss %s\n' \
      "$anon" "$shmem" "$both" "$java" "$mongod" > "$out.tmp" && mv "$out.tmp" "$out"
    sleep "$every"
  done
}

gb() { awk -v b="$1" 'BEGIN { printf "%.2f GB", b / 1e9 }'; }

case "${1:-}" in
  start)
    sample_loop "${2:-5}" > /dev/null 2>&1 &
    disown 2> /dev/null || true
    ;;
  report)
    max=$(cat "$cg/memory.max" 2>/dev/null); peak=$(cat "$cg/memory.peak" 2>/dev/null)
    echo "[cgroup] memory.max $max ($( [ "$max" = max ] && echo unlimited || gb "$max" ))"
    echo "[cgroup] memory.peak ${peak:-?} ($(gb "${peak:-0}")) -- page cache included"
    if [ -f "$out" ]; then
      while read -r k v; do echo "[cgroup] max $k $v ($(gb "$v"))"; done < "$out"
    fi
    echo "[cgroup] memory.events: $(tr '\n' ' ' < "$cg/memory.events" 2>/dev/null)"
    echo "[cgroup] now: anon $(gb "$(stat_of anon)"), shmem $(gb "$(stat_of shmem)"), file $(gb "$(stat_of file)")"
    while read -r dev rest; do
      w=$(tr ' ' '\n' <<< "$rest" | awk -F= '$1 == "wbytes" { print $2 }')
      echo "[cgroup] io.stat $dev wbytes ${w:-0} ($(gb "${w:-0}"))"
    done < "$cg/io.stat"
    for f in /tmp/gc-*.log "${RUNNER_TEMP:-/tmp}"/ci-gc/gc-*.log; do
      [ -f "$f" ] || continue
      # "... Pause Young (Normal) (G1 Evacuation Pause) 8000M->3000M(10240M) 12.3ms": the after-GC heap is the live set's bound.
      awk '{ if (match($0, /[0-9]+M->[0-9]+M\([0-9]+M\)/)) { split(substr($0, RSTART, RLENGTH), p, /->|\(|\)/)
               a = p[2] + 0; c = p[3] + 0; if (a > ma) ma = a; if (c > mc) mc = c; n++; if ($0 ~ /Pause Full/) full++ } }
           END { printf "[gc] %s: %d collections, max after-GC heap %d MB, max committed %d MB, %d full\n", FILENAME, n, ma, mc, full }' "$f"
    done
    ;;
  *) echo "usage: $0 start [seconds] | report" >&2; exit 2 ;;
esac
