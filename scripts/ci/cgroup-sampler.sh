#!/usr/bin/env bash
# The job's own memory and disk writes, read from its cgroup (the job container's, which holds the job's dockerd
# and the mongod container inside it).
#
#   cgroup-sampler.sh start [seconds]   # in the background: the maxima of anon, shmem (tmpfs), anon+shmem, and the
#                                       # java and mongod RSS, every <seconds> (default 5), into $CGROUP_SAMPLER_OUT
#   cgroup-sampler.sh report            # memory.peak against memory.max, the maxima, memory.events, io.stat, GC logs
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

rss_of() {
  local name="$1" sum=0 f kb
  for f in "$proc"/[0-9]*/status; do
    grep -qx "Name:[[:space:]]*$name" "$f" 2>/dev/null || continue
    kb=$(awk '$1 == "VmRSS:" { print $2 }' "$f" 2>/dev/null)
    sum=$((sum + ${kb:-0}))
  done
  echo $((sum * 1024))
}

sample_loop() {
  local every="$1" anon=0 shmem=0 both=0 java=0 mongod=0 a s j m
  while :; do
    a=$(stat_of anon); s=$(stat_of shmem); j=$(rss_of java); m=$(rss_of mongod)
    a=${a:-0}; s=${s:-0}
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
    for f in /tmp/gc-*.log; do
      [ -f "$f" ] || continue
      # "... Pause Young (Normal) (G1 Evacuation Pause) 8000M->3000M(10240M) 12.3ms": the after-GC heap is the live set's bound.
      awk '{ if (match($0, /[0-9]+M->[0-9]+M\([0-9]+M\)/)) { split(substr($0, RSTART, RLENGTH), p, /->|\(|\)/)
               a = p[2] + 0; c = p[3] + 0; if (a > ma) ma = a; if (c > mc) mc = c; n++; if ($0 ~ /Pause Full/) full++ } }
           END { printf "[gc] %s: %d collections, max after-GC heap %d MB, max committed %d MB, %d full\n", FILENAME, n, ma, mc, full }' "$f"
    done
    ;;
  *) echo "usage: $0 start [seconds] | report" >&2; exit 2 ;;
esac
