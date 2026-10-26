#!/usr/bin/env bash
# What a stalled process group was doing, said before it is ended: `bounded` runs this at its deadline (with
# BOUNDED_ON_TIMEOUT pointing here) while the group is still alive, so a test that hangs on CI explains itself.
#
#   stall-report.sh <pgid>
#
# To stderr (or STALL_REPORT_FILE): the load, the group's processes (state, wait channel, elapsed and CPU time), each one's kernel state and
# stack where /proc lets it be read, and the job's cgroup: memory against its cap, and CPU, memory and IO pressure.
# Main #1873's tool-conventions-test.sh sat ~10 min in a case that takes 0.04 s, and said nothing about why.
# Each part is best effort (macOS has no /proc, a container may hide the stack): a missing one is skipped, never fatal.
#
# CGROUP_ROOT stands in for /sys/fs/cgroup and PROC_ROOT for /proc, for a test.
set -uo pipefail

pgid=${1:?usage: stall-report.sh <pgid>}
# STALL_REPORT_FILE: append there instead, for a caller whose stderr is captured and compared
[ -z "${STALL_REPORT_FILE:-}" ] || exec 2>> "$STALL_REPORT_FILE"
cg="${CGROUP_ROOT:-/sys/fs/cgroup}"
proc="${PROC_ROOT:-/proc}"

{
  echo "── stall report: process group $pgid at $(date -u +%H:%M:%SZ) ──"
  [ -r "$proc/loadavg" ] && echo "load: $(cat "$proc/loadavg")"
  echo "processes:"
  # by group id, not `ps -g` (a session on some ps, a group on others)
  ps -A -o pid=,pgid=,stat=,wchan=,etime=,time=,rss=,args= 2>/dev/null |
    awk -v g="$pgid" '$2 == g { print "  " $0 }' | cut -c1-240
  for pid in $(ps -A -o pid=,pgid= 2>/dev/null | awk -v g="$pgid" '$2 == g { print $1 }'); do
    [ -r "$proc/$pid/status" ] || continue
    echo "  $pid: $(grep -E '^(State|Threads|VmRSS):' "$proc/$pid/status" | tr -s '\t ' ' ' | paste -sd';' -)"
    [ -r "$proc/$pid/stack" ] && sed -n '1,8s/^/      /p' "$proc/$pid/stack" 2>/dev/null
  done
  echo "cgroup:"
  for f in memory.current memory.max memory.events cpu.pressure memory.pressure io.pressure; do
    [ -r "$cg/$f" ] && echo "  $f: $(paste -sd';' - < "$cg/$f")"
  done
  [ -r "$cg/memory.stat" ] && echo "  memory.stat: $(awk '$1 ~ /^(anon|file|shmem|pgmajfault)$/ { printf "%s=%s ", $1, $2 }' "$cg/memory.stat")"
  echo "── end of stall report ──"
} >&2 2>/dev/null
exit 0
