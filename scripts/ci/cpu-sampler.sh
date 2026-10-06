#!/usr/bin/env bash
# Print the WHOLE runner's CPU, split between the JVM and mongod, every <seconds> until killed.
#
#   cpu-sampler.sh <seconds>      # "[cpu] busy 3.6 of 4 cores (user 3.1, system 0.4, iowait 0.1); java 2.4, mongod 1.1"
#   cpu-sampler.sh read           # one raw reading (for the test)
#   cpu-sampler.sh report <seconds> <earlier reading> <later reading>
#
# WHY. A phase line (`PhaseTimer`) carries the JVM's own CPU, and on the convergence legs that is
# 2.0-2.4 cores of the runner's 4 through every long phase (run 37499106536). Whether the rest is
# mongod — a separate process the JVM's number never sees — I/O wait, or a pipeline that simply
# waits is the question that decides how to make a leg faster, and only the machine can answer it.
#
# PROC_ROOT stands in for /proc so the test can feed a fixed one.
set -uo pipefail

proc="${PROC_ROOT:-/proc}"
ticks="$(getconf CLK_TCK 2>/dev/null || echo 100)"

# One reading: "<cores> <busy> <user> <system> <iowait> <total> <java> <mongod>", in clock ticks.
read_once() {
  awk -v proc="$proc" '
    FNR == 1 && FILENAME ~ /\/stat$/ && $1 == "cpu" {
      user = $2 + $3; sys = $4 + $7 + $8; idle = $5; iowait = $6; steal = $9
      total = user + sys + idle + iowait + steal
      next
    }
    FILENAME ~ /\/stat$/ && $1 ~ /^cpu[0-9]+$/ { cores++ }
    END { printf "%d %d %d %d %d %d", cores, user + sys + steal, user, sys, iowait, total }
  ' "$proc/stat"
  for name in java mongod; do
    local sum=0 statfile fields
    for statfile in "$proc"/[0-9]*/stat; do
      [ -r "$statfile" ] || continue
      fields="$(cat "$statfile" 2>/dev/null)" || continue
      # comm is the second field, in parentheses; utime and stime are 14 and 15, counted after it.
      case "$fields" in *"($name)"*) ;; *) continue ;; esac
      # shellcheck disable=SC2086 # split on purpose: the fields after comm, one word each
      set -- ${fields##*) }
      sum=$((sum + ${12} + ${13}))
    done
    printf ' %d' "$sum"
  done
  echo
}

# report <seconds> "<earlier>" "<later>"
report() {
  local seconds="$1"
  awk -v s="$seconds" -v t="$ticks" -v a="$2" -v b="$3" 'BEGIN {
    split(a, x, " "); split(b, y, " ")
    total = y[6] - x[6]; if (total <= 0) total = 1
    c = y[1]
    f = c / total   # ticks -> cores
    printf "[cpu] busy %.1f of %d cores (user %.1f, system %.1f, iowait %.1f); java %.1f, mongod %.1f\n",
      (y[2] - x[2]) * f, c, (y[3] - x[3]) * f, (y[4] - x[4]) * f, (y[5] - x[5]) * f,
      (y[7] - x[7]) / t / s, (y[8] - x[8]) / t / s
  }'
}

case "${1:-}" in
  read)   read_once ;;
  report) report "$2" "$3" "$4" ;;
  ''|*[!0-9]*) echo "usage: $0 <seconds> | read | report <seconds> <earlier> <later>" >&2; exit 64 ;;
  *)
    interval="$1"
    previous="$(read_once)"
    while sleep "$interval"; do
      current="$(read_once)"
      report "$interval" "$previous" "$current"
      previous="$current"
    done
    ;;
esac
