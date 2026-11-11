#!/usr/bin/env bash
# memory-guard.sh: watch a process tree on a CI runner, say every few minutes how it and the machine are doing, and end
# the tree -- naming why -- before the machine runs out of memory, so the step fails with its log instead of the
# runner being lost with it.
#
# Why: Identity takes #40 (run 43014208013, 2026-11-11): the US takes-drift-1 row's replay ran 47 minutes (13 on its
# base) and then "The hosted runner lost communication with the server" -- the runner starved, the job's whole log
# gone (BlobNotFound), no artifact uploaded, nothing to say what the replay was doing or how much memory it held. US
# rows already peak at 12.2 GB of the hosted runner's ~16 with a 10 GB heap. A step that ends the tree itself keeps
# its log, its artifacts and the job's later steps.
#
#   memory-guard.sh <pid> <reason file> [progress file]
#
# Runs until <pid> is gone (start it in the background beside the process it watches). Every MEMORY_GUARD_INTERVAL
# seconds (default 5) it reads the machine's memory; every MEMORY_GUARD_HEARTBEAT samples (default 60: 5 minutes) it
# prints one line -- elapsed minutes, memory in use, available, swap used, and the last line of [progress file] (the
# replay's log) -- so a slow step shows how far it got in its live log. When MemAvailable stays below
# MEMORY_GUARD_FLOOR_MB (default 1024) for MEMORY_GUARD_STRIKES samples in a row (default 3), it prints the machine's
# memory and the largest processes, writes one line naming the cause to <reason file>, and ends <pid> and every
# descendant (TERM, then KILL after 10 s). The caller reads <reason file> to tell that end from any other.
# Without a readable meminfo (macOS) it only waits. PROC_ROOT stands in for /proc, for a test.
set -uo pipefail

target=${1:?usage: memory-guard.sh <pid> <reason file> [progress file]}
reason=${2:?usage: memory-guard.sh <pid> <reason file> [progress file]}
progress=${3:-}
proc="${PROC_ROOT:-/proc}"
interval=${MEMORY_GUARD_INTERVAL:-5}
heartbeat=${MEMORY_GUARD_HEARTBEAT:-60}
floor_mb=${MEMORY_GUARD_FLOOR_MB:-1024}
strikes_needed=${MEMORY_GUARD_STRIKES:-3}
label=${MEMORY_GUARD_LABEL:-memory guard}

meminfo_mb() { awk -v k="$1:" '$1 == k { print int($2 / 1024) }' "$proc/meminfo" 2>/dev/null; }

# <pid> and every descendant, parents first.
tree_of() {
  local all
  all=$(ps -A -o pid= -o ppid= 2>/dev/null)
  echo "$1"
  awk -v root="$1" '{ parent[$1] = $2; order[NR] = $1 }
    END { keep[root] = 1
          do { grew = 0; for (i = 1; i <= NR; i++) { p = order[i]; if (!(p in keep) && (parent[p] in keep)) { keep[p] = 1; grew = 1; print p } } } while (grew) }' <<< "$all"
}

end_tree() {
  local pids p
  pids=$(tree_of "$target")
  for p in $pids; do kill -TERM "$p" 2>/dev/null; done
  for _ in $(seq 1 20); do
    kill -0 "$target" 2>/dev/null || break
    sleep 0.5
  done
  for p in $pids; do kill -KILL "$p" 2>/dev/null; done
}

said() { # the last line of the progress file, shortened
  [ -n "$progress" ] && [ -s "$progress" ] || return 0
  printf ' -- last: %s' "$(tail -n 1 "$progress" | tr -d '\r' | cut -c1-160)"
}

samples=0 strikes=0 start=$SECONDS
while kill -0 "$target" 2>/dev/null; do
  sleep "$interval"
  kill -0 "$target" 2>/dev/null || break
  samples=$((samples + 1))
  total=$(meminfo_mb MemTotal) avail=$(meminfo_mb MemAvailable)
  [ -n "$total" ] && [ -n "$avail" ] || continue
  swap_used=$(( $(meminfo_mb SwapTotal || echo 0) - $(meminfo_mb SwapFree || echo 0) ))
  minutes=$(( (SECONDS - start) / 60 ))
  state="$((total - avail)) MB in use of $total, $avail MB available, $swap_used MB swap used"
  if [ $((samples % heartbeat)) = 0 ]; then
    echo "$label: ${minutes} min, $state$(said)"
  fi
  if [ "$avail" -lt "$floor_mb" ]; then strikes=$((strikes + 1)); else strikes=0; fi
  if [ "$strikes" -ge "$strikes_needed" ]; then
    why="the machine had under $floor_mb MB available for $strikes samples ($state) after $minutes min$(said)"
    echo "$why" > "$reason"
    echo "$label: ENDING process $target and its children: $why"
    echo "$label: the largest processes (RSS MB, command):"
    ps -A -o rss= -o comm= 2>/dev/null | sort -rn | head -8 | awk '{ printf "    %6d %s\n", $1 / 1024, $2 }'
    end_tree
    exit 0
  fi
done
exit 0
