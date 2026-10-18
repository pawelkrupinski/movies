#!/usr/bin/env bash
# THE MEMORY GATE: a tool about to start a JVM (an sbt server, a slice, a measure's resolve) waits while the Mac is
# already paging, instead of adding a heap to it. On 2026-10-18 eighteen agents' JVMs held ~70 GB of heaps on a 36 GB
# Mac: 51.7 of 52 GB of swap used, ~18 GB compressed, a load average of 128 that was threads waiting on pages, and every
# slice and measure 3-8x slower than alone. Sourcing it defines functions only. bash 3.2 compatible.
#
#   memory_gate <label>   waits while memory_paged_gb > KINOWO_MEMORY_GATE_GB (default 20), saying so once, polling
#                         every KINOWO_MEMORY_GATE_POLL seconds (default 15); after KINOWO_MEMORY_GATE_MAX_WAIT seconds
#                         (default 600) it goes ahead, warned: a gate that never opens is worse than a slow start.
#                         KINOWO_MEMORY_GATE=0 turns it off. A no-op where the paged memory cannot be read (Linux CI).
#   memory_paged_gb       the GB already pushed out of RAM: swap used plus the compressor's pages (macOS); the
#                         KINOWO_MEMORY_GATE_PROBE command's output instead when set (the self-test's seam)

memory_paged_gb() {
  if [ -n "${KINOWO_MEMORY_GATE_PROBE:-}" ]; then eval "$KINOWO_MEMORY_GATE_PROBE"; return; fi
  [ "$(uname -s)" = Darwin ] || return 1
  local swap_mb page compressed
  swap_mb=$(sysctl -n vm.swapusage 2>/dev/null | sed -n 's/.*used = \([0-9.]*\)M.*/\1/p')
  page=$(sysctl -n hw.pagesize 2>/dev/null)
  compressed=$(vm_stat 2>/dev/null | awk '/occupied by compressor/ {gsub(/\./, "", $NF); print $NF}')
  [ -n "$swap_mb" ] && [ -n "$page" ] && [ -n "$compressed" ] || return 1
  awk -v s="$swap_mb" -v p="$page" -v c="$compressed" 'BEGIN { printf "%d\n", s / 1024 + p * c / 2^30 }'
}

memory_gate() { # <label>
  [ "${KINOWO_MEMORY_GATE:-1}" = 0 ] && return 0
  local label=${1:-tool} budget=${KINOWO_MEMORY_GATE_GB:-20} max=${KINOWO_MEMORY_GATE_MAX_WAIT:-600}
  local poll=${KINOWO_MEMORY_GATE_POLL:-15} paged waited=0 said=""
  while paged=$(memory_paged_gb) && [ -n "$paged" ] && [ "$paged" -gt "$budget" ]; do
    if [ "$waited" -ge "$max" ]; then
      echo "[$label] memory gate: still ${paged} GB paged out after ${waited}s: starting anyway" >&2
      return 0
    fi
    [ -n "$said" ] || { echo "[$label] memory gate: ${paged} GB paged out (over KINOWO_MEMORY_GATE_GB=$budget): waiting for other JVMs to finish before starting one (up to ${max}s)" >&2; said=1; }
    sleep "$poll"
    waited=$(awk -v w="$waited" -v p="$poll" 'BEGIN { printf "%d\n", w + p + 0.999 }')
  done
  [ -z "$said" ] || echo "[$label] memory gate: open after ${waited}s" >&2
  return 0
}
