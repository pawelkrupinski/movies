#!/usr/bin/env bash
# THE JVM GATE: how the tools that start heavy JVMs (sbt-warm's commands and servers, identity-slice's runs and its
# classpath compile, identity-measure-local's resolves) share one Mac. On 2026-10-18 eighteen agents' JVMs held ~70 GB
# of heaps on a 36 GB Mac (51.7 of 52 GB of swap used, ~18 GB compressed), and once memory drained, seven JVMs each
# sized their thread pools and GC for all 14 cores (70-115 threads apiece): a load average of 124 that was threads
# queueing, every slice and measure 3-8x slower than alone. So a tool waits for memory, waits for one of a few machine
# slots, and starts a JVM that sizes itself for a share of the cores. Sourcing it defines functions only. bash 3.2.
#
#   memory_gate <label>    waits while memory_paged_gb > KINOWO_MEMORY_GATE_GB (default 20) AND memory_free_gb <
#                          KINOWO_MEMORY_GATE_FREE_GB (default 6, about one tool heap): swap holds idle apps' stale pages
#                          long after RAM frees up (20 GB "paged" beside 9.6 GB free on 2026-10-18), so paging alone kept
#                          tools waiting on nothing. Says so once, polls every KINOWO_MEMORY_GATE_POLL seconds (15); after
#                          KINOWO_MEMORY_GATE_MAX_WAIT seconds (600) goes ahead, warned. KINOWO_MEMORY_GATE=0 turns it
#                          off; a no-op where paging cannot be read.
#   memory_paged_gb        the GB already pushed out of RAM: swap used plus the compressor's pages (macOS);
#                          KINOWO_MEMORY_GATE_PROBE's output instead when set (the self-test's seam)
#   memory_free_gb         the GB of RAM a new heap can have now: free, inactive and speculative pages (macOS);
#                          KINOWO_MEMORY_GATE_FREE_PROBE's output instead when set; none readable counts as short
#   jvm_slot_take <label>  the memory gate, then one of the machine's KINOWO_JVM_SLOTS (default 4) slots, held until
#                          jvm_slot_release or this process ends; waits for one (saying so once, polling every
#                          KINOWO_JVM_SLOT_POLL seconds, 5) and after KINOWO_JVM_SLOT_MAX_WAIT seconds (1800) goes ahead,
#                          warned. A child of a holder rides its slot (KINOWO_JVM_SLOT_HELD, exported): a tool run by a
#                          tool never waits on its own parent. KINOWO_JVM_SLOTS=0 turns the slots off.
#   jvm_slot_release       gives this process's slot back
#   jvm_cpu_options        the JVM options sizing a tool's JVM for KINOWO_JVM_CPUS cores (default 4) on macOS, where
#                          agents share the cores; none on Linux (CI's runner is the JVM's alone) or with KINOWO_JVM_CPUS=0
#   jvm_lock <dir>         mkdir-held by this process; one held by a process that is gone is taken over. 0 when taken
#   jvm_unlock <dir>       removes it when this process holds it

jvm_lock() {
  if mkdir "$1" 2>/dev/null; then echo $$ > "$1/pid"; return 0; fi
  local holder; holder=$(cat "$1/pid" 2>/dev/null)
  if [ -n "$holder" ] && ! kill -0 "$holder" 2>/dev/null; then rm -rf "$1"; mkdir "$1" 2>/dev/null && { echo $$ > "$1/pid"; return 0; }; fi
  return 1
}
jvm_unlock() { [ -n "${1:-}" ] && [ "$(cat "$1/pid" 2>/dev/null)" = $$ ] && rm -rf "$1"; return 0; }

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

memory_free_gb() {
  if [ -n "${KINOWO_MEMORY_GATE_FREE_PROBE:-}" ]; then eval "$KINOWO_MEMORY_GATE_FREE_PROBE"; return; fi
  [ "$(uname -s)" = Darwin ] || return 1
  vm_stat 2>/dev/null | awk -v p="$(sysctl -n hw.pagesize 2>/dev/null)" '
    /Pages (free|inactive|speculative):/ { gsub(/\./, "", $NF); n += $NF } END { if (p) printf "%d\n", n * p / 2^30 }'
}

# memory_short -> 0 while memory is both paged out past the budget and short of free RAM; prints the paged-out GB
memory_short() {
  local paged free
  paged=$(memory_paged_gb) && [ -n "$paged" ] && [ "$paged" -gt "${KINOWO_MEMORY_GATE_GB:-20}" ] || return 1
  free=$(memory_free_gb) || free=0
  [ "${free:-0}" -lt "${KINOWO_MEMORY_GATE_FREE_GB:-6}" ] || return 1
  echo "$paged GB paged out, ${free:-0} GB free"
}

# _jvm_waited <seconds waited> <poll> -> the seconds waited after one more poll (a fractional poll counted whole)
_jvm_waited() { awk -v w="$1" -v p="$2" 'BEGIN { printf "%d\n", w + p + 0.999 }'; }

memory_gate() { # <label>
  [ "${KINOWO_MEMORY_GATE:-1}" = 0 ] && return 0
  local label=${1:-tool} budget=${KINOWO_MEMORY_GATE_GB:-20} max=${KINOWO_MEMORY_GATE_MAX_WAIT:-600}
  local poll=${KINOWO_MEMORY_GATE_POLL:-15} state waited=0 said=""
  while state=$(memory_short); do
    if [ "$waited" -ge "$max" ]; then
      echo "[$label] memory gate: still ${state} after ${waited}s: starting anyway" >&2
      return 0
    fi
    [ -n "$said" ] || { echo "[$label] memory gate: ${state} (over KINOWO_MEMORY_GATE_GB=$budget, under KINOWO_MEMORY_GATE_FREE_GB=${KINOWO_MEMORY_GATE_FREE_GB:-6}): waiting for other JVMs to finish before starting one (up to ${max}s)" >&2; said=1; }
    sleep "$poll"
    waited=$(_jvm_waited "$waited" "$poll")
  done
  [ -z "$said" ] || echo "[$label] memory gate: open after ${waited}s" >&2
  return 0
}

JVM_SLOT=""
jvm_slot_take() { # <label>
  [ -n "${KINOWO_JVM_SLOT_HELD:-}" ] && return 0
  local label=${1:-tool} slots=${KINOWO_JVM_SLOTS:-4} max=${KINOWO_JVM_SLOT_MAX_WAIT:-1800}
  local poll=${KINOWO_JVM_SLOT_POLL:-5} dir n waited=0 said=""
  memory_gate "$label"
  [ "$slots" = 0 ] && return 0
  dir="${KINOWO_IDENTITY_CACHE:-$HOME/.cache/kinowo-identity}/jvm-slots"
  mkdir -p "$dir"
  while :; do
    for n in $(seq 1 "$slots"); do
      if jvm_lock "$dir/$n"; then
        JVM_SLOT="$dir/$n"; export KINOWO_JVM_SLOT_HELD="$JVM_SLOT"
        [ -z "$said" ] || echo "[$label] JVM slot $n of $slots after ${waited}s" >&2
        return 0
      fi
    done
    if [ "$waited" -ge "$max" ]; then
      echo "[$label] every JVM slot still busy after ${waited}s: starting anyway" >&2
      return 0
    fi
    [ -n "$said" ] || { echo "[$label] all $slots JVM slots busy (KINOWO_JVM_SLOTS; holders: $(cat "$dir"/*/pid 2>/dev/null | paste -sd, -)): waiting (up to ${max}s)" >&2; said=1; }
    sleep "$poll"
    waited=$(_jvm_waited "$waited" "$poll")
  done
}
jvm_slot_release() {
  [ -n "$JVM_SLOT" ] || return 0
  jvm_unlock "$JVM_SLOT"; JVM_SLOT=""; unset KINOWO_JVM_SLOT_HELD
  return 0
}

jvm_cpu_options() {
  local cpus=${KINOWO_JVM_CPUS:-4}
  [ "$cpus" = 0 ] && return 0
  [ "${KINOWO_JVM_GATE_OS:-$(uname -s)}" = Darwin ] || return 0
  echo "-XX:ActiveProcessorCount=$cpus"
}

# FOLLOW MAIN: a worktree keeps the gate it branched with, so a gate fix reached no agent already running (two of
# 2026-10-18's agents sat ~45 min at a gate main had already fixed). Sourced from a linked worktree, this re-reads the
# root checkout's copy -- local main, kept clean -- over its own definitions: what the root lacks, this copy still
# defines. KINOWO_JVM_GATE_FOLLOW_MAIN=0 keeps the worktree's own.
if [ "${KINOWO_JVM_GATE_FOLLOW_MAIN:-1}" != 0 ] && [ -z "${_JVM_GATE_FOLLOWING:-}" ]; then
  _jvm_gate_here=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)
  _jvm_gate_root=$(git -C "$_jvm_gate_here" rev-parse --path-format=absolute --git-common-dir 2>/dev/null) &&
    _jvm_gate_root=$(cd "$(dirname "$_jvm_gate_root")" && pwd -P) &&
    [ "$_jvm_gate_root/scripts" != "$_jvm_gate_here" ] && [ -r "$_jvm_gate_root/scripts/jvm-gate.sh" ] &&
    { _JVM_GATE_FOLLOWING=1
      # shellcheck source=/dev/null
      . "$_jvm_gate_root/scripts/jvm-gate.sh"
      unset _JVM_GATE_FOLLOWING; }
  unset _jvm_gate_here _jvm_gate_root
fi
