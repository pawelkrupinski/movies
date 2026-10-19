#!/usr/bin/env bash
# THE JVM GATE: how the tools that start heavy JVMs (sbt-warm's commands and servers, identity-slice's runs and its
# classpath compile, identity-measure-local's resolves) share one Mac. On 2026-10-18 eighteen agents' JVMs held ~70 GB
# of heaps on a 36 GB Mac (51.7 of 52 GB of swap used, ~18 GB compressed), and once memory drained, seven JVMs each
# sized their thread pools and GC for all 14 cores (70-115 threads apiece): a load average of 124 that was threads
# queueing, every slice and measure 3-8x slower than alone. So a tool waits for memory, waits for one of a few machine
# slots, and starts a JVM that sizes itself for a share of the cores. Sourcing it defines functions only. bash 3.2.
#
#   A long-lived JVM -- sbt-warm's kept server, identity-slice's --serve JVM -- holds its slot for its whole life, idle or
#   not, through a keeper process beside it (jvm_slot_keeper_start); a call reaching a live one takes none.
#
#   memory_gate <label>    waits while memory_paged_gb > KINOWO_MEMORY_GATE_GB (default 20) AND memory_free_gb <
#                          KINOWO_MEMORY_GATE_FREE_GB (default 6, about one tool heap): swap holds idle apps' stale pages
#                          long after RAM frees up (20 GB "paged" beside 9.6 GB free on 2026-10-18), so paging alone kept
#                          tools waiting on nothing. Polls every KINOWO_MEMORY_GATE_POLL seconds (15). While memory is
#                          short it NEVER goes ahead on time alone: a 600 s deadline let every queued tool start under
#                          sustained pressure (2026-10-19: load ~100, 29 of 30 GB swap, 0.1 GB free). It says so once, then
#                          every KINOWO_MEMORY_GATE_STATUS_SECONDS (180) prints how long, the paged and free GB, and the
#                          biggest processes. The escape hatch: KINOWO_MEMORY_GATE_MAX_WAIT=<seconds> goes ahead, warned,
#                          after that long (unset: no limit); KINOWO_MEMORY_GATE=0 turns the gate off. A no-op where
#                          paging cannot be read, and for a process riding a held slot (below): its parent's JVM is the
#                          memory it would wait on, so it would wait on itself forever.
#   memory_paged_gb        the GB already pushed out of RAM: swap used plus the compressor's pages (macOS);
#                          KINOWO_MEMORY_GATE_PROBE's output instead when set (the self-test's seam)
#   memory_free_gb         the GB of RAM a new heap can have now: free, inactive and speculative pages (macOS);
#                          KINOWO_MEMORY_GATE_FREE_PROBE's output instead when set; none readable counts as short
#   jvm_slot_take <label>  the memory gate, then one of the machine's KINOWO_JVM_SLOTS (default 4) slots (a lock, below,
#                          on descriptor 7), held until jvm_slot_release, or until this process and the children it
#                          started meanwhile (the JVM) end; waits for one (saying so once, then every
#                          KINOWO_MEMORY_GATE_STATUS_SECONDS, polling every KINOWO_JVM_SLOT_POLL seconds, 5). After
#                          KINOWO_JVM_SLOT_MAX_WAIT seconds (1800) it goes ahead without one, warned -- but only while memory
#                          is NOT short: a slot's holder may be hung, holding no memory worth waiting for, yet while memory
#                          is short another JVM is exactly what the slots are there to stop. A child of a holder rides its
#                          slot (KINOWO_JVM_SLOT_HELD, exported, and descriptor 7 inherited open): a tool run by a tool
#                          never waits on its own parent, for a slot or for memory. A child started with the descriptors
#                          closed (below) holds no slot, whatever its environment names, so it takes its own.
#                          KINOWO_JVM_SLOTS=0 turns the slots off.
#   jvm_slot_try           one pass over the slots, never waiting: 0 holding one (as jvm_slot_take), 1 when all are busy
#   jvm_slot_riding        0 while this process holds a slot, its own or its parent's (descriptor 7 open)
#   jvm_slot_hand_off <pid>  the slot now belongs to <pid>, a process this one started with descriptor 7 open that
#                          outlives it (sbt-warm's keeper of a kept server): <pid> written in, this process's descriptor
#                          closed. The slot is held until <pid> and every process holding the descriptor have ended
#   jvm_slot_keeper_start <dir> <log> <command...>  -> KEEPER_PID: the KEEPER of a long-lived JVM this process started
#                          (sbt-warm's kept server, identity-slice's --serve JVM), so that JVM is counted for its whole
#                          life, idle or not: <command> started in <dir>, detached (its own process group, nohup, stdin
#                          closed, output appended to <log>), with descriptor 7 open and 6, 8, 9 closed, and this
#                          process's slot handed off to it (jvm_slot_hand_off). The keeper's loop polls the JVM, calls
#                          jvm_slot_take_if_free, and exits once the JVM is gone -- which frees the slot. Without a slot
#                          (KINOWO_JVM_SLOTS=0, past the slot deadline, or riding a parent's) it starts all the same.
#   jvm_slot_take_if_free  a keeper's poll: 0 when this process, holding no slot, has just taken a free one (slots on)
#   jvm_slot_release       gives this process's slot back
#   jvm_cpu_options        the JVM options sizing a tool's JVM for KINOWO_JVM_CPUS cores (default 4) on macOS, where
#                          agents share the cores; none on Linux (CI's runner is the JVM's alone) or with KINOWO_JVM_CPUS=0
#   jvm_lock <file>        this process holds <file>'s lock (below) on descriptor 6, its pid written in. 0 when taken,
#                          1 at once when another process holds it
#   jvm_unlock <file>      gives it back when this process holds it
#
# THE LOCKS are the kernel's flock(2) locks on a file, which the OS frees the moment the holder has ended, however it
# ended -- exit, crash, kill -9, a restart -- so a lock is never held by a holder that is gone, and nothing detects a
# stale holder or takes its lock over. (The directory-plus-pid locks they replace each needed such a take-over, and each
# take-over left a race: two holders inside one lock and two winners of one slot under load, 2026-10-19.) macOS has no
# flock(1): python3's fcntl.flock takes the lock on a descriptor bash opened and keeps open, so the lock is this
# process's until that descriptor closes. Each kind has its own fixed descriptor (bash 3.2 has no {fd}), so one process
# holds one of each at once: 6 jvm_lock, 7 a JVM slot, 8 identity_locked (identity-tools.sh). A child inherits the
# descriptor and so holds the lock with its parent until it ends: the JVM a slot was taken for, or the download a lock
# guards, keeps it while it still runs, even past its tool being killed -- as it should. A child started to OUTLIVE the
# call (an sbt server, a kept JVM, a watchdog, a mongod) is started with them closed: `6>&- 7>&- 8>&-` on its command.
# The pid in the file is for the messages that name a holder only; the lock itself is the truth.
#   lock_take <fd> <file>  0 when this process now holds <file>'s lock on descriptor <fd> (opened here), its pid
#                          written in; 1 at once, the descriptor closed again, when another process holds it
#   lock_give <fd> <file>  gives it back: the pid cleared, the descriptor closed
#   lock_busy <file>       0 while some process holds <file>'s lock

lock_take() {
  local fd=$1 file=$2
  eval "exec $fd>>\"\$file\"" 2> /dev/null || return 1
  if _lock_flock "$fd"; then echo $$ > "$file"; return 0; fi
  eval "exec $fd>&-"
  return 1
}
lock_give() { : > "$2"; eval "exec $1>&-"; }
# _lock_flock <fd> -> 0 when the open file description on <fd> (shared with python3, which inherits it) now holds the
# exclusive lock; 1 when another holds it. With <fd> "-" and a path, a probe on a description of its own, never kept
_lock_flock() {
  python3 -I -c 'import fcntl, sys
f = int(sys.argv[1]) if sys.argv[1] != "-" else open(sys.argv[2], "a")
try:
    fcntl.flock(f, fcntl.LOCK_EX | fcntl.LOCK_NB)
except OSError:
    sys.exit(1)' "$@"
}
lock_busy() { [ -e "$1" ] && ! _lock_flock - "$1"; }

JVM_LOCK=""
jvm_lock() { lock_take 6 "$1" && JVM_LOCK=$1; }
jvm_unlock() { [ -n "${1:-}" ] && [ "$1" = "$JVM_LOCK" ] && { lock_give 6 "$1"; JVM_LOCK=""; }; return 0; }

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

# memory_top_holders -> the three biggest processes by resident memory, "name GB (pid N)", for the gate's status lines
memory_top_holders() {
  ps -axo rss=,pid=,comm= 2>/dev/null | sort -rn | head -3 |
    awk '{ n = $3; sub(/.*\//, "", n); printf "%s%s %.1f GB (pid %s)", (NR > 1 ? ", " : ""), n, $1 / 2^20, $2 }'
}

memory_gate() { # <label>
  [ "${KINOWO_MEMORY_GATE:-1}" = 0 ] && return 0
  jvm_slot_riding && return 0
  local label=${1:-tool} budget=${KINOWO_MEMORY_GATE_GB:-20} max=${KINOWO_MEMORY_GATE_MAX_WAIT:-}
  local poll=${KINOWO_MEMORY_GATE_POLL:-15} every=${KINOWO_MEMORY_GATE_STATUS_SECONDS:-180} state waited=0 said="" next
  next=$every
  while state=$(memory_short); do
    if [ -n "$max" ] && [ "$waited" -ge "$max" ]; then
      echo "[$label] memory gate: still ${state} after ${waited}s: starting anyway (KINOWO_MEMORY_GATE_MAX_WAIT=$max)" >&2
      return 0
    fi
    if [ -z "$said" ]; then
      echo "[$label] memory gate: ${state} (over KINOWO_MEMORY_GATE_GB=$budget, under KINOWO_MEMORY_GATE_FREE_GB=${KINOWO_MEMORY_GATE_FREE_GB:-6}): waiting for other JVMs to finish before starting one ($([ -n "$max" ] && echo "up to ${max}s" || echo "for as long as memory is short; KINOWO_MEMORY_GATE_MAX_WAIT=<seconds> goes ahead after that long"))" >&2
      said=1
    elif [ "$waited" -ge "$next" ]; then
      echo "[$label] memory gate: still waiting after ${waited}s: ${state}; biggest: $(memory_top_holders)" >&2
      next=$((waited + every))
    fi
    sleep "$poll"
    waited=$(_jvm_waited "$waited" "$poll")
  done
  [ -z "$said" ] || echo "[$label] memory gate: open after ${waited}s" >&2
  return 0
}

JVM_SLOT=""
jvm_slot_riding() { [ -n "${KINOWO_JVM_SLOT_HELD:-}" ] && { : >&7; } 2> /dev/null; }
jvm_slot_try() {
  local slots=${KINOWO_JVM_SLOTS:-4} dir n
  dir="${KINOWO_IDENTITY_CACHE:-$HOME/.cache/kinowo-identity}/jvm-slots"
  mkdir -p "$dir"
  for n in $(seq 1 "$slots"); do
    if lock_take 7 "$dir/$n.flock"; then
      JVM_SLOT="$dir/$n.flock"; export KINOWO_JVM_SLOT_HELD="$JVM_SLOT"
      return 0
    fi
  done
  return 1
}
jvm_slot_take() { # <label>
  # riding needs the slot's descriptor itself: a process outliving its starter has the name but not the lock
  jvm_slot_riding && return 0
  local label=${1:-tool} slots=${KINOWO_JVM_SLOTS:-4} max=${KINOWO_JVM_SLOT_MAX_WAIT:-1800}
  local poll=${KINOWO_JVM_SLOT_POLL:-5} every=${KINOWO_MEMORY_GATE_STATUS_SECONDS:-180} dir n waited=0 said="" next state
  memory_gate "$label"
  [ "$slots" = 0 ] && return 0
  dir="${KINOWO_IDENTITY_CACHE:-$HOME/.cache/kinowo-identity}/jvm-slots"
  next=$every
  while :; do
    if jvm_slot_try; then
      [ -z "$said" ] || echo "[$label] JVM slot ${JVM_SLOT##*/} of $slots after ${waited}s" >&2
      return 0
    fi
    if [ "$waited" -ge "$max" ]; then
      if [ "${KINOWO_MEMORY_GATE:-1}" != 0 ] && state=$(memory_short); then
        if [ -z "$said" ] || [ "$waited" -ge "$next" ]; then
          echo "[$label] every JVM slot still busy after ${waited}s, past KINOWO_JVM_SLOT_MAX_WAIT=$max, but ${state}: still waiting; biggest: $(memory_top_holders)" >&2
          said=1; next=$((waited + every))
        fi
      else
        echo "[$label] every JVM slot still busy after ${waited}s: starting anyway" >&2
        return 0
      fi
    elif [ -z "$said" ]; then
      echo "[$label] all $slots JVM slots busy (KINOWO_JVM_SLOTS; holders: $(for n in $(seq 1 "$slots"); do cat "$dir/$n.flock" 2>/dev/null; done | paste -sd, -)): waiting (up to ${max}s, longer while memory is short)" >&2; said=1
    elif [ "$waited" -ge "$next" ]; then
      echo "[$label] all $slots JVM slots still busy after ${waited}s (holders: $(for n in $(seq 1 "$slots"); do cat "$dir/$n.flock" 2>/dev/null; done | paste -sd, -))" >&2
      next=$((waited + every))
    fi
    sleep "$poll"
    waited=$(_jvm_waited "$waited" "$poll")
  done
}
jvm_slot_release() {
  [ -n "$JVM_SLOT" ] || return 0
  lock_give 7 "$JVM_SLOT"; JVM_SLOT=""; unset KINOWO_JVM_SLOT_HELD
  return 0
}
jvm_slot_hand_off() { # <pid>
  [ -n "$JVM_SLOT" ] || return 0
  echo "$1" > "$JVM_SLOT"; exec 7>&-; JVM_SLOT=""; unset KINOWO_JVM_SLOT_HELD
  return 0
}
KEEPER_PID=""
jvm_slot_keeper_start() { # <dir> <log> <command...>
  local dir=$1 log=$2; shift 2
  KEEPER_PID=$(cd "$dir" && { set -m; nohup "$@" >> "$log" 2>&1 < /dev/null 6>&- 8>&- 9>&- & echo $!; }) || return 1
  jvm_slot_hand_off "$KEEPER_PID"
}
jvm_slot_take_if_free() { ! jvm_slot_riding && [ "${KINOWO_JVM_SLOTS:-4}" != 0 ] && jvm_slot_try; }

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
