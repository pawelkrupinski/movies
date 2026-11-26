#!/usr/bin/env bash
# Purpose: The JVM gate (sourced): JVM slots, locks and governor tickets for the tools that start heavy JVMs
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
#   jvm_slot_try [label]   (with the governor: one look, its --try)
#   jvm_cpu_options        the JVM options sizing a tool's JVM for KINOWO_JVM_CPUS cores (default 4) on macOS, where
#                          agents share the cores; none on Linux (CI's runner is the JVM's alone) or with KINOWO_JVM_CPUS=0
#   jvm_lock <file>        this process holds <file>'s lock (below) on descriptor 6, its pid written in. 0 when taken,
#                          1 at once when another process holds it
#   jvm_unlock <file>      gives it back when this process holds it
#
# THE GOVERNOR (2026-10-27, the owner: "one governor for laptop memory and CPU"): on the Mac, with ~/bin/agentctl there
# ($KINOWO_GOVERNOR_BIN), the slots and the memory gate give way to its ONE admission control (`agentctl gov`,
# ~/bin/AGENTCTL.md "gov"): a JVM's ticket is admitted by its expected memory (KINOWO_GOV_MEM_GB, default 5) against the
# budget, free memory and pressure, in priority order -- red > check > agent > measure: KINOWO_GOV_CLASS (agentctl sets
# it for a pre-commit check's hook), else an identity-measure* tool is a measurement, a red fixer's checkout
# (*-wt-red-*) red, anything else an agent's own -- and an idle kept sbt server is evicted first when something needs
# room. The ticket is a lock held on descriptor 7, exactly as a slot was: riding it, handing it to a keeper and the
# kernel's release when its holders end all stay. KINOWO_GOVERNOR=0, no agentctl, or Linux (CI's runner) keep the slots.
#   governor_bin           the governor's agentctl when it governs here; 1 otherwise
#   gov_class <label>      the class a JVM for <label> is admitted as
#   gov_job <label>        the job a ticket for <label> is charged CORES for (the owner, 2026-11-18: the governor admits
#                          by CPU as well as memory, ~/bin/AGENTCTL.md "gov"): KINOWO_GOV_JOB, else by the tool --
#                          sbt-warm's own take (and any keeper's, jvm_slot_take_if_free) is its KEEPER's (an idle server's memory, no cores: each call through it
#                          takes a cores ticket, jvm_cpu_take), identity-what-if what-if, identity-slice slice,
#                          identity-measure* measure, any other identity tool's classpath compile compile
#   gov_take <label> [--try]  a ticket on descriptor 7 (JVM_SLOT its lock file, KINOWO_JVM_SLOT_HELD exported): 0 admitted.
#                          Its memory KINOWO_GOV_MEM_GB (5), its cores its gov_job's (KINOWO_GOV_CORES: a number instead)
#   jvm_cpu_take <label> <job>  a CORES-only ticket (no memory: the kept JVM's keeper holds that) on descriptor 7 for one
#                          call into a kept JVM -- an sbt-warm command, a what-if or slice request -- released by
#                          jvm_slot_release. None while riding a parent's ticket, under KINOWO_GOV_CPU_HELD (agentctl's
#                          pre-commit check: its own ticket is charged the hook's cores), or without the governor
#   gov_hold <label> <job> [cores] [mem GB]  a ticket for a heavy job that is no tool's JVM (the owner, 2026-11-19: "if
#                          cpu is starved then pause until we have enough resources and resume when we have too much") --
#                          a script-test run and its Linux container, a Playwright row, an alert backtest, the mirror's
#                          seed: charged <job> with <cores> (none: the job's own figure) and <mem> GB (default 1), held on
#                          descriptor 7 by this process until it ends or jvm_slot_release, so every child rides it. The
#                          wait is the governor's own (a blocked `gov admit`, no poll here); a refused admission is warned
#                          and the job runs. None, 0 at once: without the governor (Linux / CI, KINOWO_GOVERNOR=0, no
#                          agentctl), riding a parent's ticket, under KINOWO_GOV_CPU_HELD (a check's hook: its ticket is
#                          charged the hook's cores already) or holding one already. Class gov_class's (a red fixer's
#                          checkout red: admitted at once). scripts/gov-run is the same for one command
#   gov_run <label> <job> <cores> <mem> <command...>  the command (a function too) under gov_hold's ticket, given back
#                          when it ends: its exit status
#   gov_width <label> <job> <want> [mem GB] [fixed]  THE GOVERNED FAN-OUT (the owner, 2026-11-25: "fix structurally" -- load 103
#                          on 18 cores, `gov status` 0 admitted, 43 test processes ticketless): the width GOV_WIDTH a
#                          parallel runner may use. Under a ticket already (AGENTCTL_GOV_WIDTH, exported by a ticket
#                          charged its cores: a check's hook, gov_hold / gov-run with cores, a pool's own workers) it
#                          takes NONE -- a nested runner never waits on its parent -- and runs at most that wide; else it
#                          takes gov_hold's ticket charged <want> cores (a refused admission: one wide, warned). Its
#                          workers run with AGENTCTL_GOV_WIDTH=1: one core each of the ticket. THE GRANTED WIDTH
#                          (agentctl 58a5ad1, 2026-11-26: a 17-of-18-core ask held every suite): with a governor that
#                          grants (`gov probe`: grant) it asks --min-cores 1 and runs as wide as granted (its ticket
#                          record's cores), never wider than <want>; `fixed` (a pool with a time bound: a 1-core grant
#                          timed agentctl's suite out) asks exactly <want>. The one place a fan-out
#                          is sized: scripts/gov-parallel (xargs -P), scripts/lib/governed_pool.py (a Python pool),
#                          run-script-tests' pool; scripts/ci/fanout_lint.py refuses any other
#   jvm_route <job>        `ci` when the governor would hold <job> now and ci-runner's measure lane is free (`agentctl
#                          gov route`), else `local` -- the reason on stderr. Only an agent's own work (gov_class agent):
#                          local without the governor, riding, under KINOWO_GOV_CPU_HELD (a tool inside a check or a
#                          tool: never routed out from under its parent), for a check or a red fix
#   jvm_usage <job> <wall s> <cpu s> [meter]  records one call's cores in the governor's measured history (`agentctl gov
#                          usage`), with how the CPU was measured: `tree` (cpu_meter_stop) is the only one the governor
#                          counts. The CALLER names it: a worktree's old sbt-warm runs main's copy of this file (FOLLOW
#                          MAIN, below) with its own ps -S figure, which must not read as metered
#   cpu_meter_start <lister>  samples, every KINOWO_CPU_METER_POLL seconds (2) in the background, the CPU time of the
#                          processes <lister> (a function printing root pids, re-run only while its last roots are gone)
#                          names and of all their descendants
#   cpu_meter_stop         -> the CPU seconds those trees used since cpu_meter_start, a process that ended counted to its
#                          last sample. Sampled, not `ps -S`: macOS's ps adds no exited child's CPU to its parent's, so a
#                          server's forked test JVMs went uncounted and every testUnit measured ~0.5 cores while its fork
#                          ran at 5 (2026-11-25: load 83 on 18 cores, 10.5 charged)
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
  python3 -I -B -c 'import fcntl, sys
f = int(sys.argv[1]) if sys.argv[1] != "-" else open(sys.argv[2], "a")
try:
    fcntl.flock(f, fcntl.LOCK_EX | fcntl.LOCK_NB)
except OSError:
    sys.exit(1)' "$@"
}
lock_busy() { [ -e "$1" ] && ! _lock_flock - "$1"; }

GOVERNOR_BIN="" GOV_GRANTED=""
governor_bin() {
  local g=${KINOWO_GOVERNOR_BIN:-$HOME/bin/agentctl} f
  GOVERNOR_BIN=""
  [ "${KINOWO_GOVERNOR:-1}" != 0 ] && [ "${KINOWO_JVM_GATE_OS:-$(uname -s)}" = Darwin ] && [ -x "$g" ] || return 1
  # ITS FEATURES, from its own probe (`agentctl gov probe`), never its source: grepping gov.py for `def cmd_gov` went
  # blind when agentctl's split moved it to govcmd.py (2026-11-22), and for three days movies took no ticket at all
  # while `gov status` said 0 admitted at load 100. Probed once per process tree (KINOWO_GOVERNOR_FEATURES, exported).
  case "${KINOWO_GOVERNOR_FEATURES:-}" in
    "$g|"*) ;;
    *) f=$("$g" gov probe 2> /dev/null) || f=""
       case "$f" in
         "agentctl governor: "*) export KINOWO_GOVERNOR_FEATURES="$g|${f#agentctl governor: }" ;;
         *) echo "jvm-gate: $g answers no \`gov probe\`: no governor ticket, the JVM slots instead" >&2; return 1 ;;
       esac ;;
  esac
  GOVERNOR_BIN=$g
  echo "$g"
}
governor_has() { # <feature> -> 0 when the governor found by governor_bin (in this shell) has it
  case " ${KINOWO_GOVERNOR_FEATURES#*|} " in *" $1 "*) return 0 ;; esac
  return 1
}
gov_class() { # <label>
  if [ -n "${KINOWO_GOV_CLASS:-}" ]; then echo "$KINOWO_GOV_CLASS"; return 0; fi
  case "$1" in identity-measure*) echo measure; return 0 ;; esac
  case "$(basename "$(git rev-parse --show-toplevel 2>/dev/null || pwd)")" in *-wt-red-*) echo red; return 0 ;; esac
  echo agent
}
gov_job() { # <label>
  if [ -n "${KINOWO_GOV_JOB:-}" ]; then echo "$KINOWO_GOV_JOB"; return 0; fi
  case "$1" in
    sbt-warm|keeper) echo keeper ;;
    identity-what-if*) echo what-if ;;
    identity-slice*) echo slice ;;
    identity-measure*) echo measure ;;
    identity*) echo compile ;;
    *) echo job ;;
  esac
}
# an agentctl whose governor charges cores (its `gov route`): an older one refuses --job, and the ticket would fail
gov_take() { # <label> [--try]
  local g label=${1:-tool} ticket cores=()
  shift
  governor_bin > /dev/null || return 1
  g=$GOVERNOR_BIN
  if governor_has cores; then
    cores=(--job "$(gov_job "$label")")
    [ -z "${KINOWO_GOV_CORES:-}" ] || cores+=(--cores "$KINOWO_GOV_CORES")
    # a pool that can run narrower (gov_width): granted what is free, at least this many (THE GRANTED WIDTH)
    [ -z "${KINOWO_GOV_MIN_CORES:-}" ] || ! governor_has grant || cores+=(--min-cores "$KINOWO_GOV_MIN_CORES")
  fi
  ticket="${AGENTCTL_GOV_DIR:-$HOME/.cache/agentctl/gov}/t-$$-$RANDOM$RANDOM.lock"
  mkdir -p "$(dirname "$ticket")" && exec 7>> "$ticket" || return 1
  if "$g" gov admit --fd 7 --ticket "$ticket" --class "$(gov_class "$label")" --mem "${KINOWO_GOV_MEM_GB:-5}" \
      --label "$label" --pid $$ --cwd "$PWD" ${cores[@]+"${cores[@]}"} "$@"; then
    JVM_SLOT=$ticket; export KINOWO_JVM_SLOT_HELD="$ticket"
    # charged a number of cores -- the GRANT when one was asked for (its record's `cores`) -- that is the width every
    # governed fan-out under it may run (gov_width)
    GOV_GRANTED=""
    if [ -n "${KINOWO_GOV_CORES:-}" ]; then
      case " ${cores[*]-} " in *" --min-cores "*) GOV_GRANTED=$(_gov_granted "$ticket") ;; esac
      AGENTCTL_GOV_WIDTH=$(_gov_whole "${GOV_GRANTED:-$KINOWO_GOV_CORES}"); export AGENTCTL_GOV_WIDTH
    fi
    return 0
  fi
  exec 7>&-; rm -f "$ticket"
  return 1
}

jvm_cpu_take() { # <label> <job>
  local g
  governor_bin > /dev/null && governor_has cores || return 0
  g=$GOVERNOR_BIN
  jvm_slot_riding && return 0
  [ -z "${KINOWO_GOV_CPU_HELD:-}" ] || return 0
  [ -z "$JVM_SLOT" ] || return 0   # this process holds a ticket already (its own JVM's): its cores are charged
  KINOWO_GOV_MEM_GB=0 KINOWO_GOV_JOB=$2 gov_take "$1" ||
    echo "[$1] the governor ($g gov admit) failed: running without a cores ticket" >&2
  return 0
}
gov_hold() { # <label> <job> [cores] [mem GB]
  local g
  governor_bin > /dev/null || return 0
  g=$GOVERNOR_BIN
  jvm_slot_riding && return 0
  [ -z "${KINOWO_GOV_CPU_HELD:-}" ] || return 0
  [ -z "$JVM_SLOT" ] || return 0
  KINOWO_GOV_JOB=$2 KINOWO_GOV_CORES=${3:-} KINOWO_GOV_MEM_GB=${4:-1} gov_take "$1" ||
    echo "[$1] the governor ($g gov admit) failed: running without a ticket" >&2
  return 0
}
gov_run() { # <label> <job> <cores> <mem> <command...>
  local label=$1 job=$2 charged=$3 mem=$4
  shift 4
  ( gov_hold "$label" "$job" "$charged" "$mem"; "$@" )
}
# _gov_granted <ticket> -> the cores the governor granted it (its record, <ticket less .lock>.json), or nothing
_gov_granted() {
  python3 -I -B -c 'import json, sys
try:
    print(json.load(open(sys.argv[1][: -len(".lock")] + ".json"))["cores"])
except (OSError, ValueError, KeyError, TypeError):
    pass' "$1" 2> /dev/null
}
# _gov_whole <cores> -> the whole cores, at least 1 ("1.5" -> 1)
_gov_whole() { local n=${1%%.*}; [ "${n:-0}" -ge 1 ] 2> /dev/null && echo "$n" || echo 1; }
# shellcheck disable=SC2034 # GOV_WIDTH: its caller's (gov-parallel, run-script-tests, tool_pool.py)
gov_width() { # <label> <job> <want> [mem GB] [fixed] -> GOV_WIDTH
  local want above=${AGENTCTL_GOV_WIDTH:-}
  want=$(_gov_whole "$3")
  if [ -n "$above" ]; then
    above=$(_gov_whole "$above")
    GOV_WIDTH=$((want < above ? want : above))
    return 0
  fi
  GOV_WIDTH=$want
  governor_bin > /dev/null || return 0
  jvm_slot_riding && return 0
  [ -z "${KINOWO_GOV_CPU_HELD:-}" ] && [ -z "$JVM_SLOT" ] || return 0
  if KINOWO_GOV_JOB=$2 KINOWO_GOV_CORES=$want KINOWO_GOV_MEM_GB=${4:-1} KINOWO_GOV_MIN_CORES=$([ "${5:-}" = fixed ] || echo 1) \
      gov_take "$1"; then
    [ -z "$GOV_GRANTED" ] || GOV_WIDTH=$(_gov_whole "$GOV_GRANTED")
    [ "$GOV_WIDTH" -le "$want" ] || GOV_WIDTH=$want
    return 0
  fi
  GOV_WIDTH=1
  echo "[$1] the governor ($GOVERNOR_BIN gov admit) failed: running one wide, without a ticket" >&2
}
jvm_route() { # <job>
  local g
  # only an agent's own work: a check's or a red fix's runs where it was asked to (the hook judges a landing locally)
  governor_bin > /dev/null && governor_has cores && ! jvm_slot_riding && [ -z "${KINOWO_GOV_CPU_HELD:-}" ] &&
    [ "$(gov_class "$1")" = agent ] || { echo local; return 0; }
  g=$GOVERNOR_BIN
  if "$g" gov route --job "$1" --class agent >&2; then echo ci; else echo local; fi
}
jvm_usage() { # <job> <wall seconds> <cpu seconds> [meter]
  local g
  governor_bin > /dev/null && governor_has cores || return 0
  g=$GOVERNOR_BIN
  "$g" gov usage --job "$1" --wall "$2" --cpu "$3" ${4:+--meter "$4"} > /dev/null 2>&1 || true
}
# _cpu_meter_sample <root pid>... -> "<pid> <CPU centiseconds>" for each root and its descendants, one ps listing
_cpu_meter_sample() {
  [ $# -gt 0 ] || return 0
  ps -axo pid=,ppid=,time= 2> /dev/null | awk -v roots="$*" '
    function centis(t,   d, x, a, n, i, s) {
      d = 0; if (index(t, "-")) { split(t, x, "-"); d = x[1]; t = x[2] }
      n = split(t, a, ":"); s = 0; for (i = 1; i <= n; i++) s = s * 60 + a[i]
      return int((s + d * 86400) * 100) }
    { cpu[$1] = centis($3); kids[$2] = kids[$2] " " $1 }
    END { n = split(roots, q, " "); i = 1
          while (i <= n) { p = q[i++]; if (!(p in cpu) || (p in done)) continue; done[p] = 1; print p, cpu[p]
                           m = split(kids[p], a, " "); for (j = 1; j <= m; j++) q[++n] = a[j] } }'
}
CPU_METER_DIR="" CPU_METER_PID="" CPU_METER_LISTER=""
cpu_meter_start() { # <lister>
  CPU_METER_LISTER=$1
  CPU_METER_DIR=$(mktemp -d "${TMPDIR:-/tmp}/cpu-meter.XXXXXX") || { CPU_METER_DIR=""; return 0; }
  # shellcheck disable=SC2046 # one pid per word
  _cpu_meter_sample $("$CPU_METER_LISTER") > "$CPU_METER_DIR/base"
  cp "$CPU_METER_DIR/base" "$CPU_METER_DIR/seen"
  ( parent=$$ roots=""
    while kill -0 "$parent" 2> /dev/null && [ -d "$CPU_METER_DIR" ]; do
      sleep "${KINOWO_CPU_METER_POLL:-2}"
      # shellcheck disable=SC2086 # one pid per word
      [ -n "$roots" ] && kill -0 $roots 2> /dev/null || roots=$("$CPU_METER_LISTER")
      # shellcheck disable=SC2086
      _cpu_meter_sample $roots >> "$CPU_METER_DIR/seen"
    done
    rm -rf "$CPU_METER_DIR" ) > /dev/null 2>&1 5>&- 6>&- 7>&- 8>&- 9>&- &
  CPU_METER_PID=$!
}
cpu_meter_stop() {
  [ -n "$CPU_METER_DIR" ] || { echo 0; return 0; }
  kill "$CPU_METER_PID" 2> /dev/null; wait "$CPU_METER_PID" 2> /dev/null
  # shellcheck disable=SC2046 # one pid per word
  _cpu_meter_sample $("$CPU_METER_LISTER") >> "$CPU_METER_DIR/seen"
  awk -v base_file="$CPU_METER_DIR/base" 'FILENAME == base_file { base[$1] = $2; next }
       { if (!($1 in top) || $2 > top[$1]) top[$1] = $2 }
       END { for (p in top) { d = top[p] - ((p in base) ? base[p] : 0); if (d > 0) s += d }; printf "%d\n", s / 100 }' \
    "$CPU_METER_DIR/base" "$CPU_METER_DIR/seen"
  rm -rf "$CPU_METER_DIR"; CPU_METER_DIR="" CPU_METER_PID=""
}

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
  governor_bin > /dev/null && return 0   # THE GOVERNOR admits by memory itself
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
jvm_slot_try() { # [label]
  if governor_bin > /dev/null; then gov_take "${1:-tool}" --try; return; fi
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
# Who holds each slot, readably: each holder's pid and what it is (a kept sbt server and its checkout, a tool's keeper),
# "pid 123 (sbt-warm --keep, movies-wt/flip-monitor)"; a pid no longer running is named gone.
jvm_slot_holders() { # <slot dir> <slots>
  local n pid what
  for n in $(seq 1 "$2"); do
    pid=$(cat "$1/$n.flock" 2>/dev/null) || continue
    [ -n "$pid" ] || continue
    what=$(ps -o args= -p "$pid" 2>/dev/null | awk '{
      tool = ""; dir = ""
      for (i = 1; i <= NF; i++) if ($i ~ /\/scripts\/[^\/]+$/ || $i ~ /^scripts\//) { tool = $i; sub(/.*\/scripts\//, "", tool); dir = $i; sub(/\/scripts\/.*/, "", dir); sub(/.*\/projects\//, "", dir); rest = (i < NF) ? " " $(i + 1) : ""; break }
      print (tool != "" ? tool rest (dir != "" && dir != $i ? ", " dir : "") : substr($0, 1, 60)) }')
    printf 'pid %s (%s)\n' "$pid" "${what:-gone}"
  done | paste -sd';' - | sed 's/;/; /g'
}
jvm_slot_take() { # <label>
  # riding needs the slot's descriptor itself: a process outliving its starter has the name but not the lock
  jvm_slot_riding && return 0
  local label=${1:-tool} slots=${KINOWO_JVM_SLOTS:-4} max=${KINOWO_JVM_SLOT_MAX_WAIT:-1800}
  local poll=${KINOWO_JVM_SLOT_POLL:-5} every=${KINOWO_MEMORY_GATE_STATUS_SECONDS:-180} dir n waited=0 said="" next state
  if governor_bin > /dev/null; then
    gov_take "$label" || echo "[$label] the governor ($(governor_bin) gov admit) failed: starting without a ticket" >&2
    return 0
  fi
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
      echo "[$label] all $slots JVM slots busy (KINOWO_JVM_SLOTS; holders: $(for n in $(seq 1 "$slots"); do cat "$dir/$n.flock" 2>/dev/null; done | paste -sd, -)): waiting (up to ${max}s, longer while memory is short)" >&2
      echo "[$label] WAITING for a JVM slot, held by $(jvm_slot_holders "$dir" "$slots"); a kept sbt server holds its slot while idle (its owner frees it: scripts/sbt-warm --stop)" >&2; said=1
    elif [ "$waited" -ge "$next" ]; then
      echo "[$label] all $slots JVM slots still busy after ${waited}s (holders: $(for n in $(seq 1 "$slots"); do cat "$dir/$n.flock" 2>/dev/null; done | paste -sd, -))" >&2
      echo "[$label] still WAITING for a JVM slot, held by $(jvm_slot_holders "$dir" "$slots")" >&2
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
jvm_slot_take_if_free() { ! jvm_slot_riding && [ "${KINOWO_JVM_SLOTS:-4}" != 0 ] && jvm_slot_try "${1:-keeper}"; }

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
