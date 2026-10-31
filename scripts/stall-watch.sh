#!/usr/bin/env bash
# A NO-PROGRESS bound: a step is ended once it has made no progress for N seconds -- no CPU in its process tree, no
# progress mark touched, no log written -- however long it legitimately runs while it does make some. A wall-clock
# budget fails a compile under a load of 100; this fails only a step that waits on nothing that moves.
#
#   scripts/stall-watch.sh watch <root pid> <seconds> <label> <stalled file> [<progress file>] [<dir>]
#       the watchdog: polls every $KINOWO_STALL_POLL_SECONDS (30) and counts as progress (a) the CPU of <root pid>'s
#       descendants (less the watchdog's own, with what their exited children used) growing by >= $KINOWO_STALL_CPU_CENTIS
#       (100: one second) in a poll, (b) one of them runnable at the poll (state R: running, or waiting for a CPU), (c)
#       <progress file> touched, (d) a file directly in <dir> written (its logs). (b) keeps a busy step alive on a loaded
#       machine: under a load of 90 a shell loop forking `date` was granted 5 centiseconds a second, most of them in
#       children gone before the poll (2026-10-27: pre-push-test's busy case judged STALLED). A step that waits on nothing
#       sleeps. (Processes new since the last poll are no progress: the hook's own helper loops start some every second.)
#       <seconds> without any: it appends <label> to
#       <stalled file>, prints `STALLED: <label> (no progress for <seconds>s)` on stderr, and ends the descendants (TERM,
#       KILL 10 s later). It ends itself when <root pid> does; its caller kills it once the step is done.
#       KINOWO_STALL_GROUPS=1: the step runs in process groups of its own, under the root's (run-script-tests: each test
#       under tool-conventions.sh's `bounded`): a stall first says what each of those groups was stuck on
#       (scripts/stall-report.sh, on stderr), then ends their processes alone, never the root's own group -- whose
#       `bounded` then returns the step's status as for any other end.
#   . scripts/stall-watch.sh          the helpers: stall_tree_pids <pid>, stall_cpu_centis [<pid>...],
#                                     stall_any_runnable [<pid>...]
#
# THE STALL SIGNAL, for any caller (agentctl land among them): a stalled step's tool exits 75 (EX_TEMPFAIL) with a
# line `<tool> STALLED: <step> ...` on stderr -- scripts/sbt-warm (`sbt-warm STALLED: <sbt command> ...`) and the
# pre-push hook (`pre-push STALLED: <check> ...`). KINOWO_STALL_FILE names a file each appends the stalled step to, and
# KINOWO_PROGRESS_FILE one sbt-warm touches while its server makes progress: the work of a detached sbt server is no
# CPU of the hook's own process tree, so the hook's watchdog would otherwise see a long compile as a stall.
#
# Why (2026-10-27): a land's pre-push hook sat 2.5 h in `scripts/sbt-warm "common/Test/compile; ..."`, its sbtn client
# and the server at 0% CPU, holding the landing lock while five lands queued behind it: the worktree's ONE sbt server was
# still running an earlier `web/PageTest/runMain tools.FixtureServerMain` (a serving command, from a page-tests-local
# whose shell had died without its clean-up), and sbt runs one command at a time -- the compile queued behind a server
# that never ends. Nothing in sbt-warm, the hook or land had a bound.
set -uo pipefail

# <pid> and every descendant of it, one per line
stall_tree_pids() { # <pid>
  ps -axo pid=,ppid= 2> /dev/null | awk -v root="$1" '
    { kids[$2] = kids[$2] " " $1 }
    END { q[1] = root; n = 1; i = 1
          while (i <= n) { p = q[i++]; print p; m = split(kids[p], a, " "); for (j = 1; j <= m; j++) q[++n] = a[j] } }'
}

# ps's flag that adds the CPU of a process's exited (reaped) children to its own: `-S` (BSD, procps), else none
_stall_children_flag=""
ps -S -o time= -p $$ > /dev/null 2>&1 && _stall_children_flag=-S

# The CPU the live <pid>s have used, summed, with their exited children's where ps can add it, in centiseconds (ps's
# `time`: [[dd-]hh:]mm:ss[.cc], BSD and procps)
stall_cpu_centis() { # [<pid>...]
  [ $# -gt 0 ] || { echo 0; return 0; }
  ps $_stall_children_flag -o time= -p "$(echo "$@" | tr ' ' ',')" 2> /dev/null | awk '
    { t = $1; d = 0
      if (index(t, "-")) { split(t, x, "-"); d = x[1]; t = x[2] }
      n = split(t, a, ":"); s = 0; for (i = 1; i <= n; i++) s = s * 60 + a[i]
      tot += (s + d * 86400) * 100 }
    END { printf "%d\n", tot }'
}

# Whether one of the live <pid>s is runnable (ps state R): running, or ready and waiting for a CPU, as a busy process on
# a loaded machine mostly is. A step that waits on nothing sleeps (S), or is stopped or blocked on I/O.
stall_any_runnable() { # [<pid>...]
  [ $# -gt 0 ] || return 1
  ps -o stat= -p "$(echo "$@" | tr ' ' ',')" 2> /dev/null | grep -q '^R'
}

# Whether <file> was modified since the watchdog's mark <mark> (a file whose mtime is the previous poll)
_stall_newer() { [ -n "$1" ] && [ -e "$1" ] && [ "$1" -nt "$2" ]; }

stall_watch() { # <root pid> <seconds> <label> <stalled file> [<progress file>] [<dir>]
  local root=$1 bound=$2 label=$3 stalled=$4 progress=${5:-} dir=${6:-}
  local poll=${KINOWO_STALL_POLL_SECONDS:-30} need=${KINOWO_STALL_CPU_CENTIS:-100}
  local mark="" quiet=0 last now mine pids p q moved own g groups
  local nap=""
  # ended at once, mid-poll: a trap waits for a foreground `sleep` to end, so the poll sleeps in the background (each
  # check of the hook would otherwise wait up to a poll for its watchdog to go)
  # (HUP too: a watchdog whose terminal or hook went away otherwise left its stall-mark.* in the temp directory)
  # SET BEFORE THE MARK IS MADE: a caller ends its watchdog the moment a quick step is done, often while it is still in
  # `$(mktemp)`. Untrapped, that TERM ended the shell mid-substitution: mktemp wrote into a pipe nobody read ("mktemp:
  # failed to print directory name: Broken pipe" on the watchdog's stderr, which run-script-tests appends to the test's
  # own log -- Tool self-tests 41193766567 on github.com) and its stall-mark.* was left behind. Trapped, bash runs the
  # trap once the substitution is done, with the mark known, and removes it.
  trap 'kill $nap 2> /dev/null; [ -n "$mark" ] && rm -f "$mark"; exit 0' TERM INT HUP
  mark=$(mktemp "${TMPDIR:-/tmp}/stall-mark.XXXXXX") || return 1
  last=-1
  while kill -0 "$root" 2> /dev/null; do
    sleep "$poll" & nap=$!; wait "$nap"; nap=""
    mine=" $(stall_tree_pids $$ | tr '\n' ' ')"
    pids=""
    for p in $(stall_tree_pids "$root" | sed 1d); do case "$mine" in *" $p "*) ;; *) pids="$pids $p" ;; esac; done
    # shellcheck disable=SC2086 # one pid per word
    now=$(stall_cpu_centis $pids)
    moved=0
    [ "$last" -ge 0 ] && [ $((now - last)) -ge "$need" ] && moved=1
    [ "$last" -lt 0 ] && moved=1   # the first poll: no baseline yet
    # shellcheck disable=SC2086
    stall_any_runnable $pids && moved=1
    _stall_newer "$progress" "$mark" && moved=1
    [ -n "$dir" ] && [ -n "$(find "$dir" -maxdepth 1 -type f -newer "$mark" 2> /dev/null | head -1)" ] && moved=1
    touch "$mark"; last=$now
    if [ "$moved" = 1 ]; then quiet=0; continue; fi
    quiet=$(awk -v q="$quiet" -v p="$poll" 'BEGIN { printf "%d", q + p + 0.5 }')
    [ "$quiet" -ge "$bound" ] || continue
    echo "$label" >> "$stalled"
    echo "STALLED: $label (no progress for ${quiet}s: no CPU in its processes, no output, no progress mark)" >&2
    if [ "${KINOWO_STALL_GROUPS:-}" = 1 ] && [ -n "$pids" ]; then
      own=$(ps -o pgid= -p "$root" 2> /dev/null | tr -d ' ')
      # shellcheck disable=SC2086 # one pid per word
      groups=$(ps -o pid=,pgid= -p "$(echo $pids | tr ' ' ',')" 2> /dev/null | awk -v own="$own" '$2 != own { print $2 }' | sort -u)
      # shellcheck disable=SC2086
      pids=$(ps -o pid=,pgid= -p "$(echo $pids | tr ' ' ',')" 2> /dev/null | awk -v own="$own" '$2 != own { print $1 }' | tr '\n' ' ')
      for g in $groups; do bash "$(dirname "${BASH_SOURCE[0]}")/stall-report.sh" "$g"; done
    fi
    # shellcheck disable=SC2086
    [ -n "$pids" ] && kill -TERM $pids 2> /dev/null
    for _ in $(seq 1 20); do
      p=""; for q in $pids; do kill -0 "$q" 2> /dev/null && p="$p $q"; done
      [ -z "$p" ] && break; sleep 0.5
    done
    # shellcheck disable=SC2086
    [ -n "$p" ] && kill -KILL $p 2> /dev/null
    break
  done
  rm -f "$mark"
}

if [ "${BASH_SOURCE[0]}" = "$0" ]; then
  case "${1:-}" in
    watch) shift; [ $# -ge 4 ] || { echo "usage: stall-watch.sh watch <root pid> <seconds> <label> <stalled file> [<progress file>] [<dir>]" >&2; exit 2; }
      stall_watch "$@" ;;
    *) sed -n '2,/^set /p' "$0" | sed -e '$d' -e 's/^# \{0,1\}//'; exit 2 ;;
  esac
fi
