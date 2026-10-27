#!/usr/bin/env bash
# A NO-PROGRESS bound: a step is ended once it has made no progress for N seconds -- no CPU in its process tree, no
# progress mark touched, no log written -- however long it legitimately runs while it does make some. A wall-clock
# budget fails a compile under a load of 100; this fails only a step that waits on nothing that moves.
#
#   scripts/stall-watch.sh watch <root pid> <seconds> <label> <stalled file> [<progress file>] [<dir>]
#       the watchdog: polls every $KINOWO_STALL_POLL_SECONDS (30) and counts as progress (a) the CPU of <root pid>'s
#       descendants (less the watchdog's own) growing by >= $KINOWO_STALL_CPU_CENTIS (100: one second) in a poll, (b)
#       <progress file> touched, (c) a file directly in <dir> written (its logs). <seconds> without any: it appends <label> to
#       <stalled file>, prints `STALLED: <label> (no progress for <seconds>s)` on stderr, and ends the descendants (TERM,
#       KILL 10 s later). It ends itself when <root pid> does; its caller kills it once the step is done.
#   . scripts/stall-watch.sh          the helpers: stall_tree_pids <pid>, stall_cpu_centis [<pid>...]
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

# The CPU the live <pid>s have used, summed, in centiseconds (ps's `time`: [[dd-]hh:]mm:ss[.cc], BSD and procps)
stall_cpu_centis() { # [<pid>...]
  [ $# -gt 0 ] || { echo 0; return 0; }
  ps -o time= -p "$(echo "$@" | tr ' ' ',')" 2> /dev/null | awk '
    { t = $1; d = 0
      if (index(t, "-")) { split(t, x, "-"); d = x[1]; t = x[2] }
      n = split(t, a, ":"); s = 0; for (i = 1; i <= n; i++) s = s * 60 + a[i]
      tot += (s + d * 86400) * 100 }
    END { printf "%d\n", tot }'
}

# Whether <file> was modified since the watchdog's mark <mark> (a file whose mtime is the previous poll)
_stall_newer() { [ -n "$1" ] && [ -e "$1" ] && [ "$1" -nt "$2" ]; }

stall_watch() { # <root pid> <seconds> <label> <stalled file> [<progress file>] [<dir>]
  local root=$1 bound=$2 label=$3 stalled=$4 progress=${5:-} dir=${6:-}
  local poll=${KINOWO_STALL_POLL_SECONDS:-30} need=${KINOWO_STALL_CPU_CENTIS:-100}
  local mark quiet=0 last now mine pids p q moved
  mark=$(mktemp "${TMPDIR:-/tmp}/stall-mark.XXXXXX") || return 1
  trap 'rm -f "$mark"; exit 0' TERM INT
  last=-1
  while kill -0 "$root" 2> /dev/null; do
    sleep "$poll"
    mine=" $(stall_tree_pids $$ | tr '\n' ' ')"
    pids=""
    for p in $(stall_tree_pids "$root" | sed 1d); do case "$mine" in *" $p "*) ;; *) pids="$pids $p" ;; esac; done
    # shellcheck disable=SC2086 # one pid per word
    now=$(stall_cpu_centis $pids)
    moved=0
    [ "$last" -ge 0 ] && [ $((now - last)) -ge "$need" ] && moved=1
    [ "$last" -lt 0 ] && moved=1   # the first poll: no baseline yet
    _stall_newer "$progress" "$mark" && moved=1
    [ -n "$dir" ] && [ -n "$(find "$dir" -maxdepth 1 -type f -newer "$mark" 2> /dev/null | head -1)" ] && moved=1
    touch "$mark"; last=$now
    if [ "$moved" = 1 ]; then quiet=0; continue; fi
    quiet=$(awk -v q="$quiet" -v p="$poll" 'BEGIN { printf "%d", q + p + 0.5 }')
    [ "$quiet" -ge "$bound" ] || continue
    echo "$label" >> "$stalled"
    echo "STALLED: $label (no progress for ${quiet}s: no CPU in its processes, no output, no progress mark)" >&2
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
