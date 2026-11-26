#!/usr/bin/env bash
# Purpose: A no-progress bound: a step is ended once it has made no progress for N seconds
# A NO-PROGRESS bound: a step is ended once it has made no progress for N seconds -- no CPU in its process tree, no
# progress mark touched, no log written -- however long it legitimately runs while it does make some. A wall-clock
# budget fails a compile under a load of 100; this fails only a step that waits on nothing that moves.
#
#   scripts/stall-watch.sh watch <root pid> <seconds> <label> <stalled file> [<progress file>] [<dir>]
#       the watchdog, scripts/stall_watch.py (its header says how it judges progress and what a stall does); this name
#       stays: run-script-tests and the pre-push hook run `bash scripts/stall-watch.sh watch ...`
#   . scripts/stall-watch.sh          the helpers: stall_tree_pids <pid>, stall_cpu_centis [<pid>...],
#                                     stall_any_runnable [<pid>...], stall_tree_shrank <before> <after>,
#                                     stall_any_gone [<pid>...], stall_kept_pids <before> <after>
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

# Whether the tree SHRANK between two polls: a pid of <before> (whitespace-separated) is not in <after>, and every pid
# of <after> was in <before>. sbt-warm's client watch counts it as progress: the CPU of a process that ended leaves the
# sum, and a client winding down on a loaded machine (its sbt done, the wrapper reaping it) otherwise reads as quiet.
# A process ending beside a new one is no progress: a tree that keeps spawning short-lived children (a retry loop's
# `sleep`) ends one every poll, and counting any end let such a tree run for ever (2026-11-16 review) -- one doing real
# work through its children shows that work as their CPU, which ps -S adds to the parent that reaps them.
stall_tree_shrank() { # <before> <after>
  local p ended=1 before after
  # shellcheck disable=SC2086 # whitespace-separated pids, newlines folded
  before=" $(printf '%s ' $1)" after=" $(printf '%s ' $2)"
  for p in $after; do case "$before" in *" $p "*) ;; *) return 1 ;; esac; done
  for p in $before; do case "$after" in *" $p "*) ;; *) ended=0 ;; esac; done
  return "$ended"
}

# Whether one of the <pid>s has ended (gone, or a zombie not yet waited for). Asked after a measure of their CPU, it
# says whether that measure missed one: a process that ended before it is out of the sum, one alive after it was in it.
# Ask it of the processes the last poll measured too (stall_kept_pids): only their end takes CPU out of the sum that
# poll's measure is compared with. A process new since then adds nothing to that baseline, and whatever it used reaches
# the sum through its parent once reaped (ps -S). Asked of every listed process, a spawn loop's `sleep` ending while a
# slow `ps` measured read as progress (2026-11-17: sbt-warm-test's spawn-loop case never stalled at a load of 17).
stall_any_gone() { # [<pid>...]
  [ $# -gt 0 ] || return 1
  [ "$(ps -o stat= -p "$(echo "$@" | tr ' ' ',')" 2> /dev/null | grep -cv '^Z')" -lt $# ]
}

# The pids of <after> that were in <before> too (whitespace-separated lists), one per line
stall_kept_pids() { # <before> <after>
  local p before
  # shellcheck disable=SC2086 # whitespace-separated pids, newlines folded
  before=" $(printf '%s ' $1)"
  for p in $2; do case "$before" in *" $p "*) echo "$p" ;; esac; done
}

# stall_python -> the first python3 on PATH that answers a probe: the watchdog never runs on the interpreter a watched
# check may be stuck in. A python3 shadowed on PATH that hangs (a test's hung stub that answers `-c` alone, a broken
# shim) hung the watchdog with the check it was there to end (Tool self-tests #600: pre-push-tools-test's stall case
# sat 300 s on two `sleep 86400`, its check's and its watchdog's).
stall_python() {
  local dir py IFS=:
  for dir in $PATH; do
    py="${dir:-.}/python3"
    [ -x "$py" ] || continue
    [ "$("$py" -c 'print("stall-watch")' 2> /dev/null < /dev/null)" = stall-watch ] && { echo "$py"; return 0; }
  done
  return 1
}

if [ "${BASH_SOURCE[0]}" = "$0" ]; then
  py=$(stall_python) || { echo "stall-watch: no python3 on PATH answers" >&2; exit 2; }
  exec "$py" -I "$(dirname "$0")/stall_watch.py" "$@"
fi
