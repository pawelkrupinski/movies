#!/usr/bin/env bash
# The assertion harness the shell test scripts share.
#
# WHY IT EXISTS. Four test scripts — scripts/mobile-release-test.sh,
# android/scripts/store-screenshots-test.sh, android/scripts/store-graphics-test.sh
# and ios/scripts/store-screenshots-test.sh — carried a BYTE-IDENTICAL `check`,
# the same `fails=0`, and the same summary-and-exit line. Identical enough that
# their four copies hashed the same.
#
# It is deliberately NOT in store-screenshots-common.sh, which three of the four
# already reach: that file is about locating candidates, baselines and locales
# for the store screenshots. A test harness living inside it would make "the
# screenshot library" also mean "the way we assert things", and the next script
# that wants only assertions would have to drag screenshots in to get them.
#
# scripts/local-mirror/mirror-resilience-test.sh is intentionally left out — its
# harness is a genuinely different one (it reports per-case rather than
# accumulating), and rewriting a passing spec to fit a shared shape is churn, not
# deduplication.
#
# Usage:
#   . "$REPO_ROOT/scripts/shell-spec.sh"
#   check "what it does" "$expected" "$actual"
#   spec_summary            # prints the tally and exits non-zero on any failure
#   spec_bounded / spec_still_running   # a case proving a bound cuts off a hung command (HUNG STUBS, below)

# The JVM gate (scripts/jvm-gate.sh) reads the real Mac's paging and takes machine-wide slots, which a spec's stubbed
# JVM starts have no business waiting on, and sizes JVMs for a share of the cores, which a spec's command lines would
# then vary by: all off in every spec unless it asks for it (jvm-gate-test.sh does). So is the governor (~/bin/agentctl
# gov), whose tickets and evictions are the real Mac's.
export KINOWO_MEMORY_GATE=${KINOWO_MEMORY_GATE:-0} KINOWO_JVM_SLOTS=${KINOWO_JVM_SLOTS:-0} KINOWO_JVM_CPUS=${KINOWO_JVM_CPUS:-0}
export KINOWO_GOVERNOR=${KINOWO_GOVERNOR:-0}
# A spec stubs `gh` on the PATH: scripts/forge-gh would read this checkout's Forgejo remote and ask Forgejo's API
# instead, so every spec is on GitHub's side of it unless it asks for the other (forge-gh's own spec does).
export KINOWO_FORGE=${KINOWO_FORGE:-github}
# ...and a release download asks that stub, never github.com's public asset URL, which gh_download takes by default on a
# github.com runner (tool-conventions.sh): a spec there would otherwise fetch the real release's asset
export GH_DOWNLOAD_PUBLIC=${GH_DOWNLOAD_PUBLIC:-0}
# ...and a spec tests its own checkout's gate, never the root checkout's it would otherwise follow
export KINOWO_JVM_GATE_FOLLOW_MAIN=${KINOWO_JVM_GATE_FOLLOW_MAIN:-0}

# spec_lock_holder <file> -> SPEC_LOCK_HOLDER: the pid of a background process (this shell's child) holding <file>'s
# kernel lock (scripts/jvm-gate.sh), set once it holds it -- another agent's tool mid-download, as a spec sees it
spec_lock_holder() {
  local _
  # shellcheck disable=SC2016 # the child's own $1, $2
  bash -c '. "$1"; jvm_lock "$2" || exit 1; exec sleep 600' _ "$(dirname "${BASH_SOURCE[0]}")/jvm-gate.sh" "$1" > /dev/null 2>&1 &
  SPEC_LOCK_HOLDER=$!
  for _ in $(seq 1 200); do [ "$(cat "$1" 2>/dev/null)" = "$SPEC_LOCK_HOLDER" ] && return 0; sleep 0.05; done
  return 1
}

# spec_lock_state <file> -> "held" while some process holds <file>'s kernel lock, else "free"
spec_lock_state() {
  # shellcheck disable=SC2016 # the child's own $1, $2
  bash -c '. "$1"; if lock_busy "$2"; then echo held; else echo free; fi' _ "$(dirname "${BASH_SOURCE[0]}")/jvm-gate.sh" "$1"
}

# HUNG STUBS. A case proving that a tool's own bound cuts off a command that never answers (a hung gh, mongosh,
# apt-get, a test or chore that never ends) asserts that MECHANISM, never elapsed wall time. Wall time counts every
# process the run starts: "within N s" assertions failed whenever the machine ran the specs ~10x slower (a load of ~86;
# pre-push-test.sh, da73cc2ed). So:
#   - the stub never ends on its own, so the tool gets past it only by its own bound: it records its pid, then blocks
#     until killed --  echo $$ >> "$HUNG"; exec sleep 86400
#   - the case asserts the bound fired (bounded's 124 named, a "timed out" line, the tool's own exit status) and that
#     `spec_still_running "$HUNG"` is 0: no stub left behind;
#   - the run goes under `spec_bounded`, only so a tool that does hang fails the case instead of wedging the spec.
# Never count the stubs that started: that slow, a 1 s bound can end one before it has run its first line.
SPEC_HANG_BOUND=${SPEC_HANG_BOUND:-600} # what a failing case waits; lower it to watch a broken bound fail quickly
_spec_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# spec_bounded <executable> [args...] -> its status, or 124 once it outlasts SPEC_HANG_BOUND (tool-conventions.sh's
# bounded: its whole process group ended). An executable, not a shell function: bounded execs it.
spec_bounded() { ( . "$_spec_dir/tool-conventions.sh" && bounded "$SPEC_HANG_BOUND" "$@" ); }

# spec_still_running <pid file> -> how many of the pids it lists are alive after a grace of up to 10 s each (a TERM
# being delivered); each one still alive is killed, so a failing case strands nothing
spec_still_running() {
  local pid left=0 _
  [ -f "$1" ] || { echo 0; return 0; }
  while read -r pid; do
    [ -n "$pid" ] || continue
    for _ in $(seq 1 100); do kill -0 "$pid" 2>/dev/null || break; command sleep 0.1; done
    kill -0 "$pid" 2>/dev/null && { left=$((left + 1)); kill -KILL "$pid" 2>/dev/null; }
  done < "$1"
  echo "$left"
}

# WAITING FOR SOMETHING. A spec never sleeps a guessed interval and then looks (`sleep 0.3; touch go`), nor polls a
# fixed number of times and falls through when the count runs out: either one passes on an idle laptop and fails, or
# silently tests nothing, on a machine ten times slower. It waits for the condition itself, with a deadline that only a
# broken case reaches -- a wait that holds returns the moment it does, so a generous deadline costs a green run nothing.
SPEC_WAIT_BOUND=${SPEC_WAIT_BOUND:-120}

# wait_until <what> <condition> -> 0 the moment the condition holds; else, after SPEC_WAIT_BOUND seconds, 1, with a
# failed case named <what> counted and the condition's last output shown. <condition> is one shell string, evaluated with
# the caller's variables and functions (in a subshell: it reads them, sets none), or a command with its arguments:
#   wait_until "the holder took the lock" '[ "$(cat "$lock")" = "$holder" ]'
#   wait_until "the tool says it waits" grep -q 'waiting for it' "$work/out"
wait_until() {
  local what=$1 deadline out pause=0.01
  shift
  deadline=$((SECONDS + SPEC_WAIT_BOUND))
  while :; do
    if [ $# -eq 1 ]; then out=$(eval "$1" 2>&1) && return 0
    else out=$("$@" 2>&1) && return 0; fi
    [ "$SECONDS" -lt "$deadline" ] || break
    command sleep "$pause"
    case $pause in 0.01) pause=0.05 ;; 0.05) pause=0.2 ;; esac
  done
  printf '  \033[31m✗\033[0m %s: not within %ss (SPEC_WAIT_BOUND)\n     waited on: %s\n     last output: %s\n' \
    "$what" "$SPEC_WAIT_BOUND" "$*" "${out:-(none)}"
  fails=$((fails + 1))
  return 1
}

# Failure tally. Set here rather than in each caller, which is the third line all
# four repeated.
fails=0

# check <description> <expected> <actual>
check() {
  if [ "$2" = "$3" ]; then printf '  \033[32m✓\033[0m %s\n' "$1"
  else printf '  \033[31m✗\033[0m %s\n     expected: %s\n     actual:   %s\n' "$1" "$2" "$3"; fails=$((fails + 1)); fi
}

# Print the tally and exit with the shell's usual convention: 0 clean, 1 if
# anything failed. Callers end with this instead of repeating the ternary.
spec_summary() {
  if [ "$fails" -eq 0 ]; then printf '\033[32m✓\033[0m all passed\n'
  else printf '\033[31m✗\033[0m %s failed\n' "$fails"; fi
  exit $((fails > 0))
}
