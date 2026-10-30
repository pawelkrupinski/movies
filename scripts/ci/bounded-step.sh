#!/usr/bin/env bash
# bounded-step.sh: a step with its own `timeout-minutes` runs as a process group this script can end, so what it leaves
# running when it times out is ended before anything else reads the workspace.
#
# Why: Forgejo's runner (act) does not end a step that outlives its `timeout-minutes` -- it gives up on the step's
# `docker exec`, logs "context deadline exceeded", and goes on to the next step while the step's processes run on in
# the job container. Convergence run #1880 (2026-10-26): a fill step's sbt went on compiling past its 5 minutes and
# raced the suite's sbt into one classes directory. scripts/ci/stop-stray-sbt.sh ends stray JVMs; this ends ANY
# process tree a timed-out step left, whatever it runs (a tunnel, a capture, xcodebuild, a python refit).
#
# In a step (the first line of its `run:`; WorkflowPlanSpec holds every step with a `timeout-minutes` to it):
#
#     . scripts/ci/bounded-step.sh
#     <the step's script, unchanged>
#
# Sourced, it first ends what an earlier bounded step left (below), then runs the step's own script file ($0, as the
# runner wrote it) again in a new process group, marked BOUNDED_STEP=<id> in its environment, and records the group
# under $RUNNER_TEMP/bounded-steps/<id>. A step that ENDS -- pass or fail -- removes its record: whatever it started in
# the background on purpose (a tunnel, mongod) is the job's from then on. Only a step still running when the
# runner gave up on it leaves a record behind.
#
#     scripts/ci/bounded-step.sh --reap      # an `if: always()` step after a bounded one, before anything that
#                                            # reads the workspace or saves a cache (post steps run after it)
#
# Ends every recorded group, and every process whose environment carries a recorded id (a daemon that left its
# group, e.g. an sbt server), TERM then KILL after 10 s; prints each one. Exit 0 whether or not there was one; 1 when
# one survived KILL.

bounded_step_dir() { echo "${RUNNER_TEMP:-${TMPDIR:-/tmp}}/bounded-steps"; }

# The pids carrying BOUNDED_STEP=<id> in their environment (Linux; nothing elsewhere, where the group must do).
bounded_step_marked() {
  local id=$1 proc
  [ -e /proc/self/environ ] || return 0
  for proc in /proc/[0-9]*; do
    tr '\0' '\n' 2>/dev/null < "$proc/environ" | grep -qx "BOUNDED_STEP=$id" && echo "${proc#/proc/}"
  done
  return 0
}

bounded_step_reap() {
  local dir record id pgid what ids=() pids=() survivors=0 p n clean
  dir=$(bounded_step_dir)
  [ -d "$dir" ] || return 0
  for record in "$dir"/*; do
    [ -f "$record" ] || continue
    id=${record##*/}
    ids+=("$id")
    read -r pgid what < "$record" || true
    n=0
    if [ -n "$pgid" ] && kill -0 -- "-$pgid" 2>/dev/null; then
      kill -TERM -- "-$pgid" 2>/dev/null && n=1
    fi
    for p in $(bounded_step_marked "$id"); do
      [ "$p" = "$$" ] && continue
      kill -TERM "$p" 2>/dev/null && pids+=("$p") && n=1
    done
    [ "$n" = 1 ] && echo "::warning::ending what a timed-out step left running (${what:-a bounded step}, process group $pgid)"
    pids+=("-$pgid")
  done
  # The marks are looked for again on every pass, not once: a process mid-exec reads an empty environ, and one the
  # group starts as it ends (a TERM trap, a daemon's late fork) leaves it after the first look (Tool self-tests #2866).
  signal_marked() {
    local signal=$1 id p
    for id in ${ids[@]+"${ids[@]}"}; do
      for p in $(bounded_step_marked "$id"); do
        [ "$p" = "$$" ] && continue
        kill "-$signal" "$p" 2>/dev/null && pids+=("$p")
      done
    done
    return 0
  }
  alive() { local q; for q in "${pids[@]}"; do kill -0 -- "$q" 2>/dev/null && return 0; done; return 1; }
  # Gone only after two clean looks in a row, a pass apart: one can fall inside an exec.
  wait_gone() {
    local signal=$1 passes=$2
    clean=0
    for _ in $(seq 1 "$passes"); do
      signal_marked "$signal"
      if alive; then clean=0; else clean=$((clean + 1)); [ "$clean" -ge 2 ] && return 0; fi
      sleep 0.1
    done
    return 1
  }
  if [ "${#pids[@]}" -gt 0 ]; then
    if ! wait_gone TERM 100; then
      for p in "${pids[@]}"; do kill -KILL -- "$p" 2>/dev/null || true; done
      wait_gone KILL 50 || survivors=1
    fi
  fi
  for record in "$dir"/*; do [ -f "$record" ] && rm -f "$record"; done
  if [ "$survivors" = 1 ]; then
    echo "::error::a process a timed-out step left survived KILL" >&2
    return 1
  fi
  return 0
}

if [ "${BASH_SOURCE[0]}" = "$0" ]; then
  # Run, not sourced: the reap step.
  case "${1:-}" in
    --reap) bounded_step_reap; exit $? ;;
    *) echo "usage: . scripts/ci/bounded-step.sh (first line of a step) | scripts/ci/bounded-step.sh --reap" >&2; exit 2 ;;
  esac
fi

# Sourced. Inside the bounded run (the re-run of the step's own script): nothing to do, the script goes on.
if [ -z "${BOUNDED_STEP:-}" ]; then
  bounded_step_reap || exit 1
  BOUNDED_STEP="$(date +%s)-$$-${RANDOM}"
  export BOUNDED_STEP
  mkdir -p "$(bounded_step_dir)"
  # The step's script again, under the options the runner gave it: `shell: bash` is `-eo pipefail`, the default
  # shell `-e` alone, and a pipeline's exit status must not change by being bounded.
  _bounded_opts=()
  case $- in *e*) _bounded_opts+=(-e) ;; esac
  if shopt -qo pipefail; then _bounded_opts+=(-o pipefail); fi
  set -m
  bash --noprofile --norc ${_bounded_opts[@]+"${_bounded_opts[@]}"} "$0" "$@" &
  _bounded_pid=$!
  set +m
  echo "$_bounded_pid $0" > "$(bounded_step_dir)/$BOUNDED_STEP"
  # A runner that does signal the step (github.com cancels this way) ends the whole group, not this shell alone.
  trap 'kill -TERM -- "-$_bounded_pid" 2>/dev/null' TERM INT HUP
  _bounded_rc=0
  wait "$_bounded_pid" || _bounded_rc=$?
  # A trapped signal interrupts `wait`; wait for the group's leader to actually end.
  while kill -0 "$_bounded_pid" 2>/dev/null; do wait "$_bounded_pid" || _bounded_rc=$?; done
  rm -f "$(bounded_step_dir)/$BOUNDED_STEP"
  exit "$_bounded_rc"
fi
