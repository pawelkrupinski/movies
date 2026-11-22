# Purpose: Sourced, never run: `memory_guard_stop <guard pid> <reason file>` ends the scripts/ci/memory-guard.sh started beside a process
# shellcheck shell=bash
# Sourced, never run: `memory_guard_stop <guard pid> <reason file>` ends the scripts/ci/memory-guard.sh started beside a
# process, once that process is done. A guard that is ending the tree (it wrote <reason file>) finishes its KILL pass;
# an idle one is stopped now, not a sample later. Either way the call succeeds.
#
# Why it is a function: the guard exits by itself once its target is gone, so `kill "$guard"` can find it already gone.
# Under `bash -e` (every workflow step) that failing kill, the last command of an `||` list, ended the step: a passing US
# suite failed exit 1 (Identity model convergence #1342). Its callers: .github/workflows/country-convergence-leg.yml's
# suite step (scripts/ci/identity_drift_check_replay.py's stop_guard is its Python copy). Test: scripts/ci/memory-guard-test.sh.
memory_guard_stop() { # <guard pid> <reason file>
  [ -s "$2" ] || kill "$1" 2> /dev/null || true
  wait "$1" 2> /dev/null || true
}
