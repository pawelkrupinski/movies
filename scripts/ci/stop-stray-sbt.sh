#!/usr/bin/env bash
# stop-stray-sbt.sh: end every JVM an earlier step left running in this build, so the step about to compile (or the
# cache save about to read target/) is the build's only writer.
#
# Why: Forgejo's runner (act) does not end a step that outlives its `timeout-minutes` -- it gives up on the step's
# `docker exec`, logs "context deadline exceeded", and goes on to the next step while the step's processes run on in
# the job container. Convergence run #1880 (germany row, 2026-10-26): the pre-suite fill's sbt passed the fill step's
# 5 minutes mid-compile (identity's second cycle, no "done compiling"), went on compiling -- the suite's sbt found
# identity already compiled, which only the abandoned one could have done -- and reached worker while the suite's sbt
# compiled worker into the same classes directory. Two zincs moving and rewriting one module's class files: the suite
# failed on "value forCountry is not a member of services.metrics.IdentityModelGauges", a member both the tree and the
# restored cache have. Main saves the build cache at the job's end, so a stray writer there poisons every restore.
#
#   scripts/ci/stop-stray-sbt.sh [build root]   # default: the current directory
#
# Ends each `java` (its command line's program) whose working directory is the build root or under it (sbt, and any
# JVM it forked), TERM then KILL after 10 s, and prints each one it ended. JVMs of other builds (another job's, another checkout's) are left alone.
# Exit 0 whether or not there was one; 1 when one survived KILL.
set -euo pipefail

root=$(cd "${1:-.}" && pwd -P)

# Each process as `<pid> <program> <working directory>`: off /proc on Linux (the job image need not carry ps), else ps
# and lsof (macOS, where the self-test also runs).
processes() {
  local proc pid program
  if [ -e /proc/self/cwd ]; then
    for proc in /proc/[0-9]*; do
      pid=${proc#/proc/}
      program=$(tr '\0' '\n' 2>/dev/null < "$proc/cmdline" | head -1) || continue
      echo "$pid ${program:-?} $(readlink "$proc/cwd" 2>/dev/null)"
    done
  else
    ps -A -o pid= -o args= | while read -r pid program _; do
      [ "${program##*/}" = java ] || continue
      echo "$pid $program $(lsof -a -d cwd -p "$pid" -Fn 2>/dev/null | sed -n 's/^n//p' | head -1)"
    done
  fi
}

strays=()
while read -r pid program dir; do
  [ "${program##*/}" = java ] || continue
  case "$dir" in "$root" | "$root"/*) strays+=("$pid") ;; esac
done < <(processes)

[ "${#strays[@]}" -gt 0 ] || exit 0

for pid in "${strays[@]}"; do
  echo "::warning::ending a JVM an earlier step left running in this build (pid $pid)"
  kill -TERM "$pid" 2>/dev/null || true
done
alive() { local p; for p in "${strays[@]}"; do kill -0 "$p" 2>/dev/null && return 0; done; return 1; }
for _ in $(seq 1 100); do alive || exit 0; sleep 0.1; done
for pid in "${strays[@]}"; do kill -KILL "$pid" 2>/dev/null || true; done
for _ in $(seq 1 50); do alive || exit 0; sleep 0.1; done
echo "::error::a stray JVM in $root survived KILL" >&2
exit 1
