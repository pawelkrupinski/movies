#!/usr/bin/env bash
#
# Assertions for mirror.sh's supervision loop — that a FAILING step recovers on
# the next cycle instead of killing the supervisor for good. No Mongo, no prod,
# no tunnel: mirror.sh is sourced for its functions and every collaborator is
# stubbed, so this runs anywhere bash does.
#
#   scripts/local-mirror/mirror-resilience-spec.sh
#
# Exits 0 when every case passes, 1 on the first failure.
#
# Why this file exists: mirror.sh runs under `set -euo pipefail` with each
# database's `supervise_db` in a background subshell, and a bare non-zero exit
# from any command inside it kills that subshell silently. On 2026-08-02 the
# flyctl tunnel dropped mid-seed, mongosh exited non-zero, and all three
# supervisors died at once — while the parent went on `wait`ing forever, so
# launchd saw a healthy process and never restarted it. Nothing noticed for two
# days. The recovery paths are the whole point of this daemon, so they get a
# test that fails when one of them stops recovering.

set -uo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

failures=0

# Runs a case and reports it. The indirection is load-bearing: bash disables
# `set -e` for the whole dynamic extent of a function invoked as an `if`
# CONDITION — subshells and sourced files included — so writing these as
# `if survives_a_failing_seed; then` would neuter the very option under test and
# the spec would pass against the bug it exists to catch. Called as a plain
# command, `set -e` behaves exactly as it does in the real daemon.
check() {
  local what="$1" case_fn="$2" code
  "$case_fn"
  code=$?
  if [ "$code" -eq 0 ]; then echo "  ok   $what"
  else echo "  FAIL $what"; failures=$((failures + 1)); fi
}

echo "[spec] mirror supervision"

# ── A seed that fails must not end the supervisor ────────────────────────────
# Driven by running supervise_db in a subshell with `reseed` stubbed to fail.
# If the failure propagates (the bug), the loop dies on cycle 1 and cycle 2's
# marker never appears. If it recovers, cycle 2 runs and the marker is written.
#
# The child is bounded three ways so a regression can only ever make this spec
# FAIL, never hang: the stub exits the shell on cycle 2, `sleep` is stubbed to
# return instantly, and the whole thing is capped by a wall-clock watchdog.
survives_a_failing_seed() {
  local work; work="$(mktemp -d -t kinowo-mirror-spec)"
  local cycles="$work/cycles" reached="$work/reached-second-cycle"

  (
    # shellcheck source=/dev/null
    . "$HERE/mirror.sh"                       # functions only — the startup body is guarded

    ensure_local_mongo() { return 0; }
    ensure_tunnel()      { return 0; }
    needs_reseed()       { return 0; }        # always due a seed, so the loop always reaches it
    sleep()              { return 0; }        # no real waiting: backoffs are what we skip past

    # Fails like a mongosh whose connection dropped mid-copy. On the second call
    # the loop has demonstrably survived the first — record it and stop.
    reseed() {
      echo x >> "$cycles"
      [ "$(wc -l < "$cycles")" -ge 2 ] && { touch "$reached"; exit 0; }
      return 1
    }

    supervise_db kinowo 0
  ) >/dev/null 2>&1 &
  local child=$!

  # `supervise_db` loops forever by design, so cap the wait rather than trusting
  # the stubs to end it. Polled here rather than by a background watchdog, whose
  # kill the shell announces as "Terminated" over the spec's own output.
  local waited=0
  while kill -0 "$child" 2>/dev/null && [ "$waited" -lt 200 ]; do
    command sleep 0.1; waited=$((waited + 1))
  done
  kill "$child" 2>/dev/null
  wait "$child" 2>/dev/null

  local ok=1
  [ -f "$reached" ] || ok=0
  rm -rf "$work"
  return $((1 - ok))
}

check "a failed seed retries on the next cycle instead of killing the supervisor" \
  survives_a_failing_seed

# ── Sourcing must be side-effect free ────────────────────────────────────────
# The spec above depends on it, and so does anything else that wants these
# functions: reading .env.local or opening a tunnel at load time would make the
# file untestable and leak a temp file per source.
sourced_cleanly() {
  local out
  out="$( ( . "$HERE/mirror.sh" && declare -F supervise_db >/dev/null && echo SOURCED ) 2>&1 )"
  [ "$out" = "SOURCED" ]
}

check "sourcing defines the functions without reading .env.local or starting anything" \
  sourced_cleanly

if [ "$failures" -gt 0 ]; then echo "[spec] $failures failure(s)"; exit 1; fi
echo "[spec] all cases pass"
