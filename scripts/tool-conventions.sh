#!/usr/bin/env bash
# What every Bash tool in this repository shares, one copy each: a bounded command that works on stock macOS, the usage
# text read from a tool's own header, the country codes, and the shared identity cache's path. Sourcing it defines
# functions and constants only (and exports KINOWO_IDENTITY_CACHE, so a Python tool started from a Bash one reads the
# same cache). bash 3.2 compatible: no mapfile, no associative arrays, no ${x,,}.
#
# THE CONVENTIONS
#   set -euo pipefail   a tool whose every step must succeed for its answer to mean anything: the first failure ends it.
#   set -uo pipefail    (no -e) a spec, which runs every check and tallies them (scripts/shell-spec.sh), and a CI step or
#                       tool that decides what each failure means — 2 "no verdict" against 1 "failed", a retry, a step
#                       that carries on with less. Such a script checks every command whose failure matters itself
#                       (`|| die`, `|| return 1`, `rc=$?`); a failure it leaves unchecked is one carrying on past is right for.
#   Every network call (gh, curl, git fetch/push/ls-remote) runs under `bounded <seconds>`: a hung API never holds a
#   tool, a push or a CI row forever.
#   A temporary file or directory is removed by a trap set where it is made: Ctrl-C does not leak it.
#   A write into a cache another agent reads goes through identity_locked/identity_put (scripts/identity-tools.sh).
#   -h/--help prints the tool's header comment: usage_from_header.
#
#   bounded <seconds> <command...>  -> the command, ended (its whole process group: TERM, then KILL seconds later) once it
#        outlasts <seconds>: exit 124 then, else the command's own status. `timeout` (GNU coreutils), else `gtimeout`
#        (Homebrew's coreutils), else perl — stock macOS has neither.
#   usage_from_header [file]        -> the header comment of <file> (default the running script, $0): from line 2 to the
#        first `set ` line, the `# ` stripped, a leading blank line dropped
#   KINOWO_COUNTRIES                -> the country codes every tool loops over, in one order (kinowo_countries_csv: "pl,de,…")
#   KINOWO_IDENTITY_CACHE           -> the cache every identity tool shares (default ~/.cache/kinowo-identity)

KINOWO_COUNTRIES=(pl de uk us es)
kinowo_countries_csv() { local IFS=,; echo "${KINOWO_COUNTRIES[*]}"; }

KINOWO_IDENTITY_CACHE=${KINOWO_IDENTITY_CACHE:-$HOME/.cache/kinowo-identity}
export KINOWO_IDENTITY_CACHE

bounded() {
  local seconds=$1; shift
  if command -v timeout > /dev/null 2>&1; then timeout -k 5 "$seconds" "$@"
  elif command -v gtimeout > /dev/null 2>&1; then gtimeout -k 5 "$seconds" "$@"
  else
    # timeout's semantics: the command in a process group of its own (an sbt-forked JVM is ended with it), TERM at the
    # deadline then KILL, 124; this shell's own INT/TERM passed on to the group.
    perl -e '
      my $seconds = shift;
      my $pid = fork() // die "fork: $!";
      if ($pid == 0) { setpgrp(0, 0); exec @ARGV or exit 127; }
      my $stop = sub { kill "TERM", -$pid; sleep 2; kill "KILL", -$pid; exit $_[0]; };
      $SIG{ALRM} = sub { $stop->(124) };
      $SIG{INT} = sub { $stop->(130) };
      $SIG{TERM} = sub { $stop->(143) };
      alarm $seconds;
      waitpid($pid, 0);
      exit($? & 127 ? 128 + ($? & 127) : $? >> 8);' "$seconds" "$@"
  fi
}

usage_from_header() { sed -n '2,/^set /p' "${1:-$0}" | sed -e '$d' -e 's/^# \{0,1\}//' -e 's/^#$//' | sed -e '1{/^$/d;}'; }
