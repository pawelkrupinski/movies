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
#   bounded <seconds> <command...>  -> the command, ended once it outlasts <seconds> (exit 124), else its own status.
#        timeout(1)'s contract without timeout(1), which stock macOS lacks: the command runs in a process group of its
#        own, ended whole (TERM, then KILL two seconds later: an sbt-forked JVM too); and unlike GNU timeout, which
#        leaves the terminal's group, a Ctrl-C reaches it — passed on to the group, then to the caller as the signal.
#   usage_from_header [file]        -> the header comment of <file> (default the running script, $0): from line 2 to the
#        first `set ` line, the `# ` stripped, a leading blank line dropped
#   KINOWO_COUNTRIES                -> the country codes every tool loops over, in one order (kinowo_countries_csv: "pl,de,…")
#   KINOWO_IDENTITY_CACHE           -> the cache every identity tool shares (default ~/.cache/kinowo-identity)
#   local_mongo_port / local_mongo_uri [database] -> the local, password-free Mongo the prod mirror, the it/ suites
#        and the identity tools share: 127.0.0.1:$LOCAL_MIRROR_PORT (default 28017), a direct connection

KINOWO_COUNTRIES=(pl de uk us es)
kinowo_countries_csv() { local IFS=,; echo "${KINOWO_COUNTRIES[*]}"; }

KINOWO_IDENTITY_CACHE=${KINOWO_IDENTITY_CACHE:-$HOME/.cache/kinowo-identity}
export KINOWO_IDENTITY_CACHE

local_mongo_port() { echo "${LOCAL_MIRROR_PORT:-28017}"; }
local_mongo_uri() { echo "mongodb://127.0.0.1:$(local_mongo_port)/${1:-}?directConnection=true"; }

bounded() {
  perl -e '
    use POSIX ":sys_wait_h";
    my $seconds = shift;
    my $pid = fork() // die "fork: $!";
    if ($pid == 0) { setpgrp(0, 0); exec @ARGV or exit 127; }
    # the whole group: TERM, a moment to go, then KILL whatever is left of it (a forked JVM included)
    my $stop = sub {
      my ($code, $signal) = @_;
      kill "TERM", -$pid;
      for (1 .. 20) { last if waitpid($pid, WNOHANG) == $pid; select(undef, undef, undef, 0.1); }
      kill "KILL", -$pid;
      # a signal is passed on as one: the calling shell sees it interrupted, as it would the command itself
      if ($signal) { $SIG{$signal} = "DEFAULT"; kill $signal, $$; }
      exit $code;
    };
    $SIG{ALRM} = sub { $stop->(124) };
    $SIG{INT} = sub { $stop->(130, "INT") };
    $SIG{TERM} = sub { $stop->(143, "TERM") };
    alarm $seconds;
    waitpid($pid, 0);
    exit($? & 127 ? 128 + ($? & 127) : $? >> 8);' "$@"
}

usage_from_header() { sed -n '2,/^set /p' "${1:-$0}" | sed -e '$d' -e 's/^# \{0,1\}//' -e 's/^#$//' | sed -e '1{/^$/d;}'; }
