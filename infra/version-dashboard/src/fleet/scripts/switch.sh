#!/usr/bin/env bash
# Fed to `bash -s -- <closure>` on the host over ssh by FleetJobs. Activates exactly that closure.
set -uo pipefail
staged="$1"
[ -e "$staged" ] || { echo "!! $staged is not on this host any more"; exit 5; }
# STILL THE PIN, re-read here rather than trusted from the check: FleetJobs accepts any closure a
# check ever read, and CI may have staged a newer one since -- a stale tab would then roll the host
# back to what CI already replaced. Same pin path as check.sh.
pinned=$(readlink -f /var/lib/nixdeploy/staged-system 2>/dev/null || true)
if [ "$pinned" != "$staged" ]; then
  echo "!! $staged is no longer the closure staged on this host (the pin is ${pinned:-empty}); run the check again"
  exit 7
fi
running=$(readlink -f /run/current-system 2>/dev/null || true)
if [ "$staged" = "$running" ]; then
  echo "· already running it — nothing to do"
  echo "@@ DONE"
  exit 0
fi
# THE PROFILE IS SET FIRST, AND THAT ORDER IS LOAD-BEARING -- the same order, for the same reason,
# as nix/files/nixos-auto-apply.py: switch-to-configuration re-reads /nix/var/nix/profiles/system,
# so activating without setting it leaves the running system pointing at the closure it just
# replaced. Someone "tidying" these two lines into the other order gets a host that activated the
# new configuration and will roll back to the old one at next boot.
echo "· nix-env --profile /nix/var/nix/profiles/system --set $staged"
if ! nix-env --profile /nix/var/nix/profiles/system --set "$staged"; then
  echo "!! could not set the system profile; nothing was activated"
  exit 6
fi
echo "· switch-to-configuration switch"
"$staged/bin/switch-to-configuration" switch
rc=$?
echo "· switch-to-configuration exited $rc"
now=$(readlink -f /run/current-system 2>/dev/null || true)
echo "· now running $now"
# THE VERDICT IS WHAT /run/current-system POINTS AT, not the exit code. switch-to-configuration
# exits non-zero when any one unit fails to come back, which is worth reporting but is not the
# same statement as "the closure was not activated" -- and conflating them would tell an operator
# to retry a switch that has already happened.
if [ "$now" = "$staged" ]; then echo "@@ DONE"; else echo "@@ FAILED"; fi
exit "$rc"
