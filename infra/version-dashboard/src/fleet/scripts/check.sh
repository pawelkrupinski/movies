#!/usr/bin/env bash
# Fed to `bash -s` on the host over ssh by FleetJobs. Reads the pin, dry-activates, changes nothing.
set -uo pipefail
# Where CI pins the closure it staged: the same default as nix/modules/fleet/auto-apply.nix's
# `stagedSystem`, restated rather than read out of the flake because this must be able to name it
# while a `nix eval` is failing -- the moment the roster cannot be read is exactly the moment
# somebody wants the button.
pin=/var/lib/nixdeploy/staged-system
echo "· connected to $(hostname) as $(id -un)"
running=$(readlink -f /run/current-system 2>/dev/null || true)
echo "· running   ${running:-unknown}"
booted=$(readlink -f /run/booted-system 2>/dev/null || true)
if [ -n "$booted" ] && [ "$booted" != "$running" ]; then
  echo "· booted    $booted  (differs from current: ordinary residue of an earlier switch)"
fi
staged=$(readlink -f "$pin" 2>/dev/null || true)
if [ -z "$staged" ] || [ ! -e "$staged" ]; then
  echo "· staged    nothing is pinned at $pin — CI has never staged a closure here, or the"
  echo "·           gcroot went with it. There is nothing for this button to activate."
  exit 0
fi
echo "· staged    $staged"
if [ -x "$staged/sw/bin/nixos-version" ]; then
  echo "· staged revision $("$staged/sw/bin/nixos-version" --json 2>/dev/null || echo unknown)"
fi
if [ "$staged" = "$running" ]; then
  echo "· this host is already running the staged closure — there is nothing here to activate"
  exit 0
fi
echo ""
echo "--- switch-to-configuration dry-activate: what activating it WOULD disturb ---"
# Captured as well as streamed, so the failure branch below can READ the output rather than guess
# at it from an exit code. `tee` keeps the operator's console live either way.
dry=$(mktemp)
"$staged/bin/switch-to-configuration" dry-activate 2>&1 | tee "$dry"
rc=${PIPESTATUS[0]}
echo "--- dry run exited $rc — NOTHING on this host has been changed ---"
if [ "$rc" -ne 0 ]; then
  # THE TWO FAILURES WANT OPPOSITE RESPONSES and neither is legible from "exited 11", which is why
  # they are told apart here rather than left as one error. A held lock is somebody else's
  # activation in progress -- most likely nixos-auto-apply's own timer, which runs on these hosts
  # -- and it clears by itself. Anything else did not complete, so what activating would disturb is
  # UNKNOWN, and offering a switch on the back of a dry run that never finished would be offering
  # to activate something nothing has checked.
  if grep -qi 'could not acquire lock' "$dry"; then
    echo "!! Something else is activating on this host: switch-to-configuration could not take"
    echo "!! its lock. The lock is held only while a process runs, so this clears by itself —"
    echo "!! wait for nixos-auto-apply's run to finish and check again."
    # The lock file existing proves nothing (flock is released when the holder exits), but a
    # HOLDER is findable, and naming it beats sending someone to wait for something that is not
    # actually running.
    holder=""
    for fd in /proc/[0-9]*/fd/*; do
      case "$(readlink "$fd" 2>/dev/null)" in
        */switch-to-configuration.lock)
          pid=${fd#/proc/}; pid=${pid%%/*}
          holder="$holder $pid($(tr '\0' ' ' < "/proc/$pid/cmdline" 2>/dev/null | cut -c1-60))" ;;
      esac
    done
    if [ -n "$holder" ]; then
      echo "!! Held by:$holder"
    else
      echo "!! No holder is visible now, so it has already finished — check again."
    fi
  else
    echo "!! The dry run did not complete, so what activating this closure would disturb is"
    echo "!! UNKNOWN — which is why no switch is offered. Read the output above."
  fi
  rm -f "$dry"
  exit "$rc"
fi
rm -f "$dry"
echo "@@ CANSWITCH $staged"
