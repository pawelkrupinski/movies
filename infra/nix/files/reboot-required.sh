#!/usr/bin/env bash
# Decide whether a host owes a REBOOT: does the boot chain of the closure it is running differ from
# the boot chain of the closure it actually booted?
#
#   reboot-required.sh <booted-system-path> <candidate-system-path>   # prints 0 or 1
#
# A SEPARATE FILE, NOT INLINE IN THE ACTIVATION SCRIPT, so it can be run against directories a test
# controls. It used to be a shell fragment embedded in modules/fleet/observability.nix, where the
# only way to exercise it was to reboot a machine and read a Prometheus metric afterwards -- which
# is how the bug below survived.
#
# ------------------------------------------------------------------------------------------------
# WHY AN UNRESOLVABLE BOOTED PATH MEANS 0 AND NOT 1
# ------------------------------------------------------------------------------------------------
#
# The embedded version treated a missing `/run/booted-system` as "differs", reasoning that not being
# able to read one side is not evidence the two match and the safe direction is to ask for a reboot.
# That is good instinct and the wrong conclusion, because of WHEN the metric is written.
#
# It is written by an activation script, and activation runs on BOOT as well as on switch. Early in
# a boot `/run/booted-system` does not resolve yet -- so every component compared as
# `missing-booted != candidate`, and the host published `nixos_reboot_required 1` from the moment it
# came up. The value then stayed 1 until the next `switch`, because nothing else rewrites the file.
#
# The result was exactly inverted: a machine that had JUST REBOOTED -- the one host that certainly
# owes nothing -- was the one the dashboard flagged. mongo-1 sat in `reboot owed` immediately after
# a clean reboot on 2026-09-02, with its own `nixos_booted_closure_info` carrying the giveaway label
# `closure="booted-system"`, the unresolved literal.
#
# At boot-time activation the closure being activated IS the closure that booted -- GRUB chose it
# and stage-2 handed it here -- so the honest answer when the booted path cannot be read is 0. A
# switch, where the distinction actually matters, always has `/run/booted-system` available.
set -uo pipefail

booted="${1:-}"
candidate="${2:-}"

if [[ -z "$candidate" ]]; then
  echo "reboot-required.sh: needs <booted-system-path> <candidate-system-path>" >&2
  exit 2
fi

# Not resolvable means "we are the booted system" -- see the note above.
if [[ -z "$booted" ]] || [[ ! -e "$booted" ]]; then
  echo 0
  exit 0
fi

# THESE FOUR AND NOT THE CLOSURE PATH. A closure changes on any edit at all; only these can be
# brought into use exclusively by rebooting. `systemd` is in the list because a switch re-execs the
# daemon but cannot replace PID 1's own binary.
for component in kernel initrd kernel-modules systemd; do
  booted_component="$(readlink -f "$booted/$component" 2>/dev/null || true)"
  candidate_component="$(readlink -f "$candidate/$component" 2>/dev/null || true)"

  # A component absent from BOTH is not a difference -- some system closures legitimately lack one,
  # and calling that a reboot would flag every such host for ever.
  if [[ "$booted_component" != "$candidate_component" ]]; then
    echo 1
    exit 0
  fi
done

echo 0
