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

# RESOLVE ONLY WHAT IS THERE -- and the existence guard is the whole point of this function.
#
# `readlink -f` DOES NOT MEAN THE SAME THING IN THE TWO DIALECTS THIS REPOSITORY RUNS ON, and the
# difference is invisible until it is a production metric. GNU coreutils -- the fleet, and CI --
# canonicalises a path whose FINAL component does not exist and prints it anyway, exit 0. BSD, on a
# developer's macOS, fails and prints nothing. So an absent component resolved to "" on a laptop and
# to a full path on a NixOS host.
#
# That is not a cosmetic difference here, because the two paths being compared have DIFFERENT
# PARENTS: `/run/booted-system/kernel-modules` against `/nix/store/…-nixos-system-…/kernel-modules`.
# Absent from both sides, they came back as two unequal strings, this loop read that as a changed
# boot chain, and the host published `nixos_reboot_required 1` permanently -- the same inverted
# metric the header above describes, arrived at from the other direction.
#
# An absent component must therefore be resolved to the SAME value on both sides, whatever the local
# readlink does with it, so the comparison below is about the closures rather than about their
# paths. A dangling symlink is deliberately NOT folded in here: the link is a declaration that this
# closure HAS the component, and where it points is a real difference worth a reboot.
resolve_component() {
  local path="$1"
  [[ -e "$path" || -L "$path" ]] || return 0
  readlink -f "$path" 2>/dev/null || true
}

# THESE FOUR AND NOT THE CLOSURE PATH. A closure changes on any edit at all; only these can be
# brought into use exclusively by rebooting. `systemd` is in the list because a switch re-execs the
# daemon but cannot replace PID 1's own binary.
for component in kernel initrd kernel-modules systemd; do
  booted_component="$(resolve_component "$booted/$component")"
  candidate_component="$(resolve_component "$candidate/$component")"

  # A component absent from BOTH is not a difference -- some system closures legitimately lack one,
  # and calling that a reboot would flag every such host for ever.
  if [[ "$booted_component" != "$candidate_component" ]]; then
    echo 1
    exit 0
  fi
done

echo 0
