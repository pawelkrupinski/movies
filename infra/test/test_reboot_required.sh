#!/usr/bin/env bash
# Cases for nix/files/reboot-required.sh -- the decision behind `nixos_reboot_required`, and the
# only thing on this fleet that says a host must be rebooted rather than merely switched.
#
# IT IS TESTED HERE BECAUSE THE ONLY OTHER WAY TO EXERCISE IT IS TO REBOOT A MACHINE. The logic used
# to live inline in an activation script, where its boot-time behaviour could not be observed
# without booting -- and it was wrong there for as long as it existed: a missing
# `/run/booted-system`, which is the NORMAL state during a boot, made it report that a reboot was
# owed. Every host published that the moment it came up.
set -uo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
script="$here/../nix/files/reboot-required.sh"

pass=0
fail=0

check() {
  local name="$1" expected="$2" actual="$3"
  if [[ "$expected" == "$actual" ]]; then
    printf '  ok   %s\n' "$name"
    pass=$((pass + 1))
  else
    printf '  FAIL %s -- expected %s, got %s\n' "$name" "$expected" "$actual"
    fail=$((fail + 1))
  fi
}

root="$(mktemp -d)"
trap 'rm -rf "$root"' EXIT

# A system closure is a directory whose kernel/initrd/etc are symlinks into the store. The test
# builds miniature ones: what matters is only whether the links resolve to the same targets.
make_system() {
  local dir="$1" kernel="$2" initrd="$3"
  mkdir -p "$dir"
  ln -sfn "$root/store/$kernel" "$dir/kernel"
  ln -sfn "$root/store/$initrd" "$dir/initrd"
  ln -sfn "$root/store/modules-a" "$dir/kernel-modules"
  ln -sfn "$root/store/systemd-a" "$dir/systemd"
}

mkdir -p "$root/store"
touch "$root/store/kernel-a" "$root/store/kernel-b" \
      "$root/store/initrd-a" "$root/store/initrd-b" \
      "$root/store/modules-a" "$root/store/systemd-a"

make_system "$root/booted" kernel-a initrd-a
make_system "$root/same"   kernel-a initrd-a
make_system "$root/newkernel" kernel-b initrd-a
make_system "$root/newinitrd" kernel-a initrd-b

echo "[spec] reboot-required"

check "an identical boot chain owes no reboot" \
  0 "$(bash "$script" "$root/booted" "$root/same")"

check "a different kernel owes a reboot" \
  1 "$(bash "$script" "$root/booted" "$root/newkernel")"

# THE CASE THIS FILE EXISTS FOR. An initrd difference is what a hostname change produces, and it is
# a real reboot: activating cannot bring a new initrd into use.
check "a different initrd owes a reboot" \
  1 "$(bash "$script" "$root/booted" "$root/newinitrd")"

# THE REGRESSION. During a boot `/run/booted-system` does not resolve yet, and the closure being
# activated IS the one that booted -- so the answer is 0. The inline version answered 1 here, which
# left every freshly rebooted host claiming it owed another reboot until its next switch.
check "an unresolvable booted path during boot owes no reboot" \
  0 "$(bash "$script" "$root/does-not-exist" "$root/same")"

check "an empty booted path owes no reboot" \
  0 "$(bash "$script" "" "$root/same")"

# Missing components on BOTH sides agree, and must not read as a difference.
mkdir -p "$root/bare-booted" "$root/bare-candidate"
check "closures that both lack a component agree" \
  0 "$(bash "$script" "$root/bare-booted" "$root/bare-candidate")"

echo
if ((fail)); then
  printf '[spec] %d passed, %d FAILED\n' "$pass" "$fail"
  exit 1
fi
printf '[spec] all %d cases pass\n' "$pass"
