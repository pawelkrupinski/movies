#!/usr/bin/env bash
# Cases for nix/files/reboot-required.sh -- the decision behind `nixos_reboot_required`, and the
# only thing on this fleet that says a host must be rebooted rather than merely switched.
#
# IT IS TESTED HERE BECAUSE THE ONLY OTHER WAY TO EXERCISE IT IS TO REBOOT A MACHINE. The logic used
# to live inline in an activation script, where its boot-time behaviour could not be observed
# without booting -- and it was wrong there for as long as it existed: a missing
# `/run/booted-system`, which is the NORMAL state during a boot, made it report that a reboot was
# owed. Every host published that the moment it came up.
#
# ------------------------------------------------------------------------------------------------
# EVERY CASE RUNS TWICE, ONCE PER readlink DIALECT, AND THAT IS NOT BELT-AND-BRACES
# ------------------------------------------------------------------------------------------------
#
# The script's answer depends on what `readlink -f` does with a path that does not exist, and the
# two implementations disagree: GNU coreutils prints the canonicalised path anyway, BSD prints
# nothing. The fleet and CI are GNU; a developer's macOS is BSD. So the `both lack a component`
# case below passed on a laptop and FAILED in CI for a whole afternoon of red staging runs, on a
# bug that was real on the hosts and unreachable on the machine where the test was being run.
#
# A test that can only fail on the other platform is not a test of this script, so the cases run
# against the native `readlink` AND against a shim that imposes GNU semantics regardless of host.
# `os.path.realpath` IS GNU's rule -- canonicalise, tolerate a missing final component -- which is
# why the shim is three lines of Python rather than a dependency on coreutils being installed.
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

# THE GNU-SEMANTICS readlink, so a BSD host still exercises the behaviour the fleet gets. It
# implements only what this script asks of readlink: `-f`, one path, canonicalised.
mkdir -p "$root/gnu-shim"
cat > "$root/gnu-shim/readlink" <<'SHIM'
#!/usr/bin/env python3
import os, sys
args = [a for a in sys.argv[1:] if a != "-f"]
if len(args) != 1:
    sys.exit(2)
print(os.path.realpath(args[0]))
SHIM
chmod +x "$root/gnu-shim/readlink"

# `dialect` is set per pass and read by `decide`; the shim goes FIRST on PATH so it wins over
# /usr/bin/readlink, and the pass that wants the host's own readlink leaves PATH alone.
dialect=""
decide() {
  if [[ "$dialect" == "gnu" ]]; then
    PATH="$root/gnu-shim:$PATH" bash "$script" "$@"
  else
    bash "$script" "$@"
  fi
}

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

# Missing components on BOTH sides agree, and must not read as a difference.
mkdir -p "$root/bare-booted" "$root/bare-candidate"

cases() {
  check "an identical boot chain owes no reboot" \
    0 "$(decide "$root/booted" "$root/same")"

  check "a different kernel owes a reboot" \
    1 "$(decide "$root/booted" "$root/newkernel")"

  # THE CASE THIS FILE EXISTS FOR. An initrd difference is what a hostname change produces, and it
  # is a real reboot: activating cannot bring a new initrd into use.
  check "a different initrd owes a reboot" \
    1 "$(decide "$root/booted" "$root/newinitrd")"

  # THE REGRESSION. During a boot `/run/booted-system` does not resolve yet, and the closure being
  # activated IS the one that booted -- so the answer is 0. The inline version answered 1 here,
  # which left every freshly rebooted host claiming it owed another reboot until its next switch.
  check "an unresolvable booted path during boot owes no reboot" \
    0 "$(decide "$root/does-not-exist" "$root/same")"

  check "an empty booted path owes no reboot" \
    0 "$(decide "" "$root/same")"

  # THE SECOND REGRESSION, and the one only the `gnu` pass can see on a Mac. Both closures lack the
  # component, so they agree -- but the two absent paths canonicalise to different strings under
  # GNU readlink, and comparing THOSE said the boot chain had changed.
  check "closures that both lack a component agree" \
    0 "$(decide "$root/bare-booted" "$root/bare-candidate")"
}

echo "[spec] reboot-required -- native readlink"
dialect="native"
cases

echo "[spec] reboot-required -- GNU readlink semantics (what the fleet and CI run)"
dialect="gnu"
cases

echo
if ((fail)); then
  printf '[spec] %d passed, %d FAILED\n' "$pass" "$fail"
  exit 1
fi
printf '[spec] all %d cases pass\n' "$pass"
