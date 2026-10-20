#!/usr/bin/env bash
# Self-test for enable-kvm.sh's early exit: an already-usable device needs no sudo and no udev.
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

# A usable device: exits 0 without reaching sudo (a failing sudo on PATH proves it is never called).
mkdir "$tmp/bin"
printf '#!/bin/sh\necho "sudo called" >&2\nexit 99\n' > "$tmp/bin/sudo"
chmod +x "$tmp/bin/sudo"
touch "$tmp/kvm"
out="$(PATH="$tmp/bin:$PATH" KVM_DEVICE="$tmp/kvm" "$here/enable-kvm.sh")"
case "$out" in
    *"already usable"*) ;;
    *) echo "FAIL: usable device did not short-circuit: $out" >&2; exit 1 ;;
esac

# A missing device: goes on to the udev rule (and so to sudo).
if PATH="$tmp/bin:$PATH" KVM_DEVICE="$tmp/missing" "$here/enable-kvm.sh" >/dev/null 2>&1; then
    echo "FAIL: missing device skipped the udev rule" >&2; exit 1
fi
echo "enable-kvm-test: ok"
