#!/usr/bin/env bash
# Give every user on a GitHub-hosted Linux runner /dev/kvm, so the x86_64 Android emulator runs hardware-accelerated:
# without KVM it falls back to software emulation and does not boot inside the jobs' timeouts. Run by android.yml's
# and order-independence.yml's emulator jobs.
#
# A job container that already has a usable /dev/kvm has no udevd to reload, so `udevadm control` would fail there: nothing to do, say so and stop.
set -euo pipefail
KVM_DEVICE="${KVM_DEVICE:-/dev/kvm}"
if [ -r "$KVM_DEVICE" ] && [ -w "$KVM_DEVICE" ]; then
    echo "$KVM_DEVICE is already usable by $(id -un); no udev rule needed."
    exit 0
fi
echo 'KERNEL=="kvm", GROUP="kvm", MODE="0666", OPTIONS+="static_node=kvm"' | sudo tee /etc/udev/rules.d/99-kvm4all.rules
sudo udevadm control --reload-rules
sudo udevadm trigger --name-match=kvm
