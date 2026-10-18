#!/usr/bin/env bash
# Give every user on a GitHub-hosted Linux runner /dev/kvm, so the x86_64 Android emulator runs hardware-accelerated:
# without KVM it falls back to software emulation and does not boot inside the jobs' timeouts. Run by android.yml's
# and order-independence.yml's emulator jobs.
set -euo pipefail
echo 'KERNEL=="kvm", GROUP="kvm", MODE="0666", OPTIONS+="static_node=kvm"' | sudo tee /etc/udev/rules.d/99-kvm4all.rules
sudo udevadm control --reload-rules
sudo udevadm trigger --name-match=kvm
