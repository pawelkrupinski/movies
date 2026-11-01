#!/usr/bin/env bash
# machine-id.sh: one `machine:` line and one metrics line, from a fixed /proc and /sys, and exit 0 when it can read
# nothing at all.
# Run: bash scripts/ci/machine-id-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m machine-id.sh\n'
script="$REPO_ROOT/scripts/ci/machine-id.sh"
dir="$(mktemp -d)"; trap 'rm -rf "$dir"' EXIT
proc="$dir/proc" sys="$dir/sys"
mkdir -p "$proc/self" "$proc/sys/kernel" "$sys/fs/cgroup/job" "$sys/class/dmi/id"
printf 'processor\t: 0\nmodel name\t: AMD EPYC 7763 64-Core Processor\nflags\t\t: fpu sse avx2\nprocessor\t: 1\nmodel name\t: AMD EPYC 7763 64-Core Processor\n' > "$proc/cpuinfo"
printf 'MemTotal:       16374100 kB\n' > "$proc/meminfo"
echo '6.8.0-1017-azure' > "$proc/sys/kernel/osrelease"
echo '0::/job' > "$proc/self/cgroup"
echo 'max 100000' > "$sys/fs/cgroup/job/cpu.max"
echo '17179869184' > "$sys/fs/cgroup/job/memory.max"
echo 'Virtual Machine' > "$sys/class/dmi/id/product_name"
echo 'Microsoft Corporation' > "$sys/class/dmi/id/sys_vendor"

out="$(PROC_ROOT="$proc" SYS_ROOT="$sys" MACHINE_ID_NO_NETWORK=1 CI_METRICS_FILE="$dir/m.jsonl" RUNNER_NAME='GitHub "Actions" 7' \
  ImageOS=ubuntu24 ImageVersion=20251020.1 RUNNER_ENVIRONMENT=github-hosted "$script")"
field() { python3 -c 'import json,sys; print(json.loads(sys.stdin.read())[sys.argv[1]])' "$1" < "$dir/m.jsonl"; }

check "one machine: line" "1" "$(grep -c '^machine: {' <<< "$out")"
check "the CPU model and its core count" "AMD EPYC 7763 64-Core Processor 2" "$(field cpu_model) $(field cores)"
check "the image" "ubuntu24 20251020.1" "$(field image) $(field image_version)"
check "a runner name with quotes stays valid JSON" 'GitHub "Actions" 7' "$(field runner_name)"
check "RAM in MB" "15990" "$(field mem_mb)"
check "the job cgroup's limits" "max 100000 17179869184" "$(field cgroup_cpu_max) $(field cgroup_mem_max)"
check "the kernel and the hypervisor's product" "6.8.0-1017-azure Virtual Machine" "$(field kernel) $(field product)"
check "the CPU flags as a short hash" "12" "$(field cpu_flags_hash | tr -d '\n' | wc -c | tr -d ' ')"
check "nothing readable still exits 0" "0" \
  "$(PROC_ROOT=/nonexistent SYS_ROOT=/nonexistent MACHINE_ID_NO_NETWORK=1 CI_METRICS_FILE="$dir/n.jsonl" "$script" > /dev/null; echo $?)"

spec_summary
