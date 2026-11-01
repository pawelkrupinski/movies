#!/usr/bin/env bash
# Which machine this job ran on, as one `machine: {json}` line in the log and one {"kind":"machine",...} line in the
# job's metrics file ($CI_METRICS_FILE, default $RUNNER_TEMP/ci-metrics/metrics.jsonl), which the job's telemetry
# step uploads as its ci-metrics artifact (.github/actions/job-telemetry, scripts/ci/ci_trends.py). It lets
# `ci-timings report --by cpu-model` tell a slow run from slow hardware.
#
#   machine-id.sh            # print the line and append it to the metrics file
#
# NEVER FAILS: every probe falls back to an empty field, and the script exits 0 whatever happens. The one network
# probe (Azure's instance metadata, for the region) has a 1 s ceiling.
#
# PROC_ROOT, SYS_ROOT and MACHINE_ID_NO_NETWORK=1 stand in for /proc, /sys and the metadata call, for a test.
set -u

proc="${PROC_ROOT:-/proc}"
sys="${SYS_ROOT:-/sys}"
out="${CI_METRICS_FILE:-${RUNNER_TEMP:-/tmp}/ci-metrics/metrics.jsonl}"

# A JSON string: backslash and quote escaped, control characters dropped.
js() { local s="${1-}"; s="${s//\\/\\\\}"; s="${s//\"/\\\"}"; printf '"%s"' "$(printf '%s' "$s" | tr -d '\000-\037')"; }
# A JSON number, or null.
jn() { case "${1-}" in ''|*[!0-9.]*) printf 'null' ;; *) printf '%s' "$1" ;; esac; }
first() { head -n 1 "$1" 2>/dev/null | tr -d '\n'; }

main() {
  local cpu_model cores flags_hash mem_kb kernel arch cg cpu_max mem_max disk_kb free_kb virt product vendor region
  cpu_model=$(awk -F': *' '/^model name/ { print $2; exit }' "$proc/cpuinfo" 2>/dev/null)
  cores=$(grep -c '^processor' "$proc/cpuinfo" 2>/dev/null)
  flags_hash=$(awk -F': *' '/^flags/ { print $2; exit }' "$proc/cpuinfo" 2>/dev/null)
  [ -n "$flags_hash" ] && flags_hash=$(printf '%s' "$flags_hash" | { sha256sum 2>/dev/null || shasum -a 256; } | cut -c1-12)
  mem_kb=$(awk '/^MemTotal:/ { print $2 }' "$proc/meminfo" 2>/dev/null)
  kernel=$(first "$proc/sys/kernel/osrelease"); [ -n "$kernel" ] || kernel=$(uname -r 2>/dev/null)
  arch=$(uname -m 2>/dev/null)
  # This process's own cgroup (v2: one `0::<path>` line); its limits, `max` when there is none.
  cg=$(awk -F'::' 'NF > 1 { print $2; exit }' "$proc/self/cgroup" 2>/dev/null)
  cpu_max=$(first "$sys/fs/cgroup${cg}/cpu.max"); [ -n "$cpu_max" ] || cpu_max=$(first "$sys/fs/cgroup/cpu.max")
  mem_max=$(first "$sys/fs/cgroup${cg}/memory.max"); [ -n "$mem_max" ] || mem_max=$(first "$sys/fs/cgroup/memory.max")
  read -r disk_kb free_kb < <(df -kP "${GITHUB_WORKSPACE:-.}" 2>/dev/null | awk 'NR == 2 { print $2, $4 }')
  virt=$(systemd-detect-virt 2>/dev/null)
  product=$(first "$sys/class/dmi/id/product_name")
  vendor=$(first "$sys/class/dmi/id/sys_vendor")
  region="${RUNNER_REGION:-}"
  if [ -z "$region" ] && [ "${MACHINE_ID_NO_NETWORK:-0}" != 1 ] && [ "$vendor" = "Microsoft Corporation" ]; then
    # allow-silenced: the region is optional context; no answer within 1 s leaves it empty, which is its meaning.
    region=$(curl -s --max-time 1 -H Metadata:true \
      'http://169.254.169.254/metadata/instance/compute/location?api-version=2021-02-01&format=text' 2>/dev/null | head -c 40)
  fi
  local line
  line=$(printf '{"kind":"machine","runner_name":%s,"runner_os":%s,"hosted":%s,"arch":%s,"image":%s,"image_version":%s,"cpu_model":%s,"cores":%s,"cpu_flags_hash":%s,"mem_mb":%s,"disk_gb":%s,"disk_free_gb":%s,"kernel":%s,"cgroup_cpu_max":%s,"cgroup_mem_max":%s,"virt":%s,"product":%s,"vendor":%s,"region":%s}' \
    "$(js "${RUNNER_NAME:-}")" "$(js "${RUNNER_OS:-}")" "$(js "${RUNNER_ENVIRONMENT:-}")" "$(js "${RUNNER_ARCH:-$arch}")" \
    "$(js "${ImageOS:-}")" "$(js "${ImageVersion:-}")" "$(js "$cpu_model")" "$(jn "$cores")" "$(js "$flags_hash")" \
    "$(jn "$(( ${mem_kb:-0} / 1024 ))")" "$(jn "$(( ${disk_kb:-0} / 1048576 ))")" "$(jn "$(( ${free_kb:-0} / 1048576 ))")" \
    "$(js "$kernel")" "$(js "$cpu_max")" "$(js "$mem_max")" "$(js "$virt")" "$(js "$product")" "$(js "$vendor")" \
    "$(js "$region")")
  echo "machine: $line"
  mkdir -p "$(dirname "$out")" 2>/dev/null && echo "$line" >> "$out"
}

main 2>/dev/null || echo "machine: {\"kind\":\"machine\",\"error\":\"probe failed\"}"
exit 0
