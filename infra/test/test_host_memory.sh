#!/usr/bin/env bash
# What each host's EVALUATED config does when memory runs short -- zram, vm.swappiness and, on the
# k3s nodes, the kubelet's reserves and eviction threshold. See nix/modules/fleet/host-memory.nix.
#
# `nix eval` of the toplevel (bin/check) proves the module evaluates; it cannot notice that a host
# quietly has no swap, or that the kubelet config lost its memory threshold. Both are invisible until
# the OOM killer is the only thing left to answer, so this asks each host for the values themselves.
set -uo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
nix_flags=(--extra-experimental-features 'nix-command flakes')
. "$here/fleet-flake.sh"
flake="$(fleet_flake "$here/..")"

errors="$(mktemp)"
trap 'rm -f "$errors"' EXIT

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

# One evaluation per host, everything this test reads in one JSON line.
summary() {
  nix "${nix_flags[@]}" eval --json "$flake#nixosConfigurations.$1.config" --apply 'c: {
    zram = c.zramSwap.enable;
    algorithm = c.zramSwap.algorithm;
    percent = c.zramSwap.memoryPercent;
    swappiness = c.boot.kernel.sysctl."vm.swappiness" or null;
    k3s = c.services.k3s.enable;
    kubelet = c.services.k3s.extraKubeletConfig;
    execStart = c.systemd.services.k3s.serviceConfig.ExecStart or "";
  }' 2>"$errors"
}

# The value at a key path of a JSON document, printed bare (a string) or as JSON (anything else).
dig() {
  python3 -c '
import json, sys
value = json.loads(sys.argv[1])
for key in sys.argv[2:]:
    value = value.get(key) if isinstance(value, dict) else None
print(value if isinstance(value, str) else json.dumps(value))' "$@"
}
field() { dig "$@"; }
expected() { dig "$(cat "$here/../nix/files/host-memory.json")" "$@"; }

for host in mongo-1 monitoring-1 k3s-worker-1; do
  echo "$host"
  if ! out="$(summary "$host")" || ! python3 -c 'import json,sys; json.loads(sys.argv[1])' "$out" 2>/dev/null; then
    printf '  FAIL %s does not evaluate:\n%s\n' "$host" "$(tail -15 "$errors")"
    fail=$((fail + 1))
    continue
  fi
  check "$host zram enabled" true "$(field "$out" zram)"
  check "$host zram compresses with zstd" zstd "$(field "$out" algorithm)"
  check "$host zram size is host-memory.json's" "$(expected "$host" zramMemoryPercent)" "$(field "$out" percent)"
  check "$host vm.swappiness is host-memory.json's" "$(expected "$host" swappiness)" "$(field "$out" swappiness)"
  swappiness="$(field "$out" swappiness)"
  check "$host vm.swappiness is low (<= 10)" true "$([[ "$swappiness" =~ ^[0-9]+$ ]] && ((swappiness <= 10)) && echo true || echo false)"
  [ "$host" = mongo-1 ] && check "mongo-1 swaps only to avoid an OOM (MongoDB: swappiness 1)" 1 "$swappiness"

  [ "$(field "$out" k3s)" = true ] || continue
  for key in systemReservedMi:systemReserved kubeReservedMi:kubeReserved; do
    want="$(expected "$host" kubelet "${key%%:*}")"
    check "$host kubelet ${key#*:}.memory" "${want}Mi" "$(field "$out" kubelet "${key#*:}" memory)"
  done
  check "$host kubelet evicts at memory.available<$(expected "$host" kubelet evictionHardMi)Mi" \
    "$(expected "$host" kubelet evictionHardMi)Mi" "$(field "$out" kubelet evictionHard memory.available)"
  check "$host kubelet keeps k3s's nodefs eviction default" "5%" "$(field "$out" kubelet evictionHard nodefs.available)"
  check "$host kubelet keeps k3s's imagefs eviction default" "5%" "$(field "$out" kubelet evictionHard imagefs.available)"
  check "$host pods never swap" NoSwap "$(field "$out" kubelet memorySwap swapBehavior)"
  check "$host k3s is handed the kubelet config file" true \
    "$([[ "$(field "$out" execStart)" == *"--kubelet-arg=config="* ]] && echo true || echo false)"
done

echo "host memory: $pass passed, $fail failed"
((fail == 0))
