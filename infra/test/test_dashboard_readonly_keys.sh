#!/usr/bin/env bash
# monitoring-1's root accepts every operator Mac's dashboard read-only key, each under exactly the
# restrict + forced Prometheus-curl options and nothing looser.
#
# THE CASE THAT FORCED IT. 2026-11-16: the restored second Mac had no ~/.ssh/dashboard_readonly, so
# its fleet page read "Prometheus could not be read ... Permission denied (publickey)". Its own key
# was ADDED beside the first Mac's, which stays because that Mac is still in use. An add that
# dropped the old key blinds the other Mac's page; one with looser options hands a plain file key a
# root shell.
set -uo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
nix_flags=(--extra-experimental-features 'nix-command flakes')
. "$here/fleet-flake.sh"
flake="$(fleet_flake "$here/..")"

options="restrict,command=\"curl -sS --max-time 15 'http://10.20.0.11:9090/api/v1/query?query=%7B__name__%3D~%22nixos_.%2A%7Cnode_os_info%22%7D'\""
# One full entry per operator Mac. Both must stay.
expected=(
  "$options ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFPLz9aUrwhj93QLY1twe3Q3UURoxWftoDKVXqKMbBUS dashboards-readonly"
  "$options ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFNA6gtHzJuLLUIcrGirZWot47std7zKTH5gDZAAg3+i dashboards-readonly@pawel-mbp-2"
)

errors="$(mktemp)"
trap 'rm -f "$errors"' EXIT

if ! keys="$(nix "${nix_flags[@]}" eval --raw \
    "$flake#nixosConfigurations.monitoring-1.config.users.users.root.openssh.authorizedKeys.keys" \
    --apply 'ks: builtins.concatStringsSep "\n" ks' 2>"$errors")"; then
  printf '  FAIL monitoring-1: root keys do not evaluate: %s\n' "$(tail -5 "$errors")"
  exit 2
fi

pass=0
fail=0
for entry in "${expected[@]}"; do
  name="${entry##* }"
  if grep -qxF -- "$entry" <<< "$keys"; then
    printf '  ok   monitoring-1: root accepts %s under the forced read\n' "$name"
    pass=$((pass + 1))
  else
    printf '  FAIL monitoring-1: root does not accept %s under exactly the forced read\n' "$name"
    fail=$((fail + 1))
  fi
done
# Any other dashboards-readonly entry (a third Mac, a loosened copy) must be added here on purpose.
others="$(grep -F 'dashboards-readonly' <<< "$keys" | grep -vxF -f <(printf '%s\n' "${expected[@]}") || true)"
if [ -n "$others" ]; then
  printf '  FAIL monitoring-1: unexpected dashboards-readonly entry: %s\n' "$others"
  fail=$((fail + 1))
fi

echo "$pass passed, $fail failed"
[ "$fail" -eq 0 ]
