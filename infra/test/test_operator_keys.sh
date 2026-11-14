#!/usr/bin/env bash
# Every operator key in nix/modules/fleet/accounts.nix reaches root AND pawel on every host's
# evaluated config.
#
# THE CASE THAT FORCED IT. 2026-11-14: Pawel's second Mac (restored, its own Secure Enclave key
# pawel-Pawes-MacBook-Pro-2-se) was ADDED beside pawel-mbp-se, which stays because that Mac is still
# in use. An add that silently dropped the old key -- or reached one account and not the other --
# locks a Mac out of the fleet, and the deploy that would fix it is applied over that same key.
set -uo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
nix_flags=(--extra-experimental-features 'nix-command flakes')
. "$here/fleet-flake.sh"
flake="$(fleet_flake "$here/..")"

# comment=key-blob prefix, one per operator Mac. Both must stay.
expected=(
  "pawel-mbp-se=AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBO3WMjnDl0nbOhu9fggw2z"
  "pawel-Pawes-MacBook-Pro-2-se=AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBCThjT17hrCBvxIV05Lqr66"
)

errors="$(mktemp)"
trap 'rm -f "$errors"' EXIT

pass=0
fail=0
for host in $(find "$here/../nix/hosts" -mindepth 1 -maxdepth 1 -type d -exec basename {} \; | sort); do
  for user in root pawel; do
    if ! keys="$(nix "${nix_flags[@]}" eval --raw \
        "$flake#nixosConfigurations.$host.config.users.users.$user.openssh.authorizedKeys.keys" \
        --apply 'ks: builtins.concatStringsSep "\n" ks' 2>"$errors")"; then
      printf '  FAIL %s: %s does not evaluate: %s\n' "$host" "$user" "$(tail -5 "$errors")"
      fail=$((fail + 1))
      continue
    fi
    for entry in "${expected[@]}"; do
      name="${entry%%=*}" blob="${entry#*=}"
      if grep -q -- "$blob.* $name\$" <<< "$keys"; then
        printf '  ok   %s: %s accepts %s\n' "$host" "$user" "$name"
        pass=$((pass + 1))
      else
        printf '  FAIL %s: %s does not accept %s\n' "$host" "$user" "$name"
        fail=$((fail + 1))
      fi
    done
  done
done

echo "$pass passed, $fail failed"
[ "$fail" -eq 0 ]
