#!/usr/bin/env bash
# Every account that can ssh into a host LINGERS in that host's evaluated config, and the switch's
# logind race (nix/modules/fleet/accounts.nix) cannot happen for it.
#
# THE CASE THAT FORCED IT. monitoring-1, 2026-11-10 08:29Z: the dashboard's root ssh poll every
# 15 s started user@0 and logind removed it ~10 s later; switch-to-configuration-ng listed root and
# then failed to read its GID ("Unknown object '/org/freedesktop/login1/user/_0'"), aborting the
# switch midway. An account that lingers never leaves logind's list. An account any role adds with
# a key -- and that does not linger -- is the next host that fails a switch the same way.
set -uo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
nix_flags=(--extra-experimental-features 'nix-command flakes')
. "$here/fleet-flake.sh"
flake="$(fleet_flake "$here/..")"

errors="$(mktemp)"
trap 'rm -f "$errors"' EXIT

pass=0
fail=0
for host in $(find "$here/../nix/hosts" -mindepth 1 -maxdepth 1 -type d -exec basename {} \; | sort); do
  # "name=linger" for each account carrying an authorized key, one per line.
  if ! accounts="$(nix "${nix_flags[@]}" eval --raw "$flake#nixosConfigurations.$host.config.users.users" \
      --apply 'users: builtins.concatStringsSep "\n" (builtins.filter (line: line != "")
        (builtins.attrValues (builtins.mapAttrs (name: u:
          if u.openssh.authorizedKeys.keys != [ ] || u.openssh.authorizedKeys.keyFiles != [ ]
          then "${name}=${builtins.toJSON u.linger}" else "") users)))' 2>"$errors")"; then
    printf '  FAIL %s does not evaluate: %s\n' "$host" "$(tail -5 "$errors")"
    fail=$((fail + 1))
    continue
  fi
  case "$accounts" in *root=*) ;; *)
    printf '  FAIL %s: root has no key, so this test reads nothing\n' "$host"; fail=$((fail + 1)) ;;
  esac
  while IFS= read -r line; do
    [ -n "$line" ] || continue
    if [ "${line#*=}" = true ]; then
      printf '  ok   %s: %s lingers\n' "$host" "${line%%=*}"
      pass=$((pass + 1))
    else
      printf '  FAIL %s: %s can ssh in and does not linger (linger = %s)\n' "$host" "${line%%=*}" "${line#*=}"
      fail=$((fail + 1))
    fi
  done <<< "$accounts"
done

echo "$pass passed, $fail failed"
[ "$fail" -eq 0 ]
