#!/usr/bin/env bash
# Which recursive resolvers each host's EVALUATED config asks. See the "Networking" block of
# nix/modules/fleet/default.nix.
#
# WHY THIS EXISTS (2026-11-05). Hetzner's DHCP hands every server its own recursive resolvers,
# 185.12.64.1 and .2. From about 06:00 UTC that day both timed out on every zone served by
# cyberfolks.pl / hekko.net.pl -- systembiletowy.pl, the ticketing platform behind dozens of Polish
# community cinemas, among them -- while the zones' own nameservers and 1.1.1.1 / 9.9.9.9 / 8.8.8.8
# answered the same host at once. k3s hands CoreDNS /run/systemd/resolve/resolv.conf, so every pod
# inherited the failure: ~30 PL venues went red on /uptime as `ConnectException: null` (the JVM
# HttpClient's spelling of an unresolvable host) behind open circuit breakers.
#
# `nix eval` of the toplevel (bin/check) proves the module evaluates; it cannot notice that a host
# went back to asking whatever DHCP hands it. This asks each host for the values themselves.
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

# One evaluation per host: the stated nameservers and, for every wired networkd network (the
# wireless default NixOS also declares matches no interface a Hetzner server has), whether it still
# takes the DNS servers DHCP or router advertisements hand out.
summary() {
  nix "${nix_flags[@]}" eval --json "$flake#nixosConfigurations.$1.config" --apply 'c: {
    nameservers = c.networking.nameservers;
    takesDhcpDns = builtins.mapAttrs (name: n: {
      v4 = n.dhcpV4Config.UseDNS or true;
      v6 = n.dhcpV6Config.UseDNS or true;
      ra = n.ipv6AcceptRAConfig.UseDNS or true;
    }) (builtins.removeAttrs c.systemd.network.networks [ "99-wireless-client-dhcp" ]);
  }' 2>"$errors"
}

for host in mongo-1 monitoring-1 k3s-worker-1; do
  echo "$host"
  if ! out="$(summary "$host")" || ! python3 -c 'import json,sys; json.loads(sys.argv[1])' "$out" 2>/dev/null; then
    printf '  FAIL %s does not evaluate:\n%s\n' "$host" "$(tail -15 "$errors")"
    fail=$((fail + 1))
    continue
  fi
  check "$host states at least two nameservers of its own" true \
    "$(python3 -c 'import json,sys; print(str(len(json.loads(sys.argv[1])["nameservers"]) >= 2).lower())' "$out")"
  check "$host names none of Hetzner's resolvers (185.12.64.0/24)" "" \
    "$(python3 -c 'import json,sys; print(" ".join(s for s in json.loads(sys.argv[1])["nameservers"] if s.startswith("185.12.64.")))' "$out")"
  check "$host takes no DNS server from DHCP or router advertisements" "" \
    "$(python3 -c '
import json, sys
nets = json.loads(sys.argv[1])["takesDhcpDns"]
print(" ".join(f"{name}:{k}" for name, flags in sorted(nets.items()) for k, v in sorted(flags.items()) if v is not False))' "$out")"
done

echo "host dns: $pass passed, $fail failed"
((fail == 0))
