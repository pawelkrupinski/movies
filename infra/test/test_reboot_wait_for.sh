#!/usr/bin/env bash
# What each host's EVALUATED config tells a reboot to wait for: `fleet.reboot.waitFor`, read by
# fleetkit's rolling-reboot (adapters.InFlightWork). See the option in nix/modules/fleet/default.nix.
#
# WHY THIS EXISTS (2026-11-10). A rolling reboot kexec'd mongo-1 at 03:27:36, 7 s after the nightly
# mongodump (started 03:25:47) finished. A dump cut off mid-run is lost, and its timer's
# `Persistent` does not run it again. The tool waits for what this list names, so the list is the
# guard: mongo-1 must name mongodump.service, and a name that is not a real, non-RemainAfterExit
# service must fail evaluation (an unknown unit reads `inactive`, so waiting for it waits for nothing).
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

# eval_host <host> <apply> -- the JSON of <apply> over the host's nixosConfiguration, or nothing.
eval_host() {
  nix "${nix_flags[@]}" eval --json "$flake#nixosConfigurations.$1" --apply "$2" 2>"$errors"
}

# The list, and the messages of this option's own failed assertions (none on a real host).
summary='c: {
  waitFor = c.config.fleet.reboot.waitFor;
  failed = map (a: a.message) (builtins.filter (a: !a.assertion && builtins.match ".*fleet.reboot.waitFor.*" a.message != null) c.config.assertions);
}'

for host in mongo-1 monitoring-1 k3s-worker-1; do
  echo "$host"
  if ! out="$(eval_host "$host" "$summary")" || [ -z "$out" ]; then
    printf '  FAIL %s does not evaluate:\n%s\n' "$host" "$(tail -15 "$errors")"
    fail=$((fail + 1))
    continue
  fi
  case "$host" in
    mongo-1) want='["mongodump.service"]' ;;
    *) want='[]' ;;
  esac
  check "$host's reboot waits for $want" "$want" \
    "$(python3 -c 'import json,sys; print(json.dumps(json.loads(sys.argv[1])["waitFor"], separators=(",", ":")))' "$out")"
  check "$host's list passes its own assertion" "0" \
    "$(python3 -c 'import json,sys; print(len(json.loads(sys.argv[1])["failed"]))' "$out")"
done

# THE ASSERTION CAN FAIL: a unit mongo-1 does not define, and a RemainAfterExit oneshot the test
# adds, must each be refused by name -- while mongodump.service, still on the list, is not.
echo "mongo-1, with a bad name added"
extended='c: let
  e = c.extendModules { modules = [ ({ lib, ... }: {
    fleet.reboot.waitFor = [ "no-such-unit.service" "reboot-wait-for-probe.service" ];
    systemd.services.reboot-wait-for-probe.serviceConfig = { Type = "oneshot"; RemainAfterExit = true; ExecStart = "/bin/true"; };
  }) ]; };
in map (a: a.message) (builtins.filter (a: !a.assertion) e.config.assertions)'
if ! out="$(eval_host mongo-1 "$extended")" || [ -z "$out" ]; then
  printf '  FAIL the extended mongo-1 does not evaluate:\n%s\n' "$(tail -15 "$errors")"
  fail=$((fail + 1))
else
  for unit in no-such-unit.service reboot-wait-for-probe.service; do
    check "an assertion refuses $unit" "1" \
      "$(python3 -c 'import json,sys; print(sum(1 for m in json.loads(sys.argv[1]) if "fleet.reboot.waitFor names " + sys.argv[2] in m))' "$out" "$unit")"
  done
  check "...and mongodump.service is not refused" "0" \
    "$(python3 -c 'import json,sys; print(sum(1 for m in json.loads(sys.argv[1]) if "names mongodump.service" in m))' "$out")"
fi

echo "$pass passed, $fail failed"
[ "$fail" -eq 0 ]
