#!/usr/bin/env bash
# Cases for nix/lib/oneshot-start-bound.nix -- how long systemd lets a oneshot's start run, which
# fleet/auto-apply.nix's assertion weighs: a FORGIVEN oneshot that may run past 30 min must set
# `restartIfChanged = false`, or a switch that changes it while it runs waits on its whole run.
#
# A ONESHOT THAT DECLARES NO TIMEOUT IS UNBOUNDED, NOT SHORT. systemd applies
# DefaultTimeoutStartSec to every type but oneshot ("timeouts are disabled by default" for it,
# systemd.service(5)), so a oneshot with neither TimeoutStartSec nor TimeoutSec may hold its start
# job for ever. The assertion first read a missing timeout as 0 and passed exactly those units
# (registry-prune on k3s-worker-1, logrotate on mongo-1) -- the riskiest case, judged safest.
set -uo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
lib="$here/../nix/lib/oneshot-start-bound.nix"
nix="$(command -v nix || echo /nix/var/nix/profiles/default/bin/nix)"

pass=0
fail=0

# `bound <serviceConfig as a Nix attrset>` prints the seconds, or null.
bound() {
  "$nix" --extra-experimental-features nix-command eval --impure --json --expr "import $lib ($1)" 2>&1
}

check() {
  local name="$1" expected="$2" config="$3" actual
  actual="$(bound "$config")"
  if [[ "$expected" == "$actual" ]]; then
    printf '  ok   %s\n' "$name"
    pass=$((pass + 1))
  else
    printf '  FAIL %s -- expected %s, got %s\n' "$name" "$expected" "$actual"
    fail=$((fail + 1))
  fi
}

printf '\033[36m▸\033[0m oneshot-start-bound.nix\n'
check "no timeout declared: unbounded"            null '{ Type = "oneshot"; }'
check "TimeoutStartSec in minutes"                600  '{ TimeoutStartSec = "10min"; }'
check "TimeoutStartSec as a bare number"          90   '{ TimeoutStartSec = 90; }'
check "TimeoutStartSec in hours"                  3600 '{ TimeoutStartSec = "1h"; }'
check "infinity: unbounded"                       null '{ TimeoutStartSec = "infinity"; }'
check "0 disables the timeout: unbounded"         null '{ TimeoutStartSec = "0"; }'
check "a compound span this does not read: null"  null '{ TimeoutStartSec = "1h 30min"; }'
check "TimeoutSec sets the start timeout too"     1200 '{ TimeoutSec = "20min"; }'
check "TimeoutStartSec wins over TimeoutSec"      300  '{ TimeoutSec = "20min"; TimeoutStartSec = "5min"; }'
check "a merged list: the last definition wins"   2700 '{ TimeoutStartSec = [ "5min" "45min" ]; }'

printf '%d passed, %d failed\n' "$pass" "$fail"
[[ "$fail" -eq 0 ]]
