#!/usr/bin/env bash
# A RULE EDIT RELOADS PROMETHEUS, A SCRAPE OR FLAG EDIT RESTARTS IT, AND A REFUSED RELOAD FAILS THE
# SWITCH (roles/prometheus.nix, the owner's call of 2026-11-18). Two halves:
#
#   1. monitoring-1's evaluated unit: every rule file is a `reloadTriggers` entry and none is a
#      `restartTriggers` one, prometheus.yaml and the scrape files still restart it, ExecReload is
#      files/prometheus-reload.sh, and the applier forgives the reload.
#   2. files/prometheus-reload.sh against a stand-in Prometheus -- a process that answers SIGHUP by
#      rewriting a metrics file (read over file://) the way the real one moves its reload metrics:
#      a reload taken exits 0; promtool refusing, Prometheus refusing, or Prometheus never answering
#      exits non-zero, and a promtool refusal sends no signal at all.
set -uo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=SCRIPTDIR/fleet-flake.sh
. "$here/fleet-flake.sh"
flake="$(fleet_flake "$here/..")"
reload="$here/../nix/files/prometheus-reload.sh"
nix="$(command -v nix || echo /nix/var/nix/profiles/default/bin/nix)"

pass=0
fail=0
ok() { printf '  ok   %s\n' "$1"; pass=$((pass + 1)); }
no() { printf '  FAIL %s\n' "$1"; fail=$((fail + 1)); }

printf '\033[36m▸\033[0m monitoring-1: which edits reload prometheus.service and which restart it\n'
errors="$(mktemp)"
# shellcheck disable=SC2016 # a Nix expression: its ${...} are Nix's, not the shell's
unit="$("$nix" --extra-experimental-features 'nix-command flakes' eval --json \
  "$flake#nixosConfigurations.monitoring-1.config" --apply 'c: let
      s = c.systemd.services.prometheus;
      rules = builtins.filter (n: builtins.match "prometheus/rules/.*" n != null) (builtins.attrNames c.environment.etc);
      src = n: toString c.environment.etc.${n}.source;
      restart = map toString s.restartTriggers;
      reload = map toString s.reloadTriggers;
    in {
      ruleCount = builtins.length rules;
      rulesReload = builtins.all (n: builtins.elem (src n) reload) rules;
      rulesRestart = builtins.filter (n: builtins.elem (src n) restart) rules;
      reloadOnlyRules = builtins.length reload == builtins.length rules;
      configRestarts = builtins.elem (src "prometheus/prometheus.yaml") restart;
      scrapeRestarts = builtins.elem (src "prometheus/scrape.d/kinowo-apps.yaml") restart;
      execReloadShape = builtins.match ".*/bin/prometheus-reload .* [$]MAINPID .*" (s.serviceConfig.ExecReload or "") != null;
      reloadable = builtins.elem "prometheus.service" c.fleet.autoApply.reloadableUnits;
    }' 2>"$errors")"
# expect <field> <JSON value> <what it means>
expect() {
  local actual
  actual="$(field "$1")"
  if [[ "$actual" == "$2" ]]; then ok "$3"; else no "$3 -- $1 is $actual"; fi
}
field() { python3 -c 'import json,sys; print(json.dumps(json.loads(sys.argv[1])[sys.argv[2]]))' "$unit" "$1" 2>/dev/null; }
if [[ "$(field ruleCount)" =~ ^[1-9][0-9]*$ ]]; then
  ok "$(field ruleCount) rule files installed"
  expect rulesReload     true "every rule file is a reloadTrigger"
  expect rulesRestart    '[]' "no rule file is a restartTrigger"
  expect reloadOnlyRules true "reloadTriggers holds the rule files and nothing else"
  expect configRestarts  true "prometheus.yaml still restarts it"
  expect scrapeRestarts  true "a scrape file still restarts it"
  expect execReloadShape true "ExecReload is prometheus-reload, signalling \$MAINPID"
  expect reloadable      true "the applier forgives the reload (fleet.autoApply.reloadableUnits)"
else
  no "monitoring-1 does not evaluate:"
  tail -15 "$errors"
fi

printf '\033[36m▸\033[0m prometheus-reload.sh against a stand-in Prometheus\n'
work="$(mktemp -d)"
trap 'kill "${fake:-}" 2>/dev/null; rm -rf "$work" "$errors"' EXIT

# metrics <successful> <timestamp>
metrics() {
  printf '# TYPE prometheus_config_last_reload_successful gauge\nprometheus_config_last_reload_successful %s\nprometheus_config_last_reload_success_timestamp_seconds %s\n' \
    "$1" "$2" > "$work/metrics.tmp" && mv "$work/metrics.tmp" "$work/metrics"
}

# The stand-in: on SIGHUP, `takes` writes a new success stamp, `refuses` drops successful to 0,
# `ignores` changes nothing. It touches `hup` so a case can tell whether a signal was sent at all.
start_fake() {
  rm -f "$work/hup"
  metrics 1 1.7950246708385227e+09
  bash -c '
    on_hup() {
      touch "$1/hup"
      case "$2" in
        takes)   printf "prometheus_config_last_reload_successful 1\nprometheus_config_last_reload_success_timestamp_seconds 1.7950299e+09\n" > "$1/m" ;;
        refuses) printf "prometheus_config_last_reload_successful 0\nprometheus_config_last_reload_success_timestamp_seconds 1.7950246708385227e+09\n" > "$1/m" ;;
        ignores) return ;;
      esac
      mv "$1/m" "$1/metrics"
    }
    trap "on_hup \"$1\" \"$2\"" HUP
    while :; do sleep 0.1; done
  ' fake "$work" "$1" &
  fake=$!
  sleep 0.3
}

printf '#!/usr/bin/env bash\nexit 0\n' > "$work/promtool-ok"
printf '#!/usr/bin/env bash\necho "FAILED: parsing YAML file x.rules" >&2\nexit 1\n' > "$work/promtool-refuses"
chmod +x "$work/promtool-ok" "$work/promtool-refuses"

# case <name> <expected exit: 0|nonzero> <promtool> <fake mode> <expect a signal: yes|no>
case_() {
  local name="$1" expected="$2" promtool="$3" mode="$4" signal="$5" status
  start_fake "$mode"
  bash "$reload" "$work/$promtool" /dev/null "$fake" "file://$work/metrics" 3 >"$work/out" 2>&1
  status=$?
  kill "$fake" 2>/dev/null; wait "$fake" 2>/dev/null
  local sent=no; [[ -e "$work/hup" ]] && sent=yes
  if { [[ "$expected" == 0 && "$status" == 0 ]] || [[ "$expected" == nonzero && "$status" != 0 ]]; } \
      && [[ "$sent" == "$signal" ]]; then
    ok "$name"
  else
    no "$name -- exit $status (want $expected), signal sent: $sent (want $signal)"
    sed 's/^/       /' "$work/out"
  fi
}

case_ "a reload Prometheus takes exits 0"                      0       promtool-ok       takes   yes
case_ "a reload Prometheus refuses exits non-zero"             nonzero promtool-ok       refuses yes
case_ "a reload Prometheus never answers fails on the deadline" nonzero promtool-ok      ignores yes
case_ "a file promtool refuses is never signalled"             nonzero promtool-refuses  takes   no

printf '%d passed, %d failed\n' "$pass" "$fail"
[[ "$fail" -eq 0 ]]
