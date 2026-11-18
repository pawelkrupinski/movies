#!/usr/bin/env bash
# prometheus.service's ExecReload (roles/prometheus.nix): reload a running Prometheus and EXIT
# NON-ZERO UNLESS IT TOOK THE NEW CONFIGURATION, so a switch that reloads it fails visibly.
#
#   prometheus-reload.sh <promtool> <config file> <main pid> <metrics url> [deadline seconds]
#
# WHY NOT A BARE `kill -HUP $MAINPID`. SIGHUP is asynchronous: kill returns 0 at once, Prometheus
# then re-reads the configuration and, if a rule file does not parse, KEEPS RUNNING THE OLD ONE and
# says so only in its log and in `prometheus_config_last_reload_successful`. switch-to-configuration
# would report the reload as done, the host as current, and the edited alert would simply not
# exist. So this checks twice:
#
#   1. BEFORE: `promtool check config`, which parses the config AND every file in `rule_files` --
#      a broken file is refused without signalling anything, and the running rules stay in force.
#   2. AFTER: waits for Prometheus's own verdict -- `..._last_reload_success_timestamp_seconds`
#      moving past its value before the signal (success), or `..._last_reload_successful` dropping
#      from 1 to 0 (failure) -- and fails on the deadline if neither happens. A host whose previous
#      reload already failed reads 0 before and after, so its next failure is caught by the deadline.
#
# NOT `POST /-/reload`, which is synchronous and would answer the question directly: it needs
# `--web.enable-lifecycle`, which also opens `/-/quit` to anything on the private network.
set -euo pipefail

promtool="$1" config="$2" pid="$3" url="$4" deadline="${5:-30}"

metric() {
  curl -fsS --max-time 5 "$url" | awk -v name="$1" '$1 == name { print $2 }'
}

if ! "$promtool" check config "$config"; then
  echo "prometheus-reload: promtool rejects $config (above); NOT reloading -- the running rules stay in force." >&2
  exit 1
fi

before_success="$(metric prometheus_config_last_reload_successful)"
before_stamp="$(metric prometheus_config_last_reload_success_timestamp_seconds)"
kill -HUP "$pid"

for ((elapsed = 0; elapsed < deadline; elapsed++)); do
  sleep 1
  success="$(metric prometheus_config_last_reload_successful || true)"
  stamp="$(metric prometheus_config_last_reload_success_timestamp_seconds || true)"
  if [[ "$success" == 1 && -n "$stamp" && "$stamp" != "$before_stamp" ]]; then
    echo "prometheus-reload: reloaded at $stamp (was $before_stamp)."
    exit 0
  fi
  if [[ "$success" == 0 && "$before_success" == 1 ]]; then
    echo "prometheus-reload: Prometheus REJECTED the new configuration and keeps the old one; journalctl -u prometheus names why." >&2
    exit 1
  fi
done

echo "prometheus-reload: no successful reload within ${deadline}s (successful=${success:-?}, timestamp still ${before_stamp}); journalctl -u prometheus names why." >&2
exit 1
