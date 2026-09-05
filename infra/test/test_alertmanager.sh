#!/usr/bin/env bash
# ALERTMANAGER'S CONFIGURATION, ASKED WHERE AN ALERT ACTUALLY GOES.
#
# NOTHING WAS ASKING. `nix eval` proves the file RENDERS -- the substitute step fails on an
# unsubstituted `@PLACEHOLDER@` -- and that is the whole of the coverage this document had. It says
# nothing about whether the YAML parses as Alertmanager's schema, and nothing at all about routing,
# which is the part that decides whether an alert is delivered or silently swallowed by the wrong
# branch. A route matched in the wrong ORDER is the specific mistake here: `route.routes` is
# first-match-wins, so a route added below the `severity` ones is dead the moment it is written,
# and it looks perfectly correct in review.
#
# It exists now because the disk alerts were given a second destination (email beside Telegram) and
# that change is exactly the shape nothing could catch: it is ordering-dependent, it is invisible
# until an alert fires, and its failure mode is a message that does not arrive.
#
# THE PLACEHOLDERS ARE SUBSTITUTED WITH DUMMIES HERE, the same ones nix substitutes for real. This
# checks the SHAPE of the document, not the fleet's secrets -- there is nothing sensitive in this
# script and it needs no credentials to run.
#
# Run: infra/test/test_alertmanager.sh   (also run by infra/bin/check)
set -uo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
infra="$(cd "$here/.." && pwd)"
failed=0

# `amtool` from PATH when it is there and from nixpkgs otherwise -- the same fallback
# test_alert_rules.sh uses for promtool, and for the same reason: a checker that quietly skips
# itself is the failure mode this directory is written against.
if command -v amtool >/dev/null 2>&1; then
  amtool() { command amtool "$@"; }
elif command -v nix >/dev/null 2>&1; then
  amtool() {
    nix --extra-experimental-features 'nix-command flakes' shell 'nixpkgs#prometheus-alertmanager' \
      -c amtool "$@"
  }
else
  echo "  FAILED neither amtool nor nix is on PATH, so the alertmanager config was not checked."
  exit 1
fi

rendered="$(mktemp)"
trap 'rm -f "$rendered"' EXIT

sed -e 's|@TELEGRAM_BOT_TOKEN_FILE@|/run/secrets/telegram|g' \
    -e 's|@SMTP_SMARTHOST@|smtp.example.invalid:587|g' \
    -e 's|@SMTP_USERNAME@|dummy|g' \
    -e 's|@SMTP_PASSWORD_FILE@|/run/secrets/smtp|g' \
    -e 's|@ALERT_EMAIL_FROM@|alerts@example.invalid|g' \
    -e 's|@ALERT_EMAIL_TO@|operator@example.invalid|g' \
    "$infra/nix/files/monitoring/alertmanager.yaml" > "$rendered"

if grep -nE '@[A-Z0-9_]+@' "$rendered"; then
  echo "  FAILED alertmanager.yaml carries a placeholder this test does not substitute (above)."
  echo "         Add it here AND to the render step in nix/modules/roles/prometheus.nix -- a"
  echo "         placeholder only one of them knows about reaches the host unsubstituted."
  failed=1
fi

step() { printf '\n\033[1m==> %s\033[0m\n' "$1"; }

step "alertmanager config parses"
if out="$(amtool check-config "$rendered" 2>&1)"; then
  echo "  ok  alertmanager.yaml parses, and every receiver and template with it"
else
  echo "  FAILED amtool check-config rejected alertmanager.yaml:"
  printf '         %s\n' "$out"
  failed=1
fi

# WHERE DOES AN ALERT GO. `amtool config routes test` walks the real routing tree and prints the
# receiver an alert with those labels would reach -- the question that decides delivery, and the
# one no amount of reading the file reliably answers.
step "routing"
route_is() {
  local want="$1"; shift
  local got
  # STDOUT ONLY, AND THE LAST LINE OF IT. When amtool comes from `nix shell`rather than PATH, nix
  # is free to write to stderr -- "SQLite database ... is busy" from a contended eval cache is the
  # one that showed up, and folding it into the comparison turned five correct routes into five
  # failures whose message contained the right answer. A checker that fails on the weather is worse
  # than no checker. A genuine amtool failure still fails this: it prints nothing usable on stdout,
  # so `got` ends up empty and cannot equal any expected receiver.
  got="$(amtool config routes test --config.file "$rendered" "$@" 2>/dev/null | tail -1 | tr -d '[:space:]')"
  if [ "$got" = "$want" ]; then
    echo "  ok  $* -> $want"
  else
    echo "  FAILED $* -> '$got', expected '$want'"
    failed=1
  fi
}

# THE CHANGE THIS FILE WAS WRITTEN FOR. Both severities of every disk alert must reach the
# receiver that carries email, and they must reach it DESPITE the `severity` routes below them --
# which is the ordering assertion, and the one that fails if the routes are ever moved.
route_is telegram-and-email alertname=FilesystemSpaceLow severity=warning host=mongo-1 mountpoint=/
route_is telegram-and-email alertname=FilesystemSpaceCritical severity=critical host=mongo-1 mountpoint=/
route_is telegram-and-email alertname=FilesystemInodesLow severity=warning host=k3s-worker-1 mountpoint=/
route_is telegram-and-email alertname=FilesystemWillFillWithin7Days severity=warning host=mongo-1
# "nothing is watching any disk" belongs with the disk alerts, and carries no host label.
route_is telegram-and-email alertname=FilesystemMetricsAbsent severity=warning
# LIVES IN host-health.rules, NOT WITH THE OTHER SIX, so it is the one the prefix catches that a
# reader of filesystem-capacity.rules would not think to look for. Pinned so that stays deliberate.
route_is telegram-and-email alertname=FilesystemReadOnly severity=critical host=mongo-1

# THE WORKER PIPELINE EARNS THE MAILBOX FOR A DIFFERENT REASON than the disks: a stalled or
# runaway queue does not 500 anything. The site keeps serving and the listings just go stale, which
# is the failure most likely to be scrolled past in a chat channel. Both severities, and the
# absent() companion, must reach email the same way the disk alerts do.
route_is telegram-and-email alertname=WorkerQueueStalled severity=critical country=us
route_is telegram-and-email alertname=WorkerQueueGrowingUnbounded severity=warning country=uk
route_is telegram-and-email alertname=WorkerDown severity=critical country=de
route_is telegram-and-email alertname=WorkerQueueMetricsAbsent severity=warning

# AND NOTHING ELSE CHANGED. The email receiver is for the disk alerts alone; every other alert must
# still land on plain Telegram, or "add email for the disks" has quietly become "add email".
route_is telegram alertname=MongodNotRunning severity=critical host=mongo-1
route_is telegram alertname=CinemaScrapeOldestAgeHigh severity=warning country=de
route_is telegram alertname=ReadModelProjectionTriggerUnaccounted severity=warning country=us
route_is telegram alertname=JvmHeapHigh severity=warning host=k3s-worker-1

# The dead-man's handle keeps its own receiver: it must not acquire `send_resolved`, and it must
# not start arriving by email every day.
route_is telegram-heartbeat alertname=MonitoringHeartbeat

if ((failed)); then
  printf '\n\033[1;31mtest_alertmanager: FAILED\033[0m\n'
else
  printf '\n\033[1;32mtest_alertmanager: all green\033[0m\n'
fi
exit "$failed"
