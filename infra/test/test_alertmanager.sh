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
# The two rules that watch whether the movement ACHIEVES anything, added 2026-09-06.
route_is telegram-and-email alertname=WorkerTasksFailingRepeatedly severity=warning country=pl
route_is telegram-and-email alertname=WorkerTaskTypeUnhandled severity=warning country=de

# AND NOTHING ELSE CHANGED. The email receiver is for the disk alerts alone; every other alert must
# still land on plain Telegram, or "add email for the disks" has quietly become "add email".
route_is telegram alertname=MongodNotRunning severity=critical host=mongo-1
route_is telegram alertname=CinemaScrapeOldestAgeHigh severity=warning country=de
route_is telegram alertname=ReadModelProjectionTriggerUnaccounted severity=warning country=us
route_is telegram alertname=JvmHeapHigh severity=warning host=k3s-worker-1

# The dead-man's handle keeps its own receiver: it must not acquire `send_resolved`, and it must
# not start arriving by email every day.
route_is telegram-heartbeat alertname=MonitoringHeartbeat

# ------------------------------------------------------------------------------------------------
# INHIBITION, WHICH `amtool` CANNOT TEST AND WHICH SILENTLY DELETES ALERTS WHEN IT IS WRONG.
#
# `amtool config routes test` answers "where does this alert go"; there is no equivalent for "what
# does this alert SILENCE". That gap hid a real one: `equal:` compares an ABSENT label to an absent
# label as EQUAL, so an inhibit rule keyed on `host` whose source is not required to HAVE a host
# matched every host-less alert on the fleet at once. Between 2026-09-05 and 2026-09-06 a single
# OOM-killed worker would have muted every host-less warning we publish -- the `absent()` companions
# included, which are the alerts that say nothing is watching any more.
#
# THE INVARIANT, checked mechanically rather than by reading: for every inhibit rule, at least one
# of the labels in `equal:` must be PINNED NON-EMPTY by the rule's own `source_matchers`. That is
# what stops the empty-matches-empty case, and it is a property of the document rather than of the
# fleet, so it holds no matter which alerts happen to exist.
step "inhibit rules pin their equal-labels on the source side"
if ! python3 - "$rendered" <<'PY'
import re, sys, yaml

document = yaml.safe_load(open(sys.argv[1]))
# `<label> =~ ".+"` or `<label> = "something"` -- either one requires the source to carry a value.
present = re.compile(r'^\s*(\w+)\s*(=~|=)\s*"([^"]*)"\s*$')
# A regex that an EMPTY value still satisfies pins nothing: `.*`, the empty string, and any
# alternation with an empty branch (`.+|`) all match "". Checked explicitly because the first
# version of this check denied only the first two and would have passed the third.
def pins(operator, value):
    if operator == "=":
        return value != ""
    return value not in ("", ".*") and not any(branch == "" or branch == ".*" for branch in value.split("|"))

bad = 0
rules = document.get("inhibit_rules", [])
for index, rule in enumerate(rules, start=1):
    equal = rule.get("equal", [])
    if not equal:
        # NOT a pass. An inhibit rule with no `equal:` silences every matching target regardless of
        # labels, which is strictly worse than the empty-join this check exists to catch. The first
        # version `continue`d here and still counted the rule in its "all ok" tally.
        bad = 1
        print(f"  FAILED inhibit rule #{index} has no `equal:` at all, so it silences every target "
              f"it matches on any host or country. Give it the label it means to join on.")
        continue
    pinned = set()
    for matcher in rule.get("source_matchers", []):
        found = present.match(matcher)
        if found and pins(found.group(2), found.group(3)):
            pinned.add(found.group(1))
    if not (pinned & set(equal)):
        bad = 1
        print(f"  FAILED inhibit rule #{index} joins on {equal} but its source_matchers "
              f"{rule.get('source_matchers')} do not require any of those labels to be present.")
        print( "         An alert missing that label would match every other alert missing it, and")
        print( "         silence all of them. Add `<label> =~ \".+\"` to the source matchers.")
if not bad:
    print(f"  ok  {len(rules)} inhibit rules all join on a label their source must carry")
# WHAT THIS STILL DOES NOT CATCH, said out loud so the next reader does not over-trust it: only ONE
# label of a multi-label `equal:` has to be pinned. The host rule pins `host` and not `mountpoint`,
# so a mountpoint-less critical still matches mountpoint-less warnings on ITS OWN host. That is
# host-scoped rather than fleet-scoped, it predates this check, and tightening it would break the
# memory pair the rule's comment is written around -- but it is broader than that comment implies.
sys.exit(bad)
PY
then
  failed=1
fi

if ((failed)); then
  printf '\n\033[1;31mtest_alertmanager: FAILED\033[0m\n'
else
  printf '\n\033[1;32mtest_alertmanager: all green\033[0m\n'
fi
exit "$failed"
