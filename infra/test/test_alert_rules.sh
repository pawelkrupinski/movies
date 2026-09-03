#!/usr/bin/env bash
# THE ALERTING RULES, ASKED WHETHER THEY MEAN WHAT THEY SAY.
#
# Two different questions, and only the first one was ever being asked -- by Prometheus itself, at
# load time, on the host, after a deploy:
#
#   check rules  does the file PARSE and is every expression valid PromQL. A malformed rule file is
#                refused wholesale, so one typo takes every rule in it -- including the rules about
#                the thing that is on fire -- off the air at once.
#   test rules   does the expression FIRE WHEN IT SHOULD, and stay silent when it should not. This
#                is the question nothing was asking, and `MongodNoPrimary` is why it now is: it was
#                `max(state == 1) or vector(0)`, valid PromQL, parsed happily, loaded happily, and
#                fired continuously against a healthy replica set because AN ALERTING EXPRESSION
#                FIRES ON THE PRESENCE OF A SAMPLE RATHER THAN ON ITS TRUTH. Reading it does not
#                help -- it reads like a question. Feeding it a series does.
#
# Only files under test/alert-rules/ are executed, so a rule group with no test is not a failure
# here; the point is that the ones with tests are pinned. `promtool` comes from PATH when it is
# there and from nixpkgs otherwise, because bin/check already requires nix for every host it
# evaluates and a checker that quietly skips itself is the failure mode this whole directory is
# written against.
set -uo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
infra="$(cd "$here/.." && pwd)"

if command -v promtool >/dev/null 2>&1; then
  promtool() { command promtool "$@"; }
elif command -v nix >/dev/null 2>&1; then
  # `prometheus.cli` AND NOT `prometheus`. The server package ships `prometheus` and `migrate` only;
  # promtool lives in the split `cli` output, and asking for the wrong one fails with "unable to
  # execute 'promtool'" long after the download.
  promtool() {
    nix --extra-experimental-features 'nix-command flakes' shell 'nixpkgs#prometheus.cli' \
      -c promtool "$@"
  }
else
  echo "  FAILED neither promtool nor nix is on PATH, so the alerting rules were not checked."
  exit 1
fi

failed=0

# ── THE THREE LISTS THAT MUST AGREE ───────────────────────────────────────────────────────────────
# A rule file has to be named in THREE places to actually alert: it must exist on disk, be in
# `ruleNames` in nix/modules/roles/prometheus.nix (which INSTALLS it into /etc), and be in
# `rule_files` in nix/files/monitoring/prometheus.yaml (which makes Prometheus LOAD it).
#
# Miss the third and the failure is SILENT AND TOTAL: the file ships, `promtool` is perfectly happy
# with it, Prometheus starts clean with no error in the log -- and the group is simply not there, so
# the alerts never fire. Nothing anywhere says so.
#
# That is not hypothetical. `jvm-heap.rules` was added to `ruleNames` and to the repository on
# 2026-09-03 and NOT to `rule_files`; it reached /etc/prometheus/rules/ on the very next fleet
# apply, Prometheus reloaded without complaint, and the alert it exists for could never have fired.
# It was caught by asking the running Prometheus which groups it had, which is not a check anyone
# should have to remember to run. prometheus.nix already said this guard "does not have yet and
# should" -- this is it.
set -o pipefail
nix_module="$infra/nix/modules/roles/prometheus.nix"
prom_yaml="$infra/nix/files/monitoring/prometheus.yaml"

# `ruleNames` entries are bare quoted names; take only the ones inside that list.
installed="$(sed -n '/^  ruleNames = \[/,/^  \];/p' "$nix_module" | grep -oE '"[a-z0-9-]+"' | tr -d '"' | sort -u)"
# `rule_files` entries are quoted absolute paths ending in .rules.
loaded="$(sed -n '/^rule_files:/,/^$/p' "$prom_yaml" | grep -oE '/etc/prometheus/rules/[a-z0-9-]+\.rules' | sed 's|.*/||; s|\.rules$||' | sort -u)"
present="$(ls "$infra"/nix/files/monitoring/rules/*.rules 2>/dev/null | sed 's|.*/||; s|\.rules$||' | sort -u)"

if [ "$installed" = "$loaded" ] && [ "$installed" = "$present" ]; then
  echo "  ok  every rule file is on disk, installed by ruleNames and loaded by rule_files"
else
  echo "  FAILED the rule-file lists disagree -- an alert in only some of them never fires."
  echo "         on disk (nix/files/monitoring/rules/):"; echo "$present"   | sed 's/^/           /'
  echo "         installed (prometheus.nix ruleNames):";  echo "$installed" | sed 's/^/           /'
  echo "         loaded (prometheus.yaml rule_files):";   echo "$loaded"    | sed 's/^/           /'
  failed=1
fi

for rules in "$infra"/nix/files/monitoring/rules/*.rules; do
  [ -e "$rules" ] || continue
  if out="$(promtool check rules "$rules" 2>&1)"; then
    echo "  ok  $(basename "$rules") parses"
  else
    echo "  FAILED $(basename "$rules")"
    echo "$out" | sed 's/^/         /'
    failed=1
  fi
done

for suite in "$here"/alert-rules/*.yml; do
  [ -e "$suite" ] || continue
  if out="$(cd "$(dirname "$suite")" && promtool test rules "$(basename "$suite")" 2>&1)"; then
    echo "  ok  $(basename "$suite") -- every case behaves"
  else
    echo "  FAILED $(basename "$suite")"
    echo "$out" | sed 's/^/         /'
    failed=1
  fi
done

exit "$failed"
