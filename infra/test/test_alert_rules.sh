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
