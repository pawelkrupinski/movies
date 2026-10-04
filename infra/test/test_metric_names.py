#!/usr/bin/env python3
"""Every metric name a panel, template variable, alert or recording rule reads, checked to exist.

WHY. Prometheus answers an unknown name with an empty result, never an error: a panel over one
draws "No data" that looks like a quiet period, a template variable over one empties its dropdown
and blanks every panel scoped by it, and an alert over one can never fire. Three sources vouch for
three kinds of name, and each is checked against its own:

- `kinowo_*` families -- the CODE is the truth; deploy.GrafanaMetricCoverageSpec checks them
  against the worker/web registries and the fleet scripts' `# TYPE` lines. Skipped here.
- names with a `:` -- recording-rule outputs; they must be defined by a `record:` in the rule
  files, or every reader of a renamed/deleted recording rule silently reads nothing.
- everything else -- the exporters' names (`jvm_*`, `process_*`, `node_*`, `mongodb_*`, `gotk_*`,
  `kube_*`...), which no file in this repository defines. They are checked against
  metric-names.json, the subset of names the fleet Prometheus actually had over a fortnight,
  written by infra/bin/snapshot-label-values. Re-running it cannot launder a typo: it only writes
  names the fleet has.

A name published only in an exceptional state is legitimately absent from a healthy fortnight;
those carry their WHY in alert-backtest-allowlist.yml's `conditional_metrics`, the same list the
live dead-alert backtest excuses.

REFRESHING THE SNAPSHOT. A name the fleet stops exporting stays in a stale snapshot, and every
reader of it passes for good -- so the snapshot carries the day it was taken, and one older than
MAX_AGE_DAYS raises a CI warning annotation. A warning, never a failure: this runs in the job that
gates staging fleet closures, and a calendar date must not block an infra fix (possibly mid-incident)
behind a refresh that needs the fleet SOCKS proxy. An UNDATED snapshot still fails -- that is a code
change. Refresh (read-only: one Prometheus label-values query):

    ssh -f -N kinowo-fleet                          # the fleet SOCKS proxy, 127.0.0.1:1080
    infra/bin/snapshot-label-values --names-only    # rewrites metric-names.json
    python3 infra/test/test_metric_names.py         # a name now missing is a real dead reader

then commit metric-names.json. (Without --names-only it also rewrites label-values.json.)

Run: python3 infra/test/test_metric_names.py   (also run by test_alert_rules.sh)
"""

import datetime

import json
import os
import sys
import unittest

import yaml

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import promql_selectors  # noqa: E402

SNAPSHOT = os.path.join(HERE, "metric-names.json")
ALLOWLIST = os.path.join(HERE, "alert-backtest-allowlist.yml")
RESNAPSHOT = ("re-run infra/bin/snapshot-label-values --names-only (fleet SOCKS proxy up: ssh -f -N kinowo-fleet) "
              "and commit metric-names.json")
# How old the snapshot may be before the check warns: a fortnight's window, refreshed monthly.
MAX_AGE_DAYS = 45


def snapshot_age(document, today):
    """(failure, warning) for `document` on `today`: undated is a failure (a code change can fix it);
    older than MAX_AGE_DAYS only a warning -- see REFRESHING THE SNAPSHOT for why a date never fails."""
    taken = document.get("taken")
    if not taken:
        return "metric-names.json carries no 'taken' date -- " + RESNAPSHOT, None
    age = (today - datetime.date.fromisoformat(taken)).days
    if age > MAX_AGE_DAYS:
        return None, ("metric-names.json was taken %s, %d days ago (limit %d): a name the fleet stopped "
                      "exporting would still pass -- %s" % (taken, age, MAX_AGE_DAYS, RESNAPSHOT))
    return None, None


def expressions():
    return list(promql_selectors.all_expressions()) + list(promql_selectors.template_queries())


def problems(exprs, recorded, fleet_names, conditional):
    """(where, name, why) for every name in `exprs` that nothing vouches for."""
    found = []
    for where, expr in exprs:
        for name in sorted(promql_selectors.metric_names(expr)):
            if name.startswith("kinowo_") or name in conditional:
                continue
            if ":" in name:
                if name not in recorded:
                    found.append((where, name, "no recording rule defines it"))
            elif name not in fleet_names:
                found.append((where, name, "the fleet Prometheus has no such metric (" + RESNAPSHOT + ")"))
        for pattern in promql_selectors.name_patterns(expr):
            if pattern.value.startswith("kinowo_"):
                continue
            if not any(pattern.matches(n) for n in fleet_names | recorded):
                found.append((where, repr(pattern), "the name regex matches no metric the fleet has"))
    return found


class TheCheckItself(unittest.TestCase):
    """The check, fed known-bad input, must say so -- a scanner that matched nothing would pass."""

    def test_flags_a_dead_exporter_name_an_undefined_recording_and_a_dead_regex(self):
        exprs = [("panel", 'rate(node_cpu_seconds_total[5m]) + node_cpu_typo + country:gone:count '
                           '+ country:kept:count + kinowo_anything + {__name__=~"nope_.*"}')]
        found = sorted(name for _, name, _ in
                       problems(exprs, {"country:kept:count"}, {"node_cpu_seconds_total"}, {}))
        self.assertEqual(found, ['__name__=~"nope_.*"', "country:gone:count", "node_cpu_typo"])

    def test_refuses_an_undated_snapshot_and_only_warns_on_a_stale_one(self):
        today = datetime.date(2026, 10, 4)
        self.assertEqual(snapshot_age({"taken": "2026-09-20"}, today), (None, None))
        failure, warning = snapshot_age({"taken": "2026-08-01"}, today)
        self.assertIsNone(failure)   # a calendar date must never turn the staging gate red
        self.assertIn("days ago", warning)
        self.assertIn("no 'taken' date", snapshot_age({}, today)[0])

    def test_a_conditional_metric_is_excused(self):
        self.assertEqual(problems([("rule", "a_only_when_broken == 1")], set(), set(),
                                  {"a_only_when_broken": {"reason": "x"}}), [])


class EveryNameExists(unittest.TestCase):

    def test_the_snapshot_is_fresh(self):
        with open(SNAPSHOT, encoding="utf-8") as handle:
            failure, warning = snapshot_age(json.load(handle), datetime.date.today())
        self.assertIsNone(failure, failure)
        if warning:   # a GitHub workflow command; test_alert_rules.sh passes it through on success
            print("::warning title=Stale metric-name snapshot::" + warning)

    def test_every_name_read_by_a_panel_variable_alert_or_recording_rule_exists(self):
        with open(SNAPSHOT, encoding="utf-8") as handle:
            fleet_names = set(json.load(handle)["names"])
        with open(ALLOWLIST, encoding="utf-8") as handle:
            conditional = yaml.safe_load(handle).get("conditional_metrics") or {}
        exprs = expressions()
        self.assertGreater(len(exprs), 100)  # the readers reach the files
        self.assertGreater(len(fleet_names), 20)
        found = problems(exprs, promql_selectors.recorded_names(), fleet_names, conditional)
        self.assertEqual(found, [], "\n" + "\n".join("%s: %s -- %s" % f for f in found))

    def test_the_snapshot_carries_no_name_nothing_reads(self):
        with open(SNAPSHOT, encoding="utf-8") as handle:
            fleet_names = set(json.load(handle)["names"])
        read = set()
        for _, expr in expressions():
            read.update(promql_selectors.metric_names(expr))
            for pattern in promql_selectors.name_patterns(expr):
                read.update(n for n in fleet_names if pattern.matches(n))
        stale = sorted(fleet_names - read)
        self.assertEqual(stale, [], "no panel or rule reads these any more -- " + RESNAPSHOT)


if __name__ == "__main__":
    unittest.main()
