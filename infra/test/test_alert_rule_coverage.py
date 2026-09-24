#!/usr/bin/env python3
"""The alerting rules' promtool suites, asked whether they test what matters about each rule.

test_alert_rules.sh runs the suites; this asks what they COVER. Two classes of alert slipped past
a green suite on 2026-09-23, and each check below is written against the class, not the instance:

1. AN ALERT WITH NO FIRING CASE MAY NEVER FIRE, AND ONE WITH NO QUIET CASE MAY NEVER STOP.
   DuplicateVenueListing, RetiredVenueRowsLingering and ReadModelServingDiffersFromCorpus were
   each valid PromQL, loaded fine, and could not fire in production. A suite that asserts the
   alert fires for some series is the only evidence it can; one that asserts silence for a
   healthy series is the only evidence it is not `MongodNoPrimary` -- firing on the PRESENCE of
   a sample rather than its truth. Every alert owes both, Grafana-managed ones included (their
   `expr` evaluated through `promql_expr_test`, the condition's threshold applied here).

2. A LONG `for:` OVER A WORKER GAUGE IS RESET BY EVERY WORKER RESTART. A pod change leaves the
   worker's gauges absent for ~3 minutes; the pending alert is dropped with them and its hold
   starts over. At ~16 worker restarts on a deploy day, a 24h hold (DuplicateVenueListing) or a
   50h one (RetiredVenueRowsLingering) effectively never completes. Six alerts had this shape.
   Any alert holding 15 minutes or more must read each worker gauge through a range function
   that bridges the gap (`last_over_time(g[10m])`, or a rate/deriv over a window), and must
   have a case that fires ACROSS a `stale` gap -- the shape of a restart in promtool's input.
   `absent(...)` of a gauge is exempt by construction: absence is the thing it is watching.

Run: python3 infra/test/test_alert_rule_coverage.py   (also run by test_alert_rules.sh)
"""

import glob
import os
import re
import sys
import unittest

import yaml

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import promql_selectors  # noqa: E402

SUITE_FILES = sorted(glob.glob(os.path.join(HERE, "alert-rules", "*.yml")))

# The one alert that must never be quiet: it is the dead-man's switch that proves delivery works,
# `expr: vector(1)`. A case asserting it silent could only ever fail.
ALWAYS_FIRING = {"MonitoringHeartbeat"}

# The restart gap: 15 minutes of hold is where a ~3 minute gap starts to matter (it is a fifth of
# the hold), and a bridging window shorter than the gap bridges nothing.
LONG_HOLD_SECONDS = 15 * 60
RESTART_GAP_SECONDS = 5 * 60

_WORKER_GAUGE = re.compile(
    r'(?<![A-Za-z0-9_:])(kinowo_worker_[A-Za-z0-9_]+)'
    r'(\{(?:[^{}"]|"(?:[^"\\]|\\.)*")*\})?'
    r'(\s*\[\s*([0-9smhdw]+)\s*(?::[^\]]*)?\])?')
_COUNTER_SUFFIX = re.compile(r'_(total|bucket|count|sum|created)$')


def seconds(duration):
    """A Prometheus duration ("1h30m", "10m", "2d") in seconds; 0 for none."""
    units = {"s": 1, "m": 60, "h": 3600, "d": 86400, "w": 604800}
    return sum(int(n) * units[u] for n, u in re.findall(r'(\d+)([smhdw])', duration or ""))


def squash(expr):
    """An expression with its whitespace removed, so a folded YAML copy compares equal."""
    return re.sub(r'\s+', '', expr)


def load(path):
    with open(path, encoding="utf-8") as handle:
        return yaml.safe_load(handle)


def prometheus_alerts():
    """{alert name: (rule file basename, rule)} for every alert Prometheus loads."""
    alerts = {}
    for path in promql_selectors.RULE_FILES:
        for group in load(path)["groups"]:
            for rule in group["rules"]:
                if "alert" in rule:
                    alerts[rule["alert"]] = (os.path.basename(path), rule)
    return alerts


def grafana_alerts():
    """[(uid, rule, condition query expr, (evaluator type, threshold))] for every Grafana rule.

    Every rule in this tree has the same shape -- a threshold node (the rule's `condition`)
    comparing the `last` of one query against one number -- and this refuses any other shape
    rather than guessing what it means."""
    found = []
    for group in load(promql_selectors.GRAFANA_ALERT_RULES)["groups"]:
        for rule in group["rules"]:
            nodes = {node["refId"]: node["model"] for node in rule["data"]}
            condition = nodes[rule["condition"]]
            assert condition.get("type") == "threshold", "%s: condition is not a threshold" % rule["uid"]
            evaluator = condition["conditions"][0]["evaluator"]
            query = nodes[condition["expression"]]["expr"]
            found.append((rule["uid"], rule, query, (evaluator["type"], evaluator["params"][0])))
    return found


def suites():
    """[(basename, parsed suite)] for every promtool suite under alert-rules/."""
    return [(os.path.basename(path), load(path)) for path in SUITE_FILES]


def crosses(evaluator, value):
    kind, threshold = evaluator
    value = float(value)
    return {"gt": value > threshold, "lt": value < threshold}[kind]


def worker_gauges(expr):
    """[(gauge, range window or None, inside absent())] for each worker-gauge selector in expr."""
    found = []
    for match in _WORKER_GAUGE.finditer(expr):
        name = match.group(1)
        if _COUNTER_SUFFIX.search(name):
            continue
        before = expr[:match.start()].rstrip()
        in_absent = re.search(r'\babsent(_over_time)?\s*\($', before) is not None
        found.append((name, match.group(4), in_absent))
    return found


def stale_series(test, gauge):
    return any(re.match(r'%s(\{|$)' % re.escape(gauge), s["series"]) and "stale" in str(s["values"])
               for s in test.get("input_series") or [])


class EveryAlertFiresAndStaysQuiet(unittest.TestCase):

    def test_every_prometheus_alert_has_a_firing_and_a_quiet_case(self):
        alerts = prometheus_alerts()
        fires, quiet = set(), set()
        for name, suite in suites():
            loaded = {os.path.basename(f) for f in suite.get("rule_files") or []}
            for test in suite.get("tests", []):
                for case in test.get("alert_rule_test") or []:
                    alert = case["alertname"]
                    if alert in alerts and alerts[alert][0] in loaded:
                        (fires if case.get("exp_alerts") else quiet).add(alert)
        missing = []
        for alert, (rules, _) in sorted(alerts.items()):
            if alert not in fires:
                missing.append("%s %s: no case where it FIRES" % (rules, alert))
            if alert not in quiet and alert not in ALWAYS_FIRING:
                missing.append("%s %s: no case where it stays QUIET" % (rules, alert))
        self.assertEqual([], missing, "\n" + "\n".join(missing))

    def test_every_grafana_alert_expression_is_evaluated_to_both_sides_of_its_threshold(self):
        cases = {}
        for _, suite in suites():
            for test in suite.get("tests", []):
                for case in test.get("promql_expr_test") or []:
                    cases.setdefault(squash(case["expr"]), []).append(case)
        missing = []
        for uid, _, expr, evaluator in grafana_alerts():
            evaluated = cases.get(squash(expr), [])
            # A listed sample is one series' answer: past the threshold it FIRES, short of it that
            # series stays QUIET. A case expecting no samples at all is quiet for every series.
            values = [s["value"] for c in evaluated for s in c.get("exp_samples") or []]
            if not any(crosses(evaluator, v) for v in values):
                missing.append("%s: no promql_expr_test of its expr crosses %s %s (FIRES)"
                               % (uid, evaluator[0], evaluator[1]))
            if all(crosses(evaluator, v) for v in values) and all(c.get("exp_samples") for c in evaluated):
                missing.append("%s: no promql_expr_test of its expr stays short of %s %s (QUIET)"
                               % (uid, evaluator[0], evaluator[1]))
        self.assertEqual([], missing, "\n" + "\n".join(missing))


class LongHoldsSurviveAWorkerRestart(unittest.TestCase):

    def assertBridged(self, where, expr):
        problems = []
        for gauge, window, in_absent in sorted(set(worker_gauges(expr)), key=str):
            if in_absent:
                continue
            if window is None:
                problems.append("%s reads %s raw, so a worker restart's ~3m gap resets its hold "
                                "-- read it through last_over_time(%s[10m])" % (where, gauge, gauge))
            elif seconds(window) < RESTART_GAP_SECONDS:
                problems.append("%s reads %s over [%s], shorter than a restart gap"
                                % (where, gauge, window))
        return problems

    def test_a_long_held_prometheus_alert_bridges_the_restart_gap(self):
        alerts = prometheus_alerts()
        all_suites = suites()
        problems = []
        for alert, (rules, rule) in sorted(alerts.items()):
            if seconds(rule.get("for")) < LONG_HOLD_SECONDS:
                continue
            where = "%s %s (for: %s)" % (rules, alert, rule.get("for"))
            problems += self.assertBridged(where, rule["expr"])
            for gauge in sorted({g for g, _, a in worker_gauges(rule["expr"]) if not a}):
                if not any(stale_series(test, gauge)
                           and any(c["alertname"] == alert and c.get("exp_alerts")
                                   for c in test.get("alert_rule_test") or [])
                           for _, suite in all_suites for test in suite.get("tests", [])):
                    problems.append("%s: no case fires across a `stale` gap in %s" % (where, gauge))
        self.assertEqual([], problems, "\n" + "\n".join(problems))

    def test_a_long_held_grafana_alert_bridges_the_restart_gap(self):
        all_suites = suites()
        problems = []
        for uid, rule, expr, evaluator in grafana_alerts():
            if seconds(rule.get("for")) < LONG_HOLD_SECONDS:
                continue
            where = "alert-rules.yaml %s (for: %s)" % (uid, rule.get("for"))
            problems += self.assertBridged(where, expr)
            for gauge in sorted({g for g, _, a in worker_gauges(expr) if not a}):
                if not any(stale_series(test, gauge)
                           and any(squash(c["expr"]) == squash(expr)
                                   and any(crosses(evaluator, s["value"]) for s in c.get("exp_samples") or [])
                                   for c in test.get("promql_expr_test") or [])
                           for _, suite in all_suites for test in suite.get("tests", [])):
                    problems.append("%s: no case evaluates it across a `stale` gap in %s" % (where, gauge))
        self.assertEqual([], problems, "\n" + "\n".join(problems))


if __name__ == "__main__":
    unittest.main(verbosity=2)
