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
   Two readings the same gap resets are held to the same bar: `process_start_time_seconds` (the
   uptime gates, absent for the same ~3 minutes), and a RECORDED series the alert reads raw whose
   recording rule reads a worker gauge raw -- the recorded series goes absent with it. A recording
   rule that latches itself (`last_over_time(<its own name>[...])`) is exempt: carrying its own
   previous value across a gap is what it is for.

3. A RESTART RE-SEEDS AN UNMEASURED GAUGE AT NaN, AND last_over_time RETURNS IT. Every worker family exists
   from boot (deploy.WorkerMetricsAtBootSpec); a reading nothing has taken yet is seeded NaN rather than a 0 that
   would misstate it. NaN passes no comparison, so a long-held alert reading such a gauge through
   `last_over_time` starts its hold over at every restart, exactly as an absent one did before the bridge. Those
   families are the catalogue's `boot: NaN` ones (app_metrics_catalogue.py); a long-held alert must read them through
   `max_over_time` / `min_over_time`, which skip a NaN while the window holds any real sample.

4. AN ALERT OVER A METRIC THAT IS NOT THERE CAN NEVER FIRE. Every application family an alert reads owes an
   alert on its absence: a hand-written one (`absent(...)` of it, or an `...Absent` alert reading it), or the
   generated AppMetricAbsent rule app_metrics_absent.py writes from the alerts themselves -- whose checked-in output
   must be current. A family whose absence is healthy is in that module's NOT_GUARDED, with the reason.

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
import app_metrics_absent  # noqa: E402
import app_metrics_catalogue  # noqa: E402
import promql_selectors  # noqa: E402
from promql_selectors import seconds  # noqa: E402

SUITE_FILES = sorted(glob.glob(os.path.join(HERE, "alert-rules", "*.yml")))

# The one alert that must never be quiet: it is the dead-man's switch that proves delivery works,
# `expr: vector(1)`. A case asserting it silent could only ever fail.
ALWAYS_FIRING = {"MonitoringHeartbeat"}

# The restart gap: 15 minutes of hold is where a ~3 minute gap starts to matter (it is a fifth of
# the hold), and a bridging window shorter than the gap bridges nothing.
LONG_HOLD_SECONDS = 15 * 60
RESTART_GAP_SECONDS = 5 * 60

_WORKER_GAUGE = re.compile(
    r'(?<![A-Za-z0-9_:])(kinowo_worker_[A-Za-z0-9_]+|process_start_time_seconds)'
    r'(\{(?:[^{}"]|"(?:[^"\\]|\\.)*")*\})?'
    r'(\s*\[\s*([0-9smhdw]+)\s*(?::[^\]]*)?\])?')
_COUNTER_SUFFIX = re.compile(r'_(total|bucket|count|sum|created)$')


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


# A recorded series name (`level:metric:operations`), with the range window it is read over if any.
_RECORDED = re.compile(r'(?<![A-Za-z0-9_:])([a-zA-Z_][a-zA-Z0-9_]*:[a-zA-Z0-9_:]+)'
                       r'(\{(?:[^{}"]|"(?:[^"\\]|\\.)*")*\})?'
                       r'(\s*\[\s*([0-9smhdw]+)\s*(?::[^\]]*)?\])?')


def recording_rules():
    """{recorded series name: expr} for every recording rule Prometheus loads."""
    return {rule["record"]: rule["expr"]
            for path in promql_selectors.RULE_FILES
            for group in load(path)["groups"]
            for rule in group["rules"] if "record" in rule}


def recorded_reads(expr, recordings, seen=None):
    """[(name, expr)] for each recorded series `expr` reads RAW (no bridging window), followed
    through recorded series those rules read raw in turn. Self-latching rules are not followed."""
    seen = set() if seen is None else seen
    found = []
    for match in _RECORDED.finditer(expr):
        name, window = match.group(1), match.group(4)
        if name not in recordings or name in seen:
            continue
        if window is not None and seconds(window) >= RESTART_GAP_SECONDS:
            continue
        seen.add(name)
        inner = recordings[name]
        if re.search(r'last_over_time\(\s*%s\b' % re.escape(name), inner):
            continue
        found.append((name, inner))
        found += recorded_reads(inner, recordings, seen)
    return found


def unmeasured_at_boot():
    """The worker and web families seeded at NaN at boot (see the module doc, 3), from the metric catalogue."""
    return app_metrics_catalogue.unmeasured_at_boot()


def last_over_time_reads(expr, families):
    """The `families` `expr` reads through last_over_time, which returns a restart's NaN seed."""
    return sorted({name for name in families
                   if re.search(r'last_over_time\(\s*%s(\{(?:[^{}"]|"(?:[^"\\]|\\.)*")*\})?\s*\[' % re.escape(name), expr)})


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

    def test_the_check_follows_a_recorded_series_to_its_raw_gauge(self):
        # The positive control for `recorded_reads`: without it the check below could pass by
        # following nothing.
        recordings = {
            "country:raw:sum": "sum by (country) (kinowo_worker_things)",
            "country:bridged:sum": "sum by (country) (last_over_time(kinowo_worker_things[10m]))",
            "country:latch:seconds": "last_over_time(country:latch:seconds[90s]) or kinowo_worker_things",
        }
        followed = recorded_reads(
            "country:raw:sum > 0 and country:bridged:sum > 0 and country:latch:seconds > 0 "
            "and last_over_time(country:raw:sum[10m]) > 0", recordings)
        self.assertEqual(["country:raw:sum", "country:bridged:sum"], [n for n, _ in followed])
        self.assertTrue(self.assertBridged("x", recordings["country:raw:sum"]))
        self.assertFalse(self.assertBridged("x", recordings["country:bridged:sum"]))
        self.assertTrue(self.assertBridged("x", "time() - process_start_time_seconds{job=\"w\"} > 1"))

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
        recordings = recording_rules()
        all_suites = suites()
        problems = []
        for alert, (rules, rule) in sorted(alerts.items()):
            if seconds(rule.get("for")) < LONG_HOLD_SECONDS:
                continue
            where = "%s %s (for: %s)" % (rules, alert, rule.get("for"))
            problems += self.assertBridged(where, rule["expr"])
            for name, inner in recorded_reads(rule["expr"], recordings):
                problems += self.assertBridged("%s, through %s," % (where, name), inner)
            for gauge in sorted({g for g, _, a in worker_gauges(rule["expr"]) if not a}):
                if not any(stale_series(test, gauge)
                           and any(c["alertname"] == alert and c.get("exp_alerts")
                                   for c in test.get("alert_rule_test") or [])
                           for _, suite in all_suites for test in suite.get("tests", [])):
                    problems.append("%s: no case fires across a `stale` gap in %s" % (where, gauge))
        self.assertEqual([], problems, "\n" + "\n".join(problems))

    def test_a_long_held_alert_bridges_an_unmeasured_gauge_past_its_nan_seed(self):
        unmeasured = unmeasured_at_boot()
        self.assertIn("kinowo_worker_corpus_movies", unmeasured)  # the list is read
        # The positive control: the check names the last_over_time read, and not the NaN-skipping one.
        self.assertEqual(["kinowo_worker_corpus_movies"], last_over_time_reads(
            'max(last_over_time(kinowo_worker_corpus_movies{subset="total"}[10m])) > 0 '
            'and max_over_time(kinowo_worker_copied_feed_pairs[30m]) > 0', unmeasured))
        recordings = recording_rules()
        problems = []
        for alert, (rules, rule) in sorted(prometheus_alerts().items()):
            if seconds(rule.get("for")) < LONG_HOLD_SECONDS:
                continue
            exprs = [rule["expr"]] + [inner for _, inner in recorded_reads(rule["expr"], recordings)]
            for name in sorted({n for e in exprs for n in last_over_time_reads(e, unmeasured)}):
                problems.append("%s %s (for: %s) reads %s through last_over_time: a restart seeds it NaN, which "
                                "last_over_time returns and no comparison passes, so the hold starts over -- read it "
                                "through max_over_time / min_over_time, which skip NaN" % (rules, alert, rule.get("for"), name))
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


class EveryAppMetricAnAlertReadsIsWatchedForAbsence(unittest.TestCase):

    def test_the_generated_absent_alerts_are_current(self):
        stale = [os.path.relpath(p, HERE) for p in app_metrics_absent.stale()]
        self.assertEqual([], stale, "re-run `python3 infra/test/app_metrics_absent.py --write` and commit: "
                                    "an alert over an app metric changed since they were generated")

    def test_every_app_family_an_alert_reads_has_an_absence_alert(self):
        read = app_metrics_absent.read_families()
        self.assertIn("kinowo_worker_corpus_movies", read)  # the scan reaches the alerts
        watched = set(app_metrics_absent.generated_families()) | set(app_metrics_absent.hand_guarded())
        unwatched = ["%s, read by %s" % (f, ", ".join(readers)) for f, (_, readers) in sorted(read.items())
                     if f not in watched and f not in app_metrics_absent.NOT_GUARDED]
        self.assertEqual([], unwatched, "\n" + "\n".join(unwatched))

    def test_not_guarded_names_only_families_an_alert_reads(self):
        read = app_metrics_absent.read_families()
        self.assertEqual([], sorted(f for f in app_metrics_absent.NOT_GUARDED if f not in read))

    def test_the_hand_written_guards_are_recognised(self):
        guarded = app_metrics_absent.hand_guarded()
        self.assertEqual("CinemaScrapeCensusAbsent", guarded.get("kinowo_worker_cinema_scrape_oldest_age_seconds"))
        self.assertEqual("ShareCardCoverageAbsent", guarded.get("kinowo_worker_share_cards_coverage_ratio"))
        self.assertNotIn("kinowo_worker_corpus_movies", guarded)


if __name__ == "__main__":
    unittest.main(verbosity=2)
