#!/usr/bin/env python3
"""The offline half of bin/backtest-alerts: the decisions it makes about a series it was handed.

The script itself needs the fleet's Prometheus, which CI cannot reach. What it DECIDES from what it
fetches -- which identifiers in an expression are metrics, when a `for:` hold completes, what counts
as an episode, when a metric was rolled out rather than published intermittently, and which alerts
are noisy or dead -- is pure, and is pinned here so bin/check can run it offline.

Run: python3 infra/test/test_backtest_alerts.py
"""

import importlib.machinery
import importlib.util
import os
import unittest

HERE = os.path.dirname(os.path.abspath(__file__))
_PATH = os.path.join(os.path.dirname(HERE), "bin", "backtest-alerts")
_LOADER = importlib.machinery.SourceFileLoader("backtest_alerts", _PATH)
_SPEC = importlib.util.spec_from_loader("backtest_alerts", _LOADER)
bt = importlib.util.module_from_spec(_SPEC)
_LOADER.exec_module(bt)

BUDGET = {"days": 14, "max_firing_fraction": 0.05, "max_episodes_per_week": 7}


def grid(n, step=60):
    return [i * step for i in range(n)]


class MetricNames(unittest.TestCase):
    def test_reads_selectors_and_ignores_functions_keywords_and_labels(self):
        expr = ('sum by (country, service) (rate(kinowo_a_total{job="x", le=~"1|2"}[5m])) '
                '/ on (country) group_left(city) kinowo_b > 0.5 and on() up == 1 '
                'unless absent(kinowo_c offset 1h) or vector(0)')
        self.assertEqual(bt.metric_names(expr), {"kinowo_a_total", "kinowo_b", "up", "kinowo_c"})

    def test_a_name_matcher_is_a_metric_and_a_name_regex_is_not(self):
        self.assertEqual(bt.metric_names('{__name__="foo_total", job="a"} + {__name__=~"bar_.*"}'),
                         {"foo_total"})

    def test_strings_and_durations_are_not_metrics(self):
        expr = 'label_replace(up, "dst", "$1", "instance", "(.*):.*") and max_over_time(x[1h:5m])'
        self.assertEqual(bt.metric_names(expr), {"up", "x"})

    def test_words_in_a_promql_comment_are_not_metrics(self):
        # ReadModelServingDiffersFromCorpus explains its carve-out in `#` lines inside `expr`; the
        # 2026-09-25 run listed "Some", "city", "hours", "two"... as its missing metrics, which
        # would call the rule DEAD in any fortnight it stayed quiet. A `#` in a string is no comment.
        expr = ('up{job="a#b"}\n'
                '  # Some city\'s difference is not excused: two hours (the CAP)\n'
                '  and on (country) kinowo_c # trailing note_total\n')
        self.assertEqual(bt.metric_names(expr), {"up", "kinowo_c"})

    def test_the_flux_gauge_that_never_existed_is_named(self):
        # The dead rules FluxReconciliationFailing & co. selected this; the dead check needs it.
        expr = 'max by (kind, name) (gotk_reconcile_condition{type="Ready",status="False"} == 1)'
        self.assertEqual(bt.metric_names(expr), {"gotk_reconcile_condition"})


class ForHold(unittest.TestCase):
    def test_fires_only_once_the_run_is_as_old_as_the_hold(self):
        g = grid(10)
        present = set(g[2:8])  # 120 .. 420
        self.assertEqual(sorted(bt.firing_steps(present, g, hold=180)), [300, 360, 420])

    def test_a_gap_resets_the_hold(self):
        g = grid(12)
        present = set(g[0:4]) | set(g[5:12])
        # 0..180 is a run of 180s: fires at 180. The gap at 240 resets; 300..660 fires from 480.
        self.assertEqual(sorted(bt.firing_steps(present, g, hold=180)), [180, 480, 540, 600, 660])

    def test_zero_hold_fires_on_every_sample(self):
        g = grid(5)
        self.assertEqual(bt.firing_steps({60, 180}, g, hold=0), {60, 180})

    def test_keep_firing_for_bridges_a_short_gap(self):
        g = grid(8)
        present = {0, 60, 120, 300, 360}
        self.assertEqual(sorted(bt.firing_steps(present, g, hold=0, keep_firing_for=120)),
                         [0, 60, 120, 180, 240, 300, 360, 420])


class Summarise(unittest.TestCase):
    def test_counts_the_union_of_series_as_one_alert(self):
        g = grid(10)
        stats = bt.summarise({"a": {0, 60, 120}, "b": {120, 180}, "c": {480}}, g, 0, 0, 0)
        self.assertEqual(stats["episodes"], 2)  # 0..180 and 480
        self.assertEqual(stats["series_episodes"], 3)
        self.assertEqual(stats["peak_series"], 2)
        self.assertAlmostEqual(stats["firing_fraction"], 5 / 10.0)

    def test_the_lead_in_ages_the_hold_but_is_not_counted(self):
        g = grid(10)  # the window opens at 300
        stats = bt.summarise({"a": set(g)}, g, hold=240, keep_firing_for=0, window_start=300)
        self.assertAlmostEqual(stats["firing_fraction"], 1.0)

    def test_an_expected_incident_is_reported_but_excused_from_the_budget(self):
        g = grid(10)
        stats = bt.summarise({"a": set(g[0:6])}, g, 0, 0, 0, excused=[(0, 300)])
        self.assertAlmostEqual(stats["firing_fraction"], 0.6)
        self.assertEqual(stats["budget_firing_fraction"], 0.0)
        self.assertEqual(stats["budget_episodes"], 0)


class Rollout(unittest.TestCase):
    def test_a_metric_that_appears_and_stays_was_rolled_out(self):
        g = grid(100)
        self.assertEqual(bt.rollout_start(set(g[40:]), g), g[40])

    def test_a_metric_present_from_the_start_was_not(self):
        g = grid(100)
        self.assertIsNone(bt.rollout_start(set(g), g))

    def test_a_metric_published_intermittently_is_not_a_rollout(self):
        g = grid(100)
        self.assertIsNone(bt.rollout_start(set(g[40:45]) | set(g[80:85]), g))


def stats(fraction=0.0, episodes=0, returned=True, days=14):
    return {"firing_fraction": fraction, "episodes": episodes, "budget_firing_fraction": fraction,
            "budget_episodes": episodes, "returned_data": returned, "days": days}


class Verdict(unittest.TestCase):
    def test_the_mongod_no_primary_shape_is_noisy(self):
        status, problem = bt.verdict(stats(1.0, 1), BUDGET, {}, [], True)
        self.assertEqual(status, "noisy")
        self.assertIn("100.0%", problem)

    def test_the_image_policy_flapping_shape_is_noisy_by_episode_count(self):
        status, problem = bt.verdict(stats(0.04, 30), BUDGET, {}, [], True)
        self.assertEqual(status, "noisy")
        self.assertIn("30 distinct times", problem)

    def test_an_allowlisted_noisy_alert_does_not_fail(self):
        self.assertEqual(bt.verdict(stats(1.0, 1), BUDGET, {"noisy": True, "reason": "r"}, [], True),
                         ("noisy", None))

    def test_a_per_alert_budget_overrides_the_default(self):
        allow = {"max_firing_fraction": 0.2, "reason": "r"}
        self.assertEqual(bt.verdict(stats(0.1, 1), BUDGET, allow, [], True), ("ok", None))

    def test_no_data_over_a_missing_metric_is_dead(self):
        status, problem = bt.verdict(stats(returned=False), BUDGET, {}, ["gotk_reconcile_condition"],
                                     True)
        self.assertEqual(status, "dead")
        self.assertIn("gotk_reconcile_condition", problem)

    def test_no_data_over_present_metrics_is_merely_quiet(self):
        self.assertEqual(bt.verdict(stats(returned=False), BUDGET, {}, [], True), ("quiet", None))

    def test_an_undeployed_alert_is_not_called_dead(self):
        self.assertEqual(bt.verdict(stats(returned=False), BUDGET, {}, ["new_metric"], False),
                         ("not-deployed", None))

    def test_a_query_error_fails(self):
        status, problem = bt.verdict({"error": "boom", "returned_data": False}, BUDGET, {}, [], True)
        self.assertEqual(status, "error")


class GrafanaTranslation(unittest.TestCase):
    def rule(self, evaluator, no_data="OK"):
        return {"uid": "u", "condition": "C", "for": "5m", "noDataState": no_data, "data": [
            {"refId": "A", "model": {"expr": "min(up)", "instant": True}},
            {"refId": "C", "model": {"type": "threshold", "expression": "A",
                                     "conditions": [{"evaluator": evaluator}]}}]}

    def test_a_threshold_becomes_a_comparison(self):
        self.assertEqual(bt.grafana_as_prometheus(self.rule({"type": "gt", "params": [4]}), "f")["expr"],
                         "(min(up)) > 4.0")

    def test_no_data_alerting_also_fires_on_absence(self):
        rule = self.rule({"type": "lt", "params": [1]}, no_data="Alerting")
        self.assertEqual(bt.grafana_as_prometheus(rule, "f")["expr"],
                         "((min(up)) < 1.0) or absent(min(up))")

    def test_every_rule_in_the_tree_loads(self):
        # A shape the translation refuses is a loud error here, not a silent skip at run time.
        alerts = bt.load_rules()
        self.assertTrue(any(a.get("grafana") for a in alerts.values()))
        self.assertIn("MongodNoPrimary", alerts)


if __name__ == "__main__":
    unittest.main(verbosity=2)
