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
import json
import os
import shutil
import subprocess
import tempfile
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

    # Alert backtest #1 (run 44404703989): CoreDnsMetricsAbsent and IdentityRetainedSizeAbsent "fired" until the
    # SELECTOR they read appeared (11-12, 11-07) and production never fired them: their metric NAMES were present
    # from the start under other labels (another job, other collections), so a rollout read by name alone missed it.
    def test_a_selector_the_rule_reads_is_watched_for_a_rollout_beside_its_bare_name(self):
        expr = ('absent(coredns_dns_responses_total{job="coredns"})\n or\n'
                ' absent(coredns_proxy_request_duration_seconds_count{job="coredns"})')
        self.assertEqual(bt.rollout_queries(expr, {"coredns_dns_responses_total",
                                                   "coredns_proxy_request_duration_seconds_count"}, set()),
                         {"coredns_dns_responses_total", "coredns_proxy_request_duration_seconds_count",
                          'coredns_dns_responses_total{job="coredns"}',
                          'coredns_proxy_request_duration_seconds_count{job="coredns"}'})

    def test_a_selector_rolled_out_under_a_name_present_all_along_is_found(self):
        start, step = 0, 900
        end = start + 99 * step
        g = bt.grid_of(start, end, step)
        asked = []

        class Prom(object):
            def presence(self, expr, start, end, step, chunk=86400):
                asked.append(expr)
                late = 'collection="identity_retained"' in expr
                return {"k": set(g[40:]) if late else set(g)}
        selector = 'mongodb_collstats_storageStats_storageSize{collection="identity_retained"}'
        rollouts = bt.metric_rollouts(Prom(), {"mongodb_collstats_storageStats_storageSize", selector}, start, end, 2)
        self.assertEqual(rollouts, {selector: g[40]})
        self.assertIn("count(%s)" % selector, asked)


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
        self.assertEqual("5m", alerts["CityDarkInBothTiers"]["interval"])


class EvaluationStep(unittest.TestCase):
    class Recorder(object):
        def __init__(self):
            self.steps = []

        def presence(self, expr, start, end, step):
            self.steps.append(step)
            return {}

    def test_a_rule_is_asked_once_per_group_interval_not_more_often(self):
        prom = self.Recorder()
        bt.backtest(prom, {"expr": "x", "interval": "5m"}, 3600, 7200, 60, None)
        bt.backtest(prom, {"expr": "x", "interval": "30s"}, 3600, 7200, 60, None)
        bt.backtest(prom, {"expr": "x"}, 3600, 7200, 60, None)
        self.assertEqual([300, 60, 60], prom.steps)


class RecordingRuleExpansion(unittest.TestCase):
    """An alert over a recording rule reads what Prometheus RECORDED, i.e. the deployed rule. A
    recording changed in this tree is backtested as written by substituting its expression."""

    RECORDS = {
        "ci:x:baseline": {"expr": "quantile_over_time(0.5, x[7d] offset 2d)", "interval": "5m"},
        "ci:x:threshold": {"expr": "ci:x:baseline * 1.4", "interval": "5m"},
        "ci:x:recent": {"expr": "quantile_over_time(0.5, x[2d])", "interval": "5m"},
    }

    def test_a_changed_recording_is_inlined_and_an_unchanged_one_is_read(self):
        out = bt.expand_recordings("ci:x:recent > ci:x:threshold", self.RECORDS, {"ci:x:recent"})
        self.assertEqual(out, "(quantile_over_time(0.5, x[2d])) > ci:x:threshold")

    def test_a_recording_over_a_changed_one_is_stale_too(self):
        stale = bt.stale_recordings(self.RECORDS, {"ci:x:baseline": "changed", "ci:x:threshold": "same",
                                                   "ci:x:recent": "same"},
                                    lambda name, expr: "same" if name != "ci:x:baseline" else expr)
        self.assertEqual(stale, {"ci:x:baseline", "ci:x:threshold"})

    def test_a_recording_prometheus_has_not_loaded_is_stale(self):
        stale = bt.stale_recordings(self.RECORDS, {"ci:x:recent": "same", "ci:x:threshold": "same"},
                                    lambda name, expr: "same")
        self.assertEqual(stale, {"ci:x:baseline", "ci:x:threshold"})

    def test_a_loaded_recording_with_no_series_in_the_window_is_stale(self):
        # Loaded now but recorded nothing in the window (deployed after the copy ends, 2026-11-18:
        # country_city:worker_movies_served:min2h read as DEAD over a copy ending at midnight): what it
        # would have recorded is its expression, so it is inlined rather than read as absent.
        loaded = {name: "same" for name in self.RECORDS}
        stale = bt.stale_recordings(self.RECORDS, loaded, lambda name, expr: "same",
                                    recorded={"ci:x:baseline", "ci:x:threshold"})
        self.assertEqual(stale, {"ci:x:recent"})

    def test_nested_recordings_expand_all_the_way_down(self):
        out = bt.expand_recordings("ci:x:threshold", self.RECORDS, {"ci:x:baseline", "ci:x:threshold"})
        self.assertEqual(out, "((quantile_over_time(0.5, x[7d] offset 2d)) * 1.4)")

    def test_an_offset_reads_the_recording_as_it_was_then_through_a_subquery(self):
        out = bt.expand_recordings("ci:x:recent offset 1d > 3", self.RECORDS, {"ci:x:recent"})
        self.assertEqual(out, "last_over_time((quantile_over_time(0.5, x[2d]))[5m:5m] offset 1d) > 3")

    def test_a_range_over_a_recording_reads_what_it_would_have_recorded_through_a_subquery(self):
        out = bt.expand_recordings("max_over_time(ci:x:recent[21h55m] offset 10m) >= 4", self.RECORDS,
                                   {"ci:x:recent"})
        self.assertEqual(out, "max_over_time((quantile_over_time(0.5, x[2d]))[21h55m:5m] offset 10m) >= 4")
        out = bt.expand_recordings("min_over_time(ci:x:recent[1h])", self.RECORDS, {"ci:x:recent"})
        self.assertEqual(out, "min_over_time((quantile_over_time(0.5, x[2d]))[1h:5m])")

    def test_a_name_inside_a_longer_name_or_a_string_is_left_alone(self):
        records = {"ci:x": {"expr": "up", "interval": "1m"}}
        self.assertEqual(bt.expand_recordings('ci:x:other + ci:xy{a="ci:x"}', records, {"ci:x"}),
                         'ci:x:other + ci:xy{a="ci:x"}')

    def test_matchers_on_a_changed_recording_are_refused(self):
        with self.assertRaises(ValueError):
            bt.expand_recordings('ci:x:recent{window="2d"}', self.RECORDS, {"ci:x:recent"})

    def test_the_tree_s_recordings_load_with_their_group_interval(self):
        records = bt.load_recordings()
        self.assertEqual(records["ci:job_seconds:baseline"]["interval"], "5m")


class ChunkSplitting(unittest.TestCase):
    """A query_range chunk Prometheus cannot evaluate inside its query timeout is halved and both
    halves asked again, down to a floor -- not retried whole (which times out the same way) and
    not dropped as an `error` row, which is how ten heavy rules sat in the 2026-11-18 baseline."""

    STEP = 60

    def fake(self, max_span, calls):
        """A query_range that times out on any span over `max_span` seconds, else returns one
        sample per step of one series."""
        def get_chunk(start, end):
            calls.append((start, end))
            if end - start > max_span:
                raise bt.QueryTimeout("query timed out in expression evaluation")
            return [{"metric": {"a": "1"}, "values": [[t, "1"] for t in range(start, end + 1, self.STEP)]}]
        return get_chunk

    def test_a_chunk_that_times_out_is_halved_until_it_answers_and_nothing_is_lost(self):
        calls = []
        day = 86400
        series = bt.fetch_range(self.fake(day // 4, calls), 0, day - self.STEP, self.STEP, chunk=day, min_chunk=3600)
        self.assertEqual(series['{"a": "1"}'], set(range(0, day, self.STEP)))
        self.assertEqual(calls[0], (0, day - self.STEP))             # the whole day first
        answered = [c for c in calls if c[1] - c[0] <= day // 4]
        self.assertEqual(len(answered), 4)                            # then four quarters
        self.assertEqual(sorted(answered)[0][0], 0)
        self.assertEqual(sorted(answered)[-1][1], day - self.STEP)

    def test_split_points_stay_on_the_step_grid(self):
        calls = []
        bt.fetch_range(self.fake(7 * 3600, calls), 0, 86400 - self.STEP, self.STEP, chunk=86400, min_chunk=3600)
        for start, end in calls:
            self.assertEqual(start % self.STEP, 0)
            self.assertEqual(end % self.STEP, 0)

    def test_a_chunk_still_timing_out_at_the_floor_raises(self):
        with self.assertRaises(bt.QueryTimeout):
            bt.fetch_range(self.fake(60, []), 0, 86400 - self.STEP, self.STEP, chunk=86400, min_chunk=3600)

    def test_a_query_that_answers_is_asked_once_per_chunk(self):
        calls = []
        bt.fetch_range(self.fake(10 ** 9, calls), 0, 2 * 86400 - self.STEP, self.STEP, chunk=86400, min_chunk=3600)
        self.assertEqual(calls, [(0, 86400 - self.STEP), (86400, 2 * 86400 - self.STEP)])

    def test_timeouts_are_told_apart_from_other_errors(self):
        for message in ("query timed out in expression evaluation", "expanding series: context deadline exceeded",
                        "curl: (28) Operation timed out after 180002 milliseconds with 0 bytes received"):
            self.assertTrue(bt.is_query_timeout(message), message)
        for message in ("curl: (97) connection to proxy closed", "parse error at char 3", "exceeded maximum resolution"):
            self.assertFalse(bt.is_query_timeout(message), message)


RULES_DOCUMENT = {"status": "success", "data": {"groups": [
    {"name": "a", "rules": [
        {"type": "alerting", "name": "HostDown", "query": "up == 0"},
        {"type": "recording", "name": "job:up:sum", "query": "sum by (job) (up)"}]},
    {"name": "b", "rules": [{"type": "alerting", "name": "DiskFull", "query": "disk > 0.9"}]}]}}


def write_snapshot(directory, version="3.12.0"):
    os.makedirs(os.path.join(directory, "tsdb"))
    for name, body in (("rules.json", RULES_DOCUMENT),
                       ("buildinfo.json", {"status": "success", "data": {"version": version}}),
                       ("pull.json", {"pulled_at": 1795000000, "blocks": [],
                                      "min_time": 1793700000.0, "max_time": 1794990000.0})):
        with open(os.path.join(directory, name), "w") as handle:
            json.dump(body, handle)


class SnapshotSource(unittest.TestCase):
    """--source snapshot reads what PRODUCTION had loaded, and the window its copy covers -- never the
    replay's own rules (it loads none) or the wall clock."""

    def setUp(self):
        self.work = tempfile.mkdtemp()
        write_snapshot(self.work)
        self.snapshot = bt.Snapshot(self.work)

    def tearDown(self):
        shutil.rmtree(self.work)

    def test_loaded_rules_are_the_captured_ones(self):
        self.assertEqual(self.snapshot.loaded_alerts(), {"HostDown", "DiskFull"})
        self.assertEqual(self.snapshot.loaded_recordings(), {"job:up:sum": "sum by (job) (up)"})

    def test_the_window_is_the_copy_s_range(self):
        self.assertEqual((self.snapshot.floor(), self.snapshot.latest()), (1793700000.0, 1794990000.0))
        self.assertEqual(self.snapshot.version, "3.12.0")

    def test_the_replay_never_rewrites_or_retires_a_copied_block(self):
        arguments = bt.replay_arguments("prometheus", "/c.yml", "/data", 9999, 16)
        self.assertIn("--storage.tsdb.min-block-duration=2h", arguments)
        self.assertIn("--storage.tsdb.max-block-duration=2h", arguments)
        self.assertIn("--storage.tsdb.retention.time=3650d", arguments)
        self.assertIn("--web.listen-address=127.0.0.1:9999", arguments)
        self.assertIn("--query.max-concurrency=16", arguments)


class SourceSwitch(unittest.TestCase):
    def setUp(self):
        self.constructed = []
        test = self

        class Unreachable(object):
            def __init__(self, url, socks):
                test.constructed.append((url, socks))

            def ready(self):
                return False
        self.real = bt.Prometheus
        bt.Prometheus = Unreachable

    def tearDown(self):
        bt.Prometheus = self.real

    def test_the_default_is_the_snapshot_and_it_never_asks_production(self):
        missing = os.path.join(tempfile.gettempdir(), "no-such-backtest-snapshot")
        self.assertEqual(bt.main(["--snapshot", missing]), 2)
        self.assertEqual(self.constructed, [])

    def test_live_asks_production_through_the_tunnel(self):
        self.assertEqual(bt.main(["--source", "live", "--prometheus", "http://p:9090", "--socks", "s:1"]), 2)
        self.assertEqual(self.constructed, [("http://p:9090", "s:1")])


class IncidentsEndWithTheHistory(unittest.TestCase):
    def test_an_incident_window_is_clipped_to_the_end_of_the_history_not_the_wall_clock(self):
        asked = []

        class Recording(object):
            def presence(self, expr, start, end, step, chunk=86400):
                asked.append(end)
                return {}
        catalogue = [{"id": "i", "start": "2026-11-01T00:00:00Z", "end": "2026-11-30T00:00:00Z",
                      "expect_quiet": ["A"]}]
        end = bt.parse_time("2026-11-10T00:00:00Z")
        bt.check_incidents(Recording(), {"A": {"expr": "x", "for": "0s"}}, catalogue, 60, None, None, end)
        self.assertEqual(asked, [end - 60])

    def test_an_incident_before_the_retained_history_is_skipped_and_reported_not_failed(self):
        # Alert backtest #1 (run 44404703989): 29 September incidents of the catalogue start before the
        # snapshot's retention, and each failed the run, so the check could never stay green as incidents age.
        # The owner (2026-11-18): skip and report them, never prune the catalogue.
        asked = []

        class Recording(object):
            def presence(self, expr, start, end, step, chunk=86400):
                asked.append(start)
                return {}
        catalogue = [{"id": "old", "start": "2026-09-01T00:00:00Z", "end": "2026-09-02T00:00:00Z",
                      "expect_firing": ["A"]}]
        floor, end = bt.parse_time("2026-10-30T06:00:00Z"), bt.parse_time("2026-11-18T00:00:00Z")
        results, problems = bt.check_incidents(Recording(), {"A": {"expr": "x", "for": "0s"}}, catalogue, 60,
                                               floor, None, end)
        self.assertEqual(problems, [])
        self.assertEqual(results, [{"id": "old", "skipped": "starts before retention (2026-10-30T06:00:00Z)"}])
        self.assertEqual(asked, [])


@unittest.skipUnless(os.environ.get("BACKTEST_PROMETHEUS_BINARY"),
                     "set BACKTEST_PROMETHEUS_BINARY to a Prometheus to start a real replay")
class RealReplay(unittest.TestCase):
    """The replay against a real Prometheus binary: it starts over a private copy, refuses another
    version than production's, and leaves nothing behind."""

    def setUp(self):
        self.work = tempfile.mkdtemp()
        self.binary = os.environ["BACKTEST_PROMETHEUS_BINARY"]
        self.version = subprocess.run([self.binary, "--version"], capture_output=True, text=True) \
            .stdout.split()[2]

    def tearDown(self):
        shutil.rmtree(self.work)

    def test_it_answers_and_tears_down(self):
        write_snapshot(self.work, self.version)
        replay = bt.Replay(bt.Snapshot(self.work), self.binary, 4)
        with replay as prom:
            self.assertTrue(prom.ready())
        self.assertIsNotNone(replay.process.poll())
        self.assertFalse(os.path.exists(replay.work))
        self.assertEqual(os.listdir(os.path.join(self.work, "tsdb")), [], "the snapshot was written to")

    def test_another_version_than_production_s_is_refused(self):
        write_snapshot(self.work, "2.0.0")
        with self.assertRaises(RuntimeError):
            with bt.Replay(bt.Snapshot(self.work), self.binary, 4):
                pass


if __name__ == "__main__":
    unittest.main(verbosity=2)
