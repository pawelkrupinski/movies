#!/usr/bin/env python3
"""ci_trends.py over hand-made runs: clock skew, the critical path, saturation, categories, metrics artifacts, the
VictoriaLogs event round trip, bucketing, trends, regression flags with their push window and machine shift, grouping
by leg and machine, and the collector's idempotent export.
Run: python3 scripts/ci/ci_trends_test.py"""
import io
import json
import os
import sys
import tempfile
import time
import unittest
import unittest.mock

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import ci_trends as t  # noqa: E402

T0 = t.epoch("2026-11-01T10:00:00Z")


def at(seconds):
    return t.iso(T0 + seconds)


def api_job(name, start, end, created=None, skew=0, job_id=None, runner=None, steps=None, conclusion="success"):
    """An API job whose steps run start..end (seconds after T0); `skew` shifts GitHub's own job timestamps."""
    steps = steps or [("Set up job", 0, 2), ("Run actions/checkout@v5", 2, 12), ("Run sbt test", 12, end - start)]
    return {"id": job_id or hash(name) % 100000, "name": name, "conclusion": conclusion, "runner_name": runner,
            "created_at": at((created if created is not None else start) + skew), "started_at": at(start + skew),
            "completed_at": at(end + skew),
            "steps": [{"name": n, "started_at": at(start + a), "completed_at": at(start + b), "conclusion": "success"}
                      for n, a, b in steps]}


def api_run(run_id, jobs, sha="abc", start_offset=0, created_offset=0):
    run = {"id": run_id, "name": "Main", "path": ".github/workflows/main.yml", "run_number": run_id,
           "run_attempt": 1, "head_branch": "main", "head_sha": sha, "event": "push", "conclusion": "success",
           "status": "completed", "created_at": at(start_offset + created_offset),
           "repository": {"full_name": "o/r"}}
    return t.run_record(run, jobs)


def simple_run(run_id, offset, duration=300, sha=None, machine=None):
    jobs = [api_job("ci / test (rest)", offset, offset + duration, job_id=run_id * 10 + 1, runner="r%d" % run_id),
            api_job("publish", offset + duration + 30, offset + duration + 40, created=offset + duration,
                    job_id=run_id * 10 + 2)]
    record = api_run(run_id, jobs, sha or "sha%d" % run_id, offset)
    if machine:
        for job in record["jobs"]:
            job["machine"] = {"cpu_model": machine, "image_version": machine + "-img"}
    return record


class SkewTest(unittest.TestCase):
    def test_no_skew_when_job_and_steps_agree(self):
        self.assertEqual(0, t.skew_of(api_job("a", 0, 100)))

    def test_an_hour_ahead_is_detected_and_taken_off_the_queue(self):
        job = api_job("a", 0, 100, created=-30, skew=3600)
        self.assertEqual(3600, t.skew_of(job))
        record = t.job_record(job)
        self.assertAlmostEqual(30.0, record["s"] - record["r"], places=0)

    def test_a_created_at_after_the_first_step_is_impossible_and_corrected(self):
        job = api_job("a", 0, 100)
        job["created_at"] = at(3600 - 20)
        self.assertEqual(3600, t.skew_of(job))

    def test_a_small_difference_is_not_skew(self):
        job = api_job("a", 0, 100)
        job["started_at"] = at(90)
        self.assertEqual(0, t.skew_of(job))


class RunMeasuresTest(unittest.TestCase):
    def setUp(self):
        self.record = api_run(1, [
            api_job("ci / scope", 0, 10, job_id=1),
            api_job("ci / test (rest)", 0, 200, job_id=2),
            api_job("ci / page tests (safari / pw phones 1/2)", 70, 600, created=15, job_id=3),
            api_job("publish-web", 660, 670, created=600, job_id=4),
            api_job("timings / report (timings)", 700, 720, created=670, job_id=5),
        ])

    def test_wall_and_peak(self):
        m = t.run_measures(self.record)
        self.assertEqual(720, m["wall"])
        self.assertEqual(2, m["peak"])

    def test_the_critical_path_walks_back_through_queue_and_work(self):
        path = t.critical_path(self.record)
        self.assertEqual(["ci / scope", "ci / page tests (safari / pw phones 1/2)", "publish-web",
                          "timings / report (timings)"], [hop[0] for hop in path])
        self.assertEqual([0, 55, 60, 30], [round(hop[1]) for hop in path])

    def test_queue_wait_beside_execution_and_wall(self):
        m = t.run_measures(self.record)
        self.assertEqual(55 + 60 + 30, round(m["queue_total"]))  # every job's queue, not only the critical path's
        self.assertEqual((145, 574), (round(m["path_queue"]), round(m["path_work"])))
        self.assertEqual(720, m["elapsed"])
        early = api_run(2, [api_job("check (rest)", 0, 100, created=0)], created_offset=-30)
        self.assertEqual((100, 130), (t.run_measures(early)["wall"], t.run_measures(early)["elapsed"]))

    def test_the_per_run_table_and_summary_show_the_queue(self):
        summary = t.summarize([t.run_measures(self.record)])
        self.assertEqual(145, round(summary["queue_total"]["p50"]))
        text = t.format_report(summary, "precommit-check.yml", runs=True)
        self.assertIn("every job's queue summed p50 2:25", text)
        row = [line for line in text.splitlines() if line.strip().startswith("1 ")][0]
        cells = row.split()  # id, sha, change, date, time, wall, created→end, path queue, exec, all queues
        self.assertEqual(("12:00", "12:00", "2:25", "2:25"), (cells[5], cells[6], cells[7], cells[9]))
        self.assertIn(("run: all-jobs queue p50", 145.0), [(n, round(v, 1)) for n, v in t.metric_rows(summary)
                                                            if n == "run: all-jobs queue p50"])

    def test_change_type_keeps_runs_whole(self):
        other = api_run(2, [api_job("check (rest)", 0, 100), api_job("plan", 0, 10)])
        identity = api_run(3, [api_job("check (identity 1/3)", 0, 100), api_job("check (rest)", 0, 50)])
        parts = t.partition([self.record, other, identity], "change")
        self.assertEqual({"identity": [3], "other": [1, 2]}, {k: [r["id"] for r in v] for k, v in parts.items()})
        self.assertEqual(2, len(parts["identity"][0]["jobs"]))

    def test_categories_split_setup_from_work(self):
        job = self.record["jobs"][1]
        categories = t.job_categories(job)
        self.assertEqual(10, categories["checkout"])
        self.assertEqual(188, categories["tests"])

    def test_groups_and_legs(self):
        self.assertEqual("page tests", t.job_group("ci / page tests (safari / pw phones 1/2)"))
        self.assertEqual("takes-drift-1", t.job_group("leg (germany) / takes-drift-1 (germany)"))
        self.assertEqual("germany", t.job_leg("leg (germany) / takes-drift-1 (germany)"))
        self.assertEqual("webkit", t.job_leg("ci / x", "webkit"))
        self.assertIsNone(t.job_leg("ci / x"))


class CategoryTest(unittest.TestCase):
    def test_categories(self):
        cases = {"Run actions/checkout@v5": "checkout", "Run ./.github/actions/setup-sbt-build": "toolchain/cache",
                 "Run sbt testUnitWorker": "tests", "Start MongoDB (single-node replica set, for change streams)":
                     "services", 'Run sbt "web/stage" "worker/stage"': "compile",
                 "Run actions/upload-artifact@v6": "publish", "Set up job": "runner",
                 "Post Run ./.github/actions/setup-sbt-build": "toolchain/cache",
                 "Run ./.github/actions/run-page-test": "tests"}
        for name, category in cases.items():
            self.assertEqual(category, t.categorize(name), name)


class SaturationTest(unittest.TestCase):
    def test_mean_peak_and_full_share(self):
        intervals = [(0, 100)] * 20 + [(0, 50)] * 5
        figures = t.saturation((0, 100), intervals, slots=20, every=10)
        self.assertEqual(25, figures["peak"])
        self.assertEqual(1.0, figures["full"])

    def test_idle(self):
        self.assertEqual(0, t.saturation((0, 100), [(200, 300)], every=10)["peak"])


class MetricsTest(unittest.TestCase):
    def lines(self):
        columns = ["t", "cpu_total", "cpu_busy", "cpu_user", "cpu_sys", "cpu_iowait", "cpu_steal", "load1",
                   "procs_running", "mem_total_kb", "mem_avail_kb", "swap_total_kb", "swap_free_kb",
                   "psi_mem_some_us", "psi_mem_full_us"]
        rows = [[T0 + i * 5, 2000 * i, 1900 * i, 1800 * i, 100 * i, 0, 0, 3.9, 4, 16e6, 4e6, 0, 0, i * 5e6 * 0.2,
                 i * 5e6 * 0.05] for i in range(0, 61)]
        return [{"kind": "machine", "runner_name": "r1", "cpu_model": "AMD EPYC 7763", "cores": 4,
                 "image_version": "20251020.1"},
                {"kind": "job_start", "runner_name": "r1", "label": "webkit", "rate_remaining": 900},
                {"kind": "series", "columns": columns, "rows": rows},
                {"kind": "procs", "columns": ["t", "comm", "procs", "rss_kb", "ticks", "threads"],
                 "rows": [[T0, "java", 2, 2048000, 100, 80], [T0 + 5, "java", 2, 3072000, 600, 90]]},
                {"kind": "jvm", "pid": "77", "main": "xsbt.boot.Boot", "gc_count": 40, "gc_pause_ms": 60000.0,
                 "gc_max_pause_ms": 900.0, "heap_after_max_mb": 3000, "uptime_s": 290.0, "gc_overhead": 0.2},
                {"kind": "cache", "cache": "sbt-target", "status": "partial", "key": "sbt-target-unit-rest-Linux-"
                 + "a" * 64, "size_mb": 900, "restore_s": 12.0},
                {"kind": "tests", "tests": 100, "failed": 0, "skipped": 3, "time_s": 250.0},
                {"kind": "test", "suite": "S", "test": "slow one", "time_s": 30.0, "status": "passed"},
                {"kind": "rate_limit", "remaining": 880}]

    def record(self):
        run = {"id": 9, "name": "Main", "path": ".github/workflows/main.yml", "run_attempt": 1, "head_sha": "s",
               "status": "completed", "conclusion": "success", "created_at": at(0), "head_branch": "main"}
        return t.run_record(run, [api_job("ci / test (rest)", 0, 300, runner="r1", job_id=91)],
                            {"r1": self.lines()})

    def test_metrics_join_by_runner_name(self):
        job = self.record()["jobs"][0]
        self.assertEqual("AMD EPYC 7763", job["machine"]["cpu_model"])
        self.assertEqual("webkit", job["label"])
        self.assertEqual({"start": 900, "end": 880}, job["rate"])
        self.assertEqual("partial", job["cache"]["sbt-target"]["status"])

    def test_one_runner_serving_several_jobs_gives_each_its_own_metrics(self):
        def lines(label, start):
            return [{"kind": "job_start", "runner_name": "r1", "label": label, "t": T0 + start}]
        run = {"id": 9, "name": "Landing", "run_attempt": 1, "status": "completed", "conclusion": "success",
               "created_at": at(0)}
        jobs = [api_job("plan", 0, 30, runner="r1", job_id=1), api_job("heavy rows", 40, 300, runner="r1", job_id=2),
                api_job("check (identity 1/3)", 310, 900, runner="r1", job_id=3)]
        record = t.run_record(run, jobs, {"ci-metrics-heavy-r1": lines("heavy", 45),
                                          "ci-metrics-check-identity-r1": lines("identity", 315)})
        self.assertEqual([None, "heavy", "identity"], [job.get("label") for job in record["jobs"]])

    def test_resources_gc_and_the_bound(self):
        job = self.record()["jobs"][0]
        resources, steps = t.job_resources(job)
        self.assertAlmostEqual(0.95, resources["cpu"], places=2)
        self.assertAlmostEqual(0.05, resources["psi_mem_full"], places=2)
        self.assertIn("Run sbt test", steps)
        gc = t.gc_summary(job)
        self.assertEqual(0.2, gc["of_wall"])
        bound, notes = t.bound_by(resources, gc)
        self.assertEqual("memory/GC", bound)
        self.assertIn("GC 20% of wall", notes)
        self.assertTrue(any(n.startswith("PSI mem full 5%") for n in notes))

    def test_top_processes(self):
        procs = t.top_processes(self.record()["jobs"][0])
        self.assertEqual({"comm": "java", "rss_mb": 3000, "cpu_s": 5.0, "threads": 90, "procs": 2}, procs[0])

    def test_events_round_trip_through_victorialogs_strings(self):
        record = self.record()
        record["sat"] = {"mean": 18.5, "peak": 20, "full": 0.4}
        events = t.events_of(record)
        kinds = {e["kind"] for e in events}
        self.assertTrue({"run", "job", "step", "jvm", "cache", "test", "process"} <= kinds, kinds)
        job = next(e for e in events if e["kind"] == "job")
        self.assertEqual("AMD EPYC 7763", job["machine_cpu_model"])
        self.assertEqual(0.2, job["gc_of_wall"])
        self.assertEqual(100, job["tests_tests"])
        self.assertEqual("memory/GC", job["bound"])
        self.assertTrue(all(e["source"] == "ci" and e["_time"] and e["_msg"] for e in events))
        cache = next(e for e in events if e["kind"] == "cache")
        self.assertEqual("sbt-target-unit-rest-Linux", cache["family"])
        # VictoriaLogs hands every field back as a string.
        stringly = [{k: (str(v) if v is not None else "") for k, v in e.items()} for e in events]
        rebuilt = t.records_from_events(stringly)
        self.assertEqual(1, len(rebuilt))
        before, after = t.run_measures(record), t.run_measures(rebuilt[0])
        self.assertEqual(before["wall"], after["wall"])
        self.assertEqual(before["categories"], after["categories"])
        self.assertEqual(round(before["jobs"]["ci / test (rest)"][0]["gc"]["of_wall"], 3),
                         after["jobs"]["ci / test (rest)"][0]["gc"]["of_wall"])
        self.assertEqual(18.5, rebuilt[0]["sat"]["mean"])


class BucketTest(unittest.TestCase):
    def test_day_hour_week_boundaries(self):
        moment = t.epoch("2026-11-04T13:45:00Z")  # a Wednesday
        self.assertEqual(t.epoch("2026-11-04T00:00:00Z"), t.bucket_start(moment, "day"))
        self.assertEqual(t.epoch("2026-11-04T13:00:00Z"), t.bucket_start(moment, "hour"))
        self.assertEqual(t.epoch("2026-11-02T00:00:00Z"), t.bucket_start(moment, "week"))

    def test_empty_buckets_are_kept_in_order(self):
        runs = [t.run_measures(simple_run(1, 0)), t.run_measures(simple_run(2, 2 * 86400))]
        grouped = t.buckets(runs, "day")
        self.assertEqual([1, 0, 1], [len(ms) for _, ms in grouped])

    def test_relative_times(self):
        now = T0
        self.assertEqual(now - 7 * 86400, t.parse_when("7d", now))
        self.assertEqual(now - 3600, t.parse_when("1h", now))
        self.assertEqual(t.epoch("2026-10-01T00:00:00Z"), t.parse_when("2026-10-01", now))


class TrendTest(unittest.TestCase):
    def test_sparkline_slope_and_change(self):
        self.assertEqual("▁▅█", t.sparkline([1, 2, 3]))
        self.assertEqual(" ▁█", t.sparkline([None, 1, 3]))
        self.assertEqual(1.0, t.slope([1, 2, 3]))
        self.assertEqual(0.5, t.change(2, 3))

    def test_a_regression_is_flagged_with_its_push_window_and_machine_shift(self):
        previous = [t.run_measures(simple_run(i, -86400 * 3 + i * 3600, 300, machine="EPYC 7763")) for i in range(1, 7)]
        current = [t.run_measures(simple_run(10 + i, i * 3600, 300 if i < 3 else 420,
                                             machine="EPYC 7763" if i < 3 else "Xeon 8370C")) for i in range(0, 7)]
        result = t.trend(current, "day", T0, T0 + 86400, 0.2, previous)
        flagged = {f["row"]: f for f in result["regressions"]}
        self.assertIn("job: ci / test (rest) p50", flagged)
        flag = flagged["job: ci / test (rest) p50"]
        self.assertAlmostEqual(0.4, flag["change"], places=2)
        self.assertEqual("sha12", flag["between"]["last_before"]["sha"])
        self.assertEqual("sha13", flag["between"]["first_after"]["sha"])
        self.assertIn("cpu_model EPYC 7763 → Xeon 8370C", flag["machine_shift"])
        text = t.format_trend(result)
        self.assertIn("REGRESSIONS", text)
        self.assertIn("lines up with a machine change", text)

    def test_no_flag_under_the_threshold(self):
        previous = [t.run_measures(simple_run(i, -86400 + i * 600, 300)) for i in range(1, 6)]
        current = [t.run_measures(simple_run(10 + i, i * 600, 330)) for i in range(1, 6)]
        self.assertEqual([], [f for f in t.trend(current, "day", T0, T0 + 86400, 0.2, previous)["regressions"]
                              if f["row"].startswith("job: ci / test")])

    def test_a_small_absolute_rise_is_not_a_regression(self):
        previous = [simple_run(i, i * 600, 5) for i in range(1, 6)]
        current = [simple_run(10 + i, i * 600, 8) for i in range(1, 6)]
        rows = [f[0] for f in t.regressions([t.run_measures(r) for r in current], [t.run_measures(r) for r in previous], 0.2)]
        self.assertNotIn("job: ci / test (rest) p50", rows)


class GroupingTest(unittest.TestCase):
    def test_partition_by_cpu_model_and_leg(self):
        a = simple_run(1, 0, machine="EPYC")
        b = simple_run(2, 600, machine="Xeon")
        parts = t.partition([a, b], "cpu-model")
        self.assertEqual(["EPYC", "Xeon"], sorted(parts))
        legs = t.partition([api_run(3, [api_job("leg (spain) / takes (spain)", 0, 60, job_id=1),
                                        api_job("leg (poland) / takes (poland)", 0, 90, job_id=2)])], "leg")
        self.assertEqual(["poland", "spain"], sorted(legs))

    def test_leg_tables(self):
        record = api_run(3, [api_job("leg (spain) / takes (spain)", 0, 60, job_id=1),
                             api_job("leg (spain) / convergence (spain)", 10, 400, job_id=2),
                             api_job("leg (poland) / takes (poland)", 0, 90, job_id=3),
                             api_job("timings / report (timings)", 400, 420, job_id=4)])
        text = t.leg_tables(record)
        self.assertIn("#### spain — wall 6:40, critical row: leg (spain) / convergence (spain)", text)
        self.assertIn("#### poland", text)
        self.assertNotIn("timings", text)


class RedirectTest(unittest.TestCase):
    def test_the_token_never_follows_a_redirect_off_the_api(self):
        import urllib.request
        handler = t._DropAuthOnRedirect()
        request = urllib.request.Request("https://api.github.com/repos/o/r/actions/artifacts/1/zip",
                                         headers={"Authorization": "Bearer x"})
        moved = handler.redirect_request(request, None, 302, "Found", {}, "https://blob.core.windows.net/a?sig=1")
        self.assertNotIn("Authorization", dict(moved.header_items()))
        same = handler.redirect_request(request, None, 302, "Found", {}, "https://api.github.com/other")
        self.assertIn("Authorization", dict(same.header_items()))


class ExportTest(unittest.TestCase):
    def test_idempotent_and_resumable(self):
        runs = [{"id": 1, "run_attempt": 1, "status": "completed", "conclusion": "success", "created_at": at(0),
                 "name": "Main", "path": ".github/workflows/main.yml", "head_branch": "main", "head_sha": "a"},
                {"id": 2, "run_attempt": 1, "status": "completed", "conclusion": "failure", "created_at": at(600),
                 "name": "Main", "path": ".github/workflows/main.yml", "head_branch": "main", "head_sha": "b"}]

        class Fake:
            repo = "o/r"
            calls = 0

            def runs(self, **_):
                return runs

            def jobs(self, run):
                return [api_job("j", run["id"] * 600, run["id"] * 600 + 60, job_id=run["id"])]

            def metrics(self, run):
                return {}

        posted, failed = [], []

        def post(url, events):
            if events[0]["run_id"] == 2 and not failed:
                failed.append(2)
                posted.append("fail-once")
                raise RuntimeError("503")
            posted.append([e["run_id"] for e in events][0])

        with tempfile.TemporaryDirectory() as folder:
            state = os.path.join(folder, "state.json")
            args = type("A", (), {"since": "2d", "until": None, "state": state, "dry_run": False, "vl_url": "http://vl"})
            source = t.Source(Fake())
            err = io.StringIO()
            sys_stderr, sys.stderr = sys.stderr, err
            try:
                t.command_export(args, source, io.StringIO(), post=post)
                t.command_export(args, source, io.StringIO(), post=post)
                t.command_export(args, source, io.StringIO(), post=post)
            finally:
                sys.stderr = sys_stderr
            self.assertEqual([1, "fail-once", 2], posted)
            with open(state) as handle:
                self.assertEqual({"1/1", "2/1"}, set(json.load(handle)["done"]))


class FetchTest(unittest.TestCase):
    def test_runs_are_fetched_side_by_side_in_order(self):
        class Fake:
            repo = "o/r"

            def jobs(self, run):
                time.sleep(0.05)
                return [api_job("check (rest) %s" % run["id"], 0, 10)]

        runs = [{"id": i, "status": "completed", "conclusion": "success", "run_attempt": 1} for i in range(16)]
        started = time.time()
        records = t.Source(Fake(), with_metrics=False).fetch(runs)
        self.assertLess(time.time() - started, 16 * 0.05 / 2)
        self.assertEqual(list(range(16)), [r["id"] for r in records])


class CliTest(unittest.TestCase):
    # The exact argv infra's collector runs (nix/files/ci-metrics-collector/ci-metrics-collector in bitcashier/infra):
    # it failed every pass on ci-runner when `--vl-url` after `export` was refused (2026-11-01).
    COLLECTOR = ["--repo", "o/r", "export", "--since", "2d", "--state", "/tmp/s.json", "--vl-url", "http://vl:9429"]

    def parsed(self, argv):
        captured = {}

        def fake(args, source, out, **_):
            captured.update(vars(args))
        with unittest.mock.patch.object(t, "command_export", fake), \
                unittest.mock.patch.object(t, "GitHub", lambda repo: type("G", (), {"repo": repo})()):
            self.assertEqual(0, t.main(argv, out=io.StringIO()))
        return captured

    def test_the_collector_argv(self):
        args = self.parsed(self.COLLECTOR)
        self.assertEqual(("http://vl:9429", "2d", "/tmp/s.json"), (args["vl_url"], args["since"], args["state"]))

    def test_common_options_either_side_of_the_command(self):
        before = self.parsed(["--vl-url", "http://a", "--repo", "o/r", "export", "--since", "1d"])
        after = self.parsed(["export", "--since", "1d", "--repo", "o/r", "--vl-url", "http://a"])
        self.assertEqual(before["vl_url"], after["vl_url"])
        self.assertEqual("http://a", before["vl_url"])

    def test_a_value_before_the_command_is_not_reset_by_the_subcommand(self):
        self.assertEqual("http://a", self.parsed(["--vl-url", "http://a", "export", "--dry-run"])["vl_url"])

    def test_parse_only_touches_nothing(self):
        out = io.StringIO()
        with unittest.mock.patch.object(t, "GitHub", side_effect=AssertionError("no API")):
            self.assertEqual(0, t.main(self.COLLECTOR + ["--parse-only"], out=out))
        self.assertIn("arguments ok (export)", out.getvalue())

    def test_an_unknown_option_still_fails(self):
        with self.assertRaises(SystemExit) as raised, unittest.mock.patch("sys.stderr", io.StringIO()):
            t.main(["export", "--no-such-option"], out=io.StringIO())
        self.assertEqual(2, raised.exception.code)


if __name__ == "__main__":
    unittest.main()
