#!/usr/bin/env python3
"""ci_timings.py over a hand-made run: two convergence legs, their steps and PhaseTimer log lines.
The last case hands the payload to a REAL Prometheus's OTLP receiver when one is on PATH.
Run: python3 scripts/ci/ci_timings_test.py"""
import datetime
import http.server
import io
import json
import shutil
import socket
import subprocess
import sys
import tempfile
import threading
import re
import time
import unittest
import urllib.error
import urllib.parse
import urllib.request
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent))
import ci_timings as timings  # noqa: E402

REPO = Path(__file__).resolve().parents[2]

RUN = {"id": 7, "name": "Identity model convergence", "head_branch": "main", "conclusion": "failure",
       "created_at": "2026-10-14T07:59:00Z", "run_started_at": "2026-10-14T08:00:00Z"}
STAMP = "1791964800000000000"  # 2026-10-14T08:00:00Z, the run's start


def job(job_id, name, started, completed, steps, conclusion="success", status="completed"):
    return {"id": job_id, "name": name, "status": status, "conclusion": conclusion,
            "started_at": started, "completed_at": completed,
            "steps": [{"name": n, "started_at": s, "completed_at": c, "conclusion": "success"} for n, s, c in steps]}


JOBS = [
    job(1, "preflight", "2026-10-14T08:01:00Z", "2026-10-14T08:01:05Z", []),
    job(2, "united-kingdom / convergence", "2026-10-14T08:02:00Z", "2026-10-14T08:14:00Z", [
        ("Run ./.github/actions/convergence-setup", "2026-10-14T08:02:10Z", "2026-10-14T08:02:45Z"),
        ("Run the united-kingdom convergence suite", "2026-10-14T08:02:45Z", "2026-10-14T08:13:50Z"),
        ("Run actions/upload-artifact@v6", "2026-10-14T08:13:50Z", "2026-10-14T08:13:52Z"),
        ("Run actions/upload-artifact@v6", "2026-10-14T08:13:52Z", "2026-10-14T08:13:55Z"),
    ]),
    job(3, "spain / convergence", "2026-10-14T08:02:30Z", "2026-10-14T08:06:00Z", [
        ("Run the spain convergence suite", "2026-10-14T08:03:00Z", "2026-10-14T08:05:40Z"),
    ], conclusion="failure"),
    job(4, "spain / sample", "2026-10-14T08:02:30Z", None, [], conclusion="skipped"),
    job(5, "timings", "2026-10-14T08:14:05Z", None, [], conclusion=None, status="in_progress"),
]

UK_LOG = "\n".join([
    "2026-10-14T08:03:00.1Z [uk] seed 817 venues into the archive done in 4.1s, cpu 13.3s, gc 0.4s, heap 2.47 of 7.00GB",
    "2026-10-14T08:03:00.2Z [uk] seed 817 venues into the archive done in 4.5s, cpu 14.3s, gc 0.4s, heap 2.73 of 7.00GB",
    "2026-10-14T08:09:00.0Z [uk]   identityTick done in 368.9s, cpu 1101.8s, gc 14.1s, heap 6.12 of 7.00GB",
    "2026-10-14T08:10:00.0Z \x1b[0m[uk] bootCutover done in 486.8s, cpu 1407.3s, gc 16.9s, heap 3.36 of 7.00GB\x1b[0m",
    "2026-10-14T08:11:00.0Z [ukp0] replayCutover done in 590.0s, cpu 1.0s, gc 0.0s, heap 1.00 of 7.00GB",
    "2026-10-14T08:11:30.0Z [ukp2] replayCutover done in 604.7s, cpu 1.0s, gc 0.0s, heap 1.00 of 7.00GB",
    "2026-10-14T08:12:00.0Z [uk] tick 1 done in 8.5s, cpu 1.0s, gc 0.0s, heap 1.00 of 7.00GB",
    "2026-10-14T08:12:00.0Z [uk] bootCutover …",
])
LOGS = {2: UK_LOG, 3: "[es] bootCutover done in 99.0s, cpu 1.0s", 1: "[pl] never fetched done in 1.0s"}


def jobs():
    return timings.collect(JOBS, lambda job_id: LOGS.get(job_id, ""))


class CollectTest(unittest.TestCase):
    def test_only_finished_jobs_longest_first(self):
        self.assertEqual([j.name for j in jobs()], ["united-kingdom / convergence", "spain / convergence", "preflight"])

    def test_steps_keep_github_names(self):
        uk, es, _ = jobs()
        self.assertIn(("Run the united-kingdom convergence suite", 665.0), uk.steps)
        self.assertIn(("Run the spain convergence suite", 160.0), es.steps)

    def test_top_level_phases_keep_their_longest_copy(self):
        self.assertEqual(jobs()[0].phases, {
            "seed N venues into the archive": 4.5,
            "bootCutover": 486.8,           # through the colour codes
            "replayCutover": 604.7,         # the slowest of the passes running side by side
            "tick 1": 8.5,
        })                                  # and no indented sub-phase, no unfinished `…` line

    def test_a_short_job_s_log_is_never_fetched(self):
        self.assertEqual(jobs()[2].phases, {})

    def test_an_excluded_job_is_left_out(self):
        self.assertNotIn("preflight", [j.name for j in timings.collect(JOBS, lambda _: "", exclude={"preflight"})])

    def test_run_measures(self):
        self.assertEqual(timings.run_measures(RUN, jobs()), {"wall": 780.0, "queued": 60.0})


class PayloadTest(unittest.TestCase):
    def series(self, name, payload=None):
        payload = payload or timings.otlp_payload(RUN, jobs())
        for metric in payload["resourceMetrics"][0]["scopeMetrics"][0]["metrics"]:
            if metric["name"] == name:
                return [({a["key"]: a["value"]["stringValue"] for a in p["attributes"]}, p["asDouble"], p["timeUnixNano"])
                        for p in metric["gauge"]["dataPoints"]]
        self.fail("no metric %s" % name)

    def test_every_sample_is_stamped_at_the_run_start(self):
        stamps = {stamp for name in ("kinowo_ci_run_seconds", "kinowo_ci_job_seconds",
                                     "kinowo_ci_step_seconds", "kinowo_ci_phase_seconds")
                  for _, _, stamp in self.series(name)}
        self.assertEqual(stamps, {STAMP})

    def test_a_run_pushed_twice_is_the_same_payload(self):
        self.assertEqual(json.dumps(timings.otlp_payload(RUN, jobs())), json.dumps(timings.otlp_payload(RUN, jobs())))

    def test_run_measures_carry_workflow_and_conclusion(self):
        self.assertIn(({"workflow": "Identity model convergence", "ref": "main", "measure": "wall", "conclusion": "failure"}, 780.0, STAMP),
                      self.series("kinowo_ci_run_seconds"))

    def test_jobs_carry_workflow_name_and_conclusion(self):
        self.assertIn(({"workflow": "Identity model convergence", "ref": "main", "ci_job": "spain / convergence", "conclusion": "failure"}, 210.0, STAMP),
                      self.series("kinowo_ci_job_seconds"))

    def test_a_branch_run_is_charted_as_branch(self):
        payload = timings.otlp_payload(dict(RUN, head_branch="convergence-timings"), jobs())
        self.assertEqual({labels["ref"] for labels, _, _ in self.series("kinowo_ci_job_seconds", payload)}, {"branch"})

    def test_a_repeated_step_is_one_summed_sample(self):
        uploads = [value for labels, value, _ in self.series("kinowo_ci_step_seconds")
                   if labels["step"] == "Run actions/upload-artifact@v6"]
        self.assertEqual(uploads, [5.0])

    def test_phases_per_job(self):
        self.assertIn(({"workflow": "Identity model convergence", "ref": "main", "ci_job": "united-kingdom / convergence", "phase": "replayCutover"}, 604.7, STAMP),
                      self.series("kinowo_ci_phase_seconds"))

    def test_no_jobs_no_payload(self):
        self.assertIsNone(timings.otlp_payload(RUN, []))

    def test_summary_names_the_longest_step_and_phases(self):
        self.assertIn("| united-kingdom / convergence | 12m00s | Run the united-kingdom convergence suite 11m05s | replayCutover 605s, bootCutover 487s",
                      timings.summary_markdown(RUN, jobs()))

    def test_report_prints_steps_and_phases(self):
        out = io.StringIO()
        timings.report(RUN, jobs(), out)
        self.assertIn("wall 13m00s, queued 1m00s", out.getvalue())
        self.assertIn("phase    604.7s  replayCutover", out.getvalue())


class ConclusionTest(unittest.TestCase):
    def test_a_run_in_progress_takes_its_jobs_worst(self):
        self.assertEqual(timings.conclusion_of(dict(RUN, conclusion=None), jobs()), "failure")

    def test_all_green_jobs_are_a_success(self):
        green = [j for j in jobs() if j.conclusion == "success"]
        self.assertEqual(timings.conclusion_of(dict(RUN, conclusion=None), green), "success")

    def test_a_finished_run_keeps_its_own(self):
        self.assertEqual(timings.conclusion_of(dict(RUN, conclusion="cancelled"), jobs()), "cancelled")


class Response:
    def __init__(self, status):
        self.status = status

    def __enter__(self):
        return self

    def __exit__(self, *exc):
        return False


class PushTest(unittest.TestCase):
    def test_unset_secrets_skip(self):
        self.assertIn("not pushed", timings.push({}, "", ""))

    def test_posts_with_the_token_to_the_gated_path(self):
        seen = []

        def opener(request, timeout):
            seen.append(request)
            return Response(200)

        self.assertEqual(timings.push({"a": 1}, "https://grafana.example/", "tok", opener), "Pushed timings.")
        self.assertEqual(seen[0].full_url, "https://grafana.example/ci/otlp/v1/metrics")
        self.assertEqual(seen[0].get_header("Authorization"), "Bearer tok")
        self.assertEqual(seen[0].get_method(), "POST")

    def test_a_failure_is_a_warning_not_an_exception(self):
        def opener(request, timeout):
            raise urllib.error.HTTPError(request.full_url, 302, "Found", {}, None)

        self.assertTrue(timings.push({}, "https://g", "t", opener).startswith("::warning::"))

    def test_the_default_opener_does_not_follow_a_redirect_to_the_sign_in(self):
        class Redirect(http.server.BaseHTTPRequestHandler):
            def do_POST(self):
                self.send_response(302)
                self.send_header("Location", "/oauth2/start")
                self.end_headers()

            def log_message(self, *args):
                pass

        server = http.server.HTTPServer(("127.0.0.1", 0), Redirect)
        threading.Thread(target=server.handle_request, daemon=True).start()
        try:
            message = timings.push({}, "http://127.0.0.1:%d" % server.server_address[1], "t")
        finally:
            server.server_close()
        self.assertEqual(message, "::warning::timings not pushed: HTTP Error 302: Found")


class FakeGitHub:
    """Runs by workflow, and each run's jobs, as `GitHub` reads them."""

    def __init__(self, runs, jobs_of):
        self.runs, self.jobs_of, self.asked = runs, jobs_of, []

    def workflow_runs(self, workflow_id, since, until):
        self.asked.append((workflow_id, since, until))
        return [run for run in self.runs if run["workflow_id"] == workflow_id and since <= run["created_at"][:10] < until]

    def workflows(self):
        return sorted({run["workflow_id"] for run in self.runs})

    def workflow_name(self, workflow_id):
        return "Workflow %s" % workflow_id

    def jobs(self, run_id):
        return self.jobs_of.get(run_id, [])

    def log(self, job_id):
        raise AssertionError("the history reads no logs")


# A run from before the in-run reporter: no `timings` job among its jobs.
UNREPORTED = [j for j in JOBS if j["name"] != "timings"]


class HistoryTest(unittest.TestCase):
    """The last N days of runs on main, from GitHub's run history, as payloads to push once."""

    def run_(self, run_id, created, conclusion="success", workflow_id=1):
        return dict(RUN, id=run_id, workflow_id=workflow_id, created_at=created, run_started_at=created, conclusion=conclusion)

    def test_every_finished_run_of_every_workflow_its_steps_but_no_phases(self):
        github = FakeGitHub([self.run_("a", "2026-09-01T10:00:00Z"), self.run_("b", "2026-09-20T10:00:00Z", workflow_id=2)],
                            {"a": UNREPORTED, "b": UNREPORTED})
        payloads = list(timings.history(github, days=60, today=datetime.date(2026, 10, 14)))
        self.assertEqual(len(payloads), 2)
        names = {m["name"] for p in payloads for m in p["resourceMetrics"][0]["scopeMetrics"][0]["metrics"]}
        self.assertEqual(names, {"kinowo_ci_run_seconds", "kinowo_ci_job_seconds", "kinowo_ci_step_seconds"})

    def test_a_run_its_own_timings_job_reported_is_not_pushed_again(self):
        github = FakeGitHub([self.run_("a", "2026-10-14T10:00:00Z")], {"a": JOBS})   # JOBS holds its `timings` job
        self.assertEqual(list(timings.history(github, days=60, today=datetime.date(2026, 10, 14))), [])

    def test_a_skipped_or_cancelled_run_is_left_out_without_reading_its_jobs(self):
        github = FakeGitHub([self.run_("a", "2026-10-01T10:00:00Z", conclusion="skipped"),
                             self.run_("b", "2026-10-02T10:00:00Z", conclusion="cancelled")], {})
        github.jobs = lambda run_id: self.fail("read the jobs of %s" % run_id)
        self.assertEqual(list(timings.history(github, days=60, today=datetime.date(2026, 10, 14))), [])

    def test_asks_in_windows_github_answers_whole(self):
        # GitHub answers a filtered run listing with at most 1000 runs: Main alone runs more in 90 days.
        github = FakeGitHub([self.run_("a", "2026-10-01T10:00:00Z")], {"a": UNREPORTED})
        list(timings.history(github, days=90, today=datetime.date(2026, 10, 14)))
        spans = [(datetime.date.fromisoformat(until) - datetime.date.fromisoformat(since)).days for _, since, until in github.asked]
        self.assertTrue(all(span <= timings.HISTORY_WINDOW_DAYS for span in spans))
        self.assertEqual(github.asked[0][1], "2026-07-16")
        self.assertEqual(github.asked[-1][2], "2026-10-15")


class WorkflowNameTest(unittest.TestCase):
    """A workflow with a `run-name:` (identity measure, identity decorations) titles each run its own way, and GitHub
    names the run by that title: charted by it, each run would be a workflow of its own on the board."""

    def test_a_run_is_charted_under_its_workflow_s_name_not_its_run_name(self):
        run = HistoryTest.run_(None, "a", "2026-10-01T10:00:00Z", workflow_id=3)
        run["name"] = "identity measure w7v2"
        github = FakeGitHub([run], {"a": UNREPORTED})
        [payload] = timings.history(github, days=60, today=datetime.date(2026, 10, 14))
        labels = {a["key"]: a["value"]["stringValue"] for a in
                  payload["resourceMetrics"][0]["scopeMetrics"][0]["metrics"][0]["gauge"]["dataPoints"][0]["attributes"]}
        self.assertEqual(labels["workflow"], "Workflow 3")
        self.assertEqual(timings.named(github, run)["name"], "Workflow 3")


class FlakyHistoryTest(unittest.TestCase):
    def test_a_run_whose_jobs_cannot_be_read_is_asked_again_then_skipped_and_the_rest_still_written(self):
        runs = [HistoryTest.run_(None, run_id, "2026-10-0%dT10:00:00Z" % day) for run_id, day in (("a", 1), ("b", 2), ("c", 3))]
        asked = []

        class Flaky(FakeGitHub):
            def jobs(self, run_id):
                asked.append(run_id)
                if run_id == "b":
                    raise subprocess.CalledProcessError(1, ["gh"])
                return UNREPORTED
        payloads = list(timings.history(Flaky(runs, {}), days=60, today=datetime.date(2026, 10, 14)))
        self.assertEqual(len(payloads), 2)
        self.assertEqual(asked.count("b"), timings.HISTORY_ATTEMPTS)


class PushFileTest(unittest.TestCase):
    def test_pushes_every_payload_in_batches_and_counts_what_failed(self):
        payload = timings.otlp_payload(RUN, jobs())
        path = Path(tempfile.mkdtemp()) / "history.jsonl"
        path.write_text("\n".join(json.dumps(payload) for _ in range(5)) + "\n")
        posted = []

        def opener(request, timeout):
            body = json.loads(request.data)
            posted.append(len(body["resourceMetrics"]))
            return Response(200 if len(posted) == 1 else 400)
        pushed, failed = timings.push_file(path, "https://grafana.example", "t", batch=3, opener=opener)
        self.assertEqual(posted, [3, 2, 1, 1])
        self.assertEqual((pushed, failed), (3, 2))

    def test_a_batch_refused_for_one_run_pushes_the_rest_of_it_one_by_one(self):
        # One run past the out-of-order window (a history pushed hours after it was read) failed its whole batch of 25:
        # run 38639023800 lost 72 good runs with 3 too old.
        too_old = dict(RUN, run_started_at="2026-07-01T00:00:00Z")
        path = Path(tempfile.mkdtemp()) / "history.jsonl"
        path.write_text("\n".join(json.dumps(timings.otlp_payload(run, jobs())) for run in (RUN, too_old, RUN)) + "\n")

        def opener(request, timeout):
            stamps = {dp["timeUnixNano"] for rm in json.loads(request.data)["resourceMetrics"] for sm in rm["scopeMetrics"]
                      for m in sm["metrics"] for dp in m["gauge"]["dataPoints"]}
            return Response(400 if any(int(stamp) < 1790000000000000000 for stamp in stamps) else 200)
        self.assertEqual(timings.push_file(path, "https://grafana.example", "t", batch=25, opener=opener), (2, 1))


@unittest.skipUnless(shutil.which("prometheus"), "no prometheus on PATH")
class RealPrometheusTest(unittest.TestCase):
    """The payload as Prometheus's own OTLP receiver reads it: names and labels unchanged, a stamp
    hours old admitted by the out-of-order window, and the same run pushed twice accepted."""

    def test_prometheus_stores_what_the_dashboards_query(self):
        with socket.socket() as probe:
            probe.bind(("127.0.0.1", 0))
            port = probe.getsockname()[1]
        data = tempfile.mkdtemp()
        config = Path(data) / "prometheus.yml"
        # The window the monitoring host runs with, read from its own config.
        window = re.search(r"out_of_order_time_window: (\S+)", (REPO / "infra/nix/files/monitoring/prometheus.yaml").read_text()).group(1)
        config.write_text("storage:\n  tsdb:\n    out_of_order_time_window: %s\n" % window)
        server = subprocess.Popen(["prometheus", "--config.file=%s" % config, "--storage.tsdb.path=%s/tsdb" % data,
                                   "--web.listen-address=127.0.0.1:%d" % port, "--web.enable-otlp-receiver"],
                                  stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        try:
            base = "http://127.0.0.1:%d" % port
            for _ in range(60):
                try:
                    urllib.request.urlopen(base + "/-/ready", timeout=1).close()
                    break
                except Exception:
                    time.sleep(0.5)
            started = datetime.datetime.now(datetime.timezone.utc) - datetime.timedelta(hours=5)
            payload = timings.otlp_payload(dict(RUN, run_started_at=started.strftime("%Y-%m-%dT%H:%M:%SZ")), jobs())
            for _ in range(2):
                request = urllib.request.Request(base + "/api/v1/otlp/v1/metrics", data=json.dumps(payload).encode(),
                                                 headers={"Content-Type": "application/json"}, method="POST")
                with urllib.request.urlopen(request, timeout=10) as response:
                    self.assertEqual(response.status, 200)
            # The history backfill: a run two months old, as `--history-days` reads it off GitHub, admitted too --
            # Prometheus keeps 90 days (roles/prometheus.nix `retention`), and a trend needs them all.
            old = datetime.datetime.now(datetime.timezone.utc) - datetime.timedelta(days=60)
            history = timings.otlp_payload(dict(RUN, run_started_at=old.strftime("%Y-%m-%dT%H:%M:%SZ")), jobs())
            request = urllib.request.Request(base + "/api/v1/otlp/v1/metrics", data=json.dumps(history).encode(),
                                             headers={"Content-Type": "application/json"}, method="POST")
            with urllib.request.urlopen(request, timeout=10) as response:
                self.assertEqual(response.status, 200)
            query = 'last_over_time(kinowo_ci_phase_seconds{ci_job="united-kingdom / convergence",phase="replayCutover"}[6h])'
            with urllib.request.urlopen(base + "/api/v1/query?" + urllib.parse.urlencode({"query": query})) as response:
                result = json.load(response)
            [series] = result["data"]["result"]
            self.assertEqual(series["metric"]["workflow"], "Identity model convergence")
            self.assertEqual(series["metric"]["job"], timings.SERVICE_NAME)
            self.assertEqual(series["value"][1], "604.7")
        finally:
            server.terminate()
            server.wait(timeout=30)
            shutil.rmtree(data, ignore_errors=True)


if __name__ == "__main__":
    unittest.main()
