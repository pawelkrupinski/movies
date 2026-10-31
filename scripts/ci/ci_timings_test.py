#!/usr/bin/env python3
"""ci_timings.py over a hand-made run: two convergence legs, their steps and PhaseTimer log lines.
The last case hands the payload to a REAL Prometheus's OTLP receiver when one is on PATH.
Run: python3 scripts/ci/ci_timings_test.py"""
import datetime
import http.server
import io
import json
import os
import shutil
import signal
import socket
import subprocess
import sys
import tempfile
import threading
import re
import time
import unittest
import unittest.mock
import urllib.error
import urllib.parse
import urllib.request
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent))
import ci_timings as timings  # noqa: E402

REPO = Path(__file__).resolve().parents[2]


def wait_until(what, predicate, bound=120):
    """Poll predicate until it holds (an exception counts as not yet), then return. After `bound` seconds FAIL the test
    naming `what`: a wait never counts a fixed number of tries and falls through to test nothing."""
    deadline = time.monotonic() + bound
    last = None
    while True:
        try:
            if predicate():
                return
            last = None
        except Exception as error:
            last = error
        if time.monotonic() >= deadline:
            raise AssertionError("%s: not within %ss (last error: %r)" % (what, bound, last))
        time.sleep(0.1)

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

    def test_the_reporter_still_running_is_left_out_of_its_own_report(self):
        self.assertNotIn("timings", [j.name for j in jobs()])

    def test_a_short_job_s_log_is_never_fetched(self):
        self.assertEqual(jobs()[2].phases, {})


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


class CeilingTest(unittest.TestCase):
    """Each job's `timeout-minutes` beside its time, for the near-ceiling alert (ci-build-times.rules)."""

    CEILINGS = timings.workflow_ceilings(".github/workflows/identity-model-convergence.yml")

    def test_a_literal_ceiling_is_read_off_the_workflow_and_the_ones_it_calls(self):
        self.assertEqual(timings.ceiling_of("preflight", self.CEILINGS), 5)
        self.assertEqual(timings.ceiling_of("report (timings)", self.CEILINGS), 5)  # report-timings.yml's

    def test_a_caller_row_is_not_its_callee(self):
        self.assertIsNone(timings.ceiling_of("leg (poland)", self.CEILINGS))

    def test_an_expression_ceiling_is_read_off_the_line_its_job_echoes(self):
        echoed = 'echo "ci-job-ceiling 55 minutes"\nci-job-ceiling 55 minutes\n'
        [takes] = timings.collect([job(9, "takes (poland)", "2026-10-14T08:02:00Z", "2026-10-14T08:50:00Z", [])],
                                  lambda _: echoed, self.CEILINGS)
        self.assertEqual(takes.ceiling, 3300.0)

    def test_without_its_line_an_expression_ceiling_is_unknown(self):
        [takes] = timings.collect([job(9, "takes (poland)", "2026-10-14T08:02:00Z", "2026-10-14T08:50:00Z", [])],
                                  lambda _: "", self.CEILINGS)
        self.assertIsNone(takes.ceiling)

    def test_a_job_naming_none_has_the_platform_default(self):
        ceilings = timings.workflow_ceilings(".github/workflows/nix-stage-closures.yml")
        self.assertEqual(timings.ceiling_of("nix-evaluate", ceilings), timings.DEFAULT_TIMEOUT_MINUTES)
        self.assertEqual(timings.ceiling_of("publish-web", timings.workflow_ceilings(".github/workflows/main.yml")), 10)

    def test_a_ceiling_with_a_comment_after_it_is_the_number(self):
        # ci.yml's rows: `timeout-minutes: 40 # longest green row 31 min`, read whole, was an expression no job echoed
        ceilings = timings.workflow_ceilings(".github/workflows/main.yml")
        self.assertEqual(timings.ceiling_of("test (rest)", ceilings), 40)
        self.assertEqual(timings.scalar(' "40 # not a comment" # one'), "40 # not a comment")

    def test_the_ceiling_and_the_run_number_go_out_beside_the_times(self):
        [pre] = timings.collect([JOBS[0]], lambda _: "", self.CEILINGS)
        payload = timings.otlp_payload(dict(RUN, run_number=1641), [pre])
        names = {m["name"]: m["gauge"]["dataPoints"] for m in payload["resourceMetrics"][0]["scopeMetrics"][0]["metrics"]}
        self.assertEqual([p["asDouble"] for p in names["kinowo_ci_job_timeout_seconds"]], [300.0])
        self.assertEqual([p["asDouble"] for p in names["kinowo_ci_run_number"]], [1641.0])
        self.assertEqual({p["timeUnixNano"] for p in names["kinowo_ci_run_number"]}, {STAMP})


class WorkflowCeilingSpec(unittest.TestCase):
    """EVERY JOB'S CEILING IS KNOWN: every job that runs steps names a literal `timeout-minutes`, names none (the
    platform's default), or names an expression AND echoes what it came to -- the same expression, so the two cannot
    drift apart. A job whose ceiling went unexported could reach it with no near-ceiling alert first."""

    def test_every_job_of_every_workflow(self):
        missing = []
        for name in sorted(os.listdir(timings.WORKFLOWS)):
            with open(os.path.join(timings.WORKFLOWS, name), encoding="utf-8") as handle:
                jobs = timings.workflow_jobs(handle.read())
            self.assertTrue(jobs, "%s: no jobs read" % name)
            for key, (attrs, block) in jobs.items():
                timeout = attrs.get("timeout-minutes")
                if "uses" in attrs or timeout is None or timeout.isdigit():
                    continue
                if 'echo "ci-job-ceiling %s minutes"' % timeout not in block:
                    missing.append("%s: %s" % (name, key))
        self.assertEqual(missing, [], "expression ceilings with no `ci-job-ceiling` echo of the same expression")

    def test_the_convergence_row_echoes_its_ceiling(self):
        with open(os.path.join(timings.WORKFLOWS, "country-convergence-leg.yml"), encoding="utf-8") as handle:
            attrs, block = timings.workflow_jobs(handle.read())["convergence"]
        self.assertIn("${{", attrs["timeout-minutes"])
        self.assertIn("ci-job-ceiling", block)


class RunnerWaitTest(unittest.TestCase):
    """How long the run's first job waited for a runner, not for the run ahead of it."""

    MAIN = {"id": 20, "path": ".github/workflows/main.yml", "head_branch": "main", "created_at": "2026-10-14T07:56:00Z",
            "run_started_at": "2026-10-14T08:00:00Z"}

    def ahead(self, **fields):
        return dict({"id": 19, "path": self.MAIN["path"], "head_branch": "main", "status": "completed",
                     "created_at": "2026-10-14T07:30:00Z", "updated_at": "2026-10-14T07:59:30Z"}, **fields)

    def wait(self, others):
        return timings.run_measures(self.MAIN, jobs(), timings.ready_at(self.MAIN, others))["runner_wait"]

    def test_from_the_end_of_the_run_it_queued_behind(self):
        self.assertEqual(self.wait([self.ahead()]), 90.0)  # first job 08:01:00

    def test_from_its_creation_when_nothing_was_ahead(self):
        self.assertEqual(self.wait([self.ahead(updated_at="2026-10-14T07:50:00Z")]), 300.0)

    def test_other_workflows_branches_and_later_runs_hold_nothing_back(self):
        others = [self.ahead(path=".github/workflows/ios.yml"), self.ahead(head_branch="measure/x"),
                  self.ahead(id=21, created_at="2026-10-14T07:57:00Z"), self.ahead(status="in_progress")]
        self.assertEqual(self.wait(others), 300.0)

    def test_never_negative(self):
        self.assertEqual(self.wait([self.ahead(updated_at="2026-10-14T09:00:00Z")]), 0.0)


class SummaryTest(unittest.TestCase):
    """The job summary is written."""

    class Github:
        def workflow_name(self, _):
            return RUN["name"]

        def jobs(self, _):
            return JOBS

        def log(self, job_id):
            return LOGS.get(job_id, "")

    def summary_on(self, server):
        with tempfile.TemporaryDirectory() as tmp:
            summary = Path(tmp, "summary.md")
            summary.write_text("")
            env = {"GITHUB_STEP_SUMMARY": str(summary), "GITHUB_SERVER_URL": server}
            with unittest.mock.patch.dict(os.environ, env):
                timings.handle(self.Github(), dict(RUN, workflow_id=1), type("Args", (), {"push": False})(), io.StringIO())
            return summary.read_text()

    def test_github_gets_the_summary(self):
        self.assertIn("united-kingdom / convergence", self.summary_on("https://github.com"))


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


class WorkflowNameTest(unittest.TestCase):
    """A workflow with a `run-name:` (identity measure, identity decorations) titles each run its own way, and GitHub
    names the run by that title: charted by it, each run would be a workflow of its own on the board."""

    class Named:
        def workflow_name(self, workflow_id):
            return "Workflow %s" % workflow_id

    def test_a_run_is_charted_under_its_workflow_s_name_not_its_run_name(self):
        run = dict(RUN, workflow_id=3, name="identity measure w7v2")
        named = timings.named(self.Named(), run)
        self.assertEqual(named["name"], "Workflow 3")
        labels = {a["key"]: a["value"]["stringValue"] for a in
                  timings.otlp_payload(named, jobs())["resourceMetrics"][0]["scopeMetrics"][0]["metrics"][0]["gauge"]["dataPoints"][0]["attributes"]}
        self.assertEqual(labels["workflow"], "Workflow 3")


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
            wait_until("prometheus answers /-/ready", lambda: urllib.request.urlopen(base + "/-/ready", timeout=1).close() or True)
            started = datetime.datetime.now(datetime.timezone.utc) - datetime.timedelta(hours=5)
            payload = timings.otlp_payload(dict(RUN, run_started_at=started.strftime("%Y-%m-%dT%H:%M:%SZ")), jobs())
            for _ in range(2):
                request = urllib.request.Request(base + "/api/v1/otlp/v1/metrics", data=json.dumps(payload).encode(),
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


class HungGhTest(unittest.TestCase):
    """A gh that never answers must not hold the run's last job: json() raises at the bound, log() gives no log.

    The stub never ends on its own (it records its pid and blocks until killed), so both calls return only by the bound
    cutting gh off, and none is left running -- proven by that, never by elapsed wall time, which a loaded machine
    stretches ~10x (scripts/shell-spec.sh's HUNG STUBS). An alarm only turns a call that does hang into a failure."""

    def setUp(self):
        self.bin = tempfile.mkdtemp()
        self.hung = Path(self.bin, "hung")
        gh = Path(self.bin, "gh")
        gh.write_text('#!/bin/sh\necho $$ >> "%s"\nexec sleep 86400\n' % self.hung)
        gh.chmod(0o755)
        self.addCleanup(shutil.rmtree, self.bin, True)
        self.path = mock_environ_path(self.bin)

    def test_bounded(self):
        def hung(signum, frame):
            raise AssertionError("a gh call was not cut off by its bound")
        previous = signal.signal(signal.SIGALRM, hung)
        signal.alarm(int(os.environ.get("SPEC_HANG_BOUND", "600")))  # shell-spec.sh's outer bound
        try:
            with self.path, mock_timeout(1):
                github = timings.GitHub("o/r")
                self.assertEqual(github.log(1), "")
                with self.assertRaises(subprocess.TimeoutExpired):
                    github.json("actions/runs/1")
        finally:
            signal.alarm(0)
            signal.signal(signal.SIGALRM, previous)
        self.assertEqual([pid for pid in self.recorded_pids() if alive(pid)], [])

    def recorded_pids(self):
        return [int(line) for line in self.hung.read_text().split()] if self.hung.exists() else []


def alive(pid):
    try:
        os.kill(pid, 0)
    except ProcessLookupError:
        return False
    os.kill(pid, signal.SIGKILL)  # a failing case strands nothing
    return True


def mock_timeout(seconds):
    from unittest import mock
    return mock.patch.object(timings, "GH_TIMEOUT_SECONDS", seconds)


def mock_environ_path(directory):
    from unittest import mock
    import os
    return mock.patch.dict(os.environ, {"PATH": directory + os.pathsep + os.environ["PATH"]})


if __name__ == "__main__":
    unittest.main()
