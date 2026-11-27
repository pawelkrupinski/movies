#!/usr/bin/env python3
"""Tests for scripts/ci-status: the newest run per gating workflow from one gh api call, a commit's runs, a run's
failed jobs and steps, the 30 s cache, an unreachable gh. gh is a stub script on KINOWO_GH that counts its calls."""
import os
import stat
import subprocess
import sys
import tempfile
import unittest

HERE = os.path.dirname(os.path.abspath(__file__))
RUNS = '''{"workflow_runs":[
 {"id":3,"name":"Main","path":".github/workflows/main.yml","head_sha":"bbbbbbbbbbbbbbbb","status":"in_progress","conclusion":null,"created_at":"2026-11-15T10:00:00Z"},
 {"id":2,"name":"Main","path":".github/workflows/main.yml","head_sha":"aaaaaaaaaaaaaaaa","status":"completed","conclusion":"failure","created_at":"2026-11-15T09:00:00Z"},
 {"id":9,"name":"Identity takes","path":".github/workflows/identity-takes.yml","head_sha":"aaaaaaaaaaaaaaaa","status":"completed","conclusion":"success","created_at":"2026-11-15T09:05:00Z"}]}'''
JOBS = '''{"jobs":[
 {"id":11,"name":"scope","status":"completed","conclusion":"success","steps":[]},
 {"id":12,"name":"test (worker)","status":"completed","conclusion":"failure","steps":[{"name":"Set up job","conclusion":"success"},{"name":"Run sbt","conclusion":"failure"}]},
 {"id":13,"name":"ui-tests","status":"completed","conclusion":"cancelled","steps":[]}]}'''
# iOS #526's ui-tests (run 43903912001): no runner, no step, its cause only in its annotations.
ANNOTATIONS = '''[
 {"annotation_level":"failure","message":"The job was not acquired by Runner of type hosted even after multiple attempts"},
 {"annotation_level":"notice","message":"Due to capacity constraints, jobs targeting macOS arm64 runners may experience longer queue times."},
 {"annotation_level":"notice","message":"Due to capacity constraints, jobs targeting macOS arm64 runners may experience longer queue times."}]'''


class CiStatus(unittest.TestCase):
    def setUp(self):
        self.dir = tempfile.mkdtemp()
        self.counter = os.path.join(self.dir, "calls")
        gh = os.path.join(self.dir, "gh")
        with open(gh, "w") as f:
            f.write(f"""#!/bin/bash
echo "$*" >> {self.counter}
[ -n "${{FAKE_GH_FAIL:-}}" ] && exit 1
case "$2" in
  *annotations*) cat <<'A'
{ANNOTATIONS}
A
;;
  *jobs*) cat <<'J'
{JOBS}
J
;;
  *) cat <<'R'
{RUNS}
R
;;
esac
""")
        os.chmod(gh, os.stat(gh).st_mode | stat.S_IEXEC)
        self.env = dict(os.environ, KINOWO_GH=gh, CI_STATUS_CACHE_DIR=os.path.join(self.dir, "cache"),
                        KINOWO_GH_REPO="o/r")

    def ci(self, *args, **env):
        done = subprocess.run([os.path.join(HERE, "ci-status"), *args], capture_output=True, text=True,
                              env=dict(self.env, **env))
        return done.returncode, done.stdout

    def calls(self):
        if not os.path.exists(self.counter):
            return 0
        with open(self.counter) as f:
            return len(f.read().splitlines())

    def test_newest_run_per_gating_workflow_from_one_call(self):
        status, out = self.ci()
        self.assertEqual(status, 0)
        rows = [line.split("\t") for line in out.splitlines()]
        self.assertEqual(rows[0], ["main.yml", "3", "bbbbbbbbbb", "in_progress", "-", "2026-11-15T10:00:00Z"])
        self.assertEqual(rows[1][1], "-")  # identity-model-convergence.yml has no run
        self.assertEqual(rows[2][:5], ["identity-takes.yml", "9", "aaaaaaaaaa", "completed", "success"])
        self.assertEqual(self.calls(), 1)

    def test_a_commit_selects_its_own_run(self):
        _, out = self.ci("--sha", "aaaaaaa", "--workflow", "Main")
        self.assertEqual(out.split("\t")[:5], ["Main", "2", "aaaaaaaaaa", "completed", "failure"])

    def test_a_repeat_within_thirty_seconds_is_cached(self):
        self.ci()
        self.ci()
        self.assertEqual(self.calls(), 1)
        self.ci("--no-cache")
        self.assertEqual(self.calls(), 2)

    def test_a_run_lists_only_its_failed_jobs_and_steps(self):
        _, out = self.ci("--run", "2")
        self.assertIn("3 jobs, 1 ok", out)
        self.assertIn("test (worker)\t12\tcompleted\tfailure\tRun sbt", out)
        self.assertNotIn("scope", out)
        self.assertIn("agentctl ci-why 2", out)

    def test_a_job_with_no_failed_step_is_named_by_its_annotations(self):
        _, out = self.ci("--run", "2")
        self.assertIn("ui-tests\t13\tcompleted\tcancelled\t-\n  failure: The job was not acquired by Runner of type hosted "
                      "even after multiple attempts\n  notice: Due to capacity constraints", out)
        self.assertEqual(out.count("capacity constraints"), 1)  # each distinct annotation once

    def test_a_job_with_a_failed_step_asks_for_no_annotations(self):
        self.ci("--run", "2")
        with open(self.counter) as f:
            asked = f.read()
        self.assertIn("check-runs/13/annotations", asked)
        self.assertNotIn("check-runs/12/", asked)

    def test_unreachable_gh_is_exit_2(self):
        status, _ = self.ci(FAKE_GH_FAIL="1")
        self.assertEqual(status, 2)


sys.path.insert(0, HERE)
sys.path.insert(0, os.path.join(HERE, "lib"))
import ci_status  # noqa: E402
import tool_github  # noqa: E402
import tool_process  # noqa: E402
from tool_files import Files  # noqa: E402


class Gh:
    """gh answering `stdout` with `code`, counting its calls."""

    def __init__(self, stdout, code=0):
        self.stdout, self.code, self.asked = stdout, code, []

    def run(self, command):
        return self.capture(command).code

    def capture(self, command):
        self.asked.append(list(command.argv))
        return tool_process.Captured(self.code, self.stdout, "")


class CachedApiTest(unittest.TestCase):
    """The cache over in-memory files and an injected clock: fresh for 30 s, then asked again; --no-cache asks every
    time; a torn cache file is asked again, never an error; gh failing is None and nothing kept."""

    def api(self, gh, use_cache=True):
        self.clock = [1000.0]
        self.files = Files.in_memory(now=lambda: self.clock[0])
        return ci_status.CachedApi(tool_github.GitHub(gh, {}, "gh"), self.files, lambda: self.clock[0], "o/r",
                                   "/cache", use_cache)

    def test_an_answer_is_kept_thirty_seconds(self):
        gh = Gh('{"n": 1}')
        api = self.api(gh)
        self.assertEqual({"n": 1}, api.get("actions/runs"))
        self.clock[0] += 29
        self.assertEqual({"n": 1}, api.get("actions/runs"))
        self.assertEqual(1, len(gh.asked))
        self.clock[0] += 2
        api.get("actions/runs")
        self.assertEqual([["gh", "api", "repos/o/r/actions/runs"]] * 2, gh.asked)

    def test_no_cache_asks_every_time(self):
        gh = Gh("[]")
        api = self.api(gh, use_cache=False)
        api.get("x")
        api.get("x")
        self.assertEqual(2, len(gh.asked))

    def test_a_torn_cache_file_is_asked_again(self):
        gh = Gh('{"n": 2}')
        api = self.api(gh)
        api.get("x")
        [kept] = self.files.list("/cache")
        self.files.write_text("/cache/" + kept, '{"n"')
        self.assertEqual({"n": 2}, api.get("x"))
        self.assertEqual(2, len(gh.asked))

    def test_gh_failing_is_none_and_keeps_nothing(self):
        api = self.api(Gh("", code=1))
        self.assertIsNone(api.get("x"))
        self.assertFalse(self.files.exists("/cache"))


if __name__ == "__main__":
    unittest.main()
