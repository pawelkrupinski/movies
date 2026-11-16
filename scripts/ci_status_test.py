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


if __name__ == "__main__":
    unittest.main()
