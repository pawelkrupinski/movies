#!/usr/bin/env python3
"""Tests for harness_aa.py: which pushes run the drift harness's A/A row (a harness path changed since the last
convergence run's head, else since the parent), how the row's drift check is judged (only two replays of one commit
differing is the harness's fault), and the wiring that runs it in main's convergence.

    python3 scripts/ci/harness_aa_test.py
"""
import os
import subprocess
import sys
import tempfile
import unittest

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(os.path.dirname(HERE))
TOOL = os.path.join(HERE, "harness_aa.py")
SCRATCH_GIT = os.path.join(ROOT, "scripts/scratch-git.sh")
HARNESS = "scripts/ci/identity-drift-check.sh"


class HarnessAaTest(unittest.TestCase):
    def setUp(self):
        self.dir = tempfile.TemporaryDirectory()
        self.repo = os.path.join(self.dir.name, "repo")
        self.env = {k: v for k, v in os.environ.items() if not k.startswith("GIT_")}
        self.env.update(GIT_CONFIG_GLOBAL="/dev/null", GIT_CONFIG_NOSYSTEM="1")
        self.scratch("scratch_repo", "-q")
        self.paths = self.write(".github/harness-aa-paths.txt", f"# the harness\n{HARNESS}\nworker/src/test/scala/scripts/IdentityDrift.scala\n")
        self.base = self.commit({"README": "r\n"}, "base")

    def tearDown(self):
        self.dir.cleanup()

    def scratch(self, helper, *args):
        """git through scripts/scratch-git.sh, never a raw git (a hook's GIT_DIR would point it at the shared clone)."""
        return subprocess.run(["bash", "-c", '. "$1"; "$2" "$3" "${@:4}"', "_", SCRATCH_GIT, helper, self.repo, *args],
                              env=self.env, check=True, capture_output=True, text=True).stdout.strip()

    def git(self, *args):
        return self.scratch("scratch_git", *args)

    def write(self, path, text):
        full = os.path.join(self.repo, path)
        os.makedirs(os.path.dirname(full) or self.repo, exist_ok=True)
        with open(full, "w") as f:
            f.write(text)
        return full

    def commit(self, files, message):
        for p, t in files.items():
            self.write(p, t)
        self.git("add", "-A")
        self.git("commit", "-qm", message)
        return self.git("rev-parse", "HEAD")

    def run_tool(self, *args, cwd=None):
        done = subprocess.run([sys.executable, "-I", TOOL, *args], cwd=cwd or self.repo, env=self.env,
                              capture_output=True, text=True)
        return done.returncode, done.stdout, done.stderr

    def decide(self, head, since=""):
        return self.run_tool("decide", "--head", head, "--since", since)

    def test_a_harness_change_runs_the_a_a_row(self):
        head = self.commit({HARNESS: "x\n"}, "harness")
        code, out, _ = self.decide(head)
        self.assertEqual(0, code)
        self.assertIn("harness-aa=true", out)
        self.assertIn(f"harness-aa-paths={HARNESS}", out)

    def test_a_change_elsewhere_runs_none(self):
        head = self.commit({"worker/src/main/A.scala": "a\n"}, "worker")
        self.assertIn("harness-aa=false", self.decide(head)[1])

    def test_a_harness_change_earlier_in_the_runs_range_counts(self):
        self.commit({HARNESS: "x\n"}, "harness")
        head = self.commit({"docs/a.md": "d\n"}, "a doc after it")
        self.assertIn("harness-aa=false", self.decide(head)[1], "the parent alone: only the doc")
        self.assertIn("harness-aa=true", self.decide(head, self.base)[1], "since the last convergence run's head")

    def test_an_unknown_since_falls_back_to_the_parent(self):
        head = self.commit({HARNESS: "x\n"}, "harness")
        code, out, err = self.decide(head, "f" * 40)
        self.assertEqual(0, code, err)
        self.assertIn("harness-aa=true", out)
        self.assertIn("parent", err)

    def judge(self, code, verdict):
        v = self.write("out/verdict.txt", verdict)
        return self.run_tool("judge", "--cc", "de", "--code", str(code), "--verdict", v)

    def test_two_replays_that_differ_are_the_harness_fault(self):
        code, out, _ = self.judge(1, "not deterministic: two replays of the same seed(s) differ in 3 TSV line(s)\n")
        self.assertEqual(1, code)
        self.assertIn("HARNESS FAULT", out)
        self.assertIn("::error", out)

    def test_a_clean_a_a_passes_and_a_divergence_is_the_drift_rows_verdict(self):
        self.assertEqual(0, self.judge(0, "no divergence\n")[0])
        code, out, _ = self.judge(1, "2 divergence(s): ...\n")
        self.assertEqual(0, code, "a divergence is the head's own, judged by takes-drift-1/2, not an A/A finding")
        self.assertIn("::notice", out)

    def test_a_check_that_could_not_run_is_no_verdict(self):
        self.assertEqual(2, self.judge(2, "")[0])

    def test_the_wiring_runs_the_a_a_row_on_a_harness_push_to_main(self):
        conv = open(os.path.join(ROOT, ".github/workflows/identity-model-convergence.yml")).read()
        leg = open(os.path.join(ROOT, ".github/workflows/country-convergence-leg.yml")).read()
        self.assertIn("scripts/ci/harness_aa.py decide", conv)
        self.assertIn("harness-aa: ${{ steps.aa.outputs.harness-aa || 'false' }}", conv)
        self.assertIn("harness-aa:                    ${{ needs.preflight.outputs.harness-aa == 'true' }}", conv)
        self.assertIn("inputs.harness-aa && ',\"takes-drift-aa\"'", leg)
        self.assertIn("scripts/ci/harness_aa.py judge", leg)
        rows = open(os.path.join(ROOT, ".github/gating-checks.tsv")).read()
        for country in ("poland", "germany", "united-kingdom", "spain", "united-states"):
            self.assertIn(f"job\tidentity-model-convergence.yml\ttakes-drift-aa ({country})\tunproven", rows)


if __name__ == "__main__":
    unittest.main()
