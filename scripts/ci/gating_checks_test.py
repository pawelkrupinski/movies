#!/usr/bin/env python3
"""gating_checks.py: the registry's lint against scratch workflows, a red run's report-only verdict, and `prove`."""

from __future__ import annotations

import json
import os
import subprocess
import sys
import tempfile
import unittest

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import gating_checks as g  # noqa: E402

MAIN = """name: Main
jobs:
    ci:
        uses: ./.github/workflows/ci.yml
        with:
            lane: 'main-lane'
    timings:
        needs: [ci]
        runs-on: ubuntu-26.04
"""
CI = """name: CI
on:
    workflow_call:
jobs:
    test:
        name: test (${{ matrix.shard }})
        runs-on: ${{ inputs.lane || 'ubuntu-26.04' }}
    commit-rules:
        name: commit rules
        runs-on: ${{ inputs.lane || 'ubuntu-26.04' }}
"""
CONV = """name: Identity model convergence
jobs:
    preflight:
        runs-on: gate-lane
"""
HOOK = 'GatingWorkflows=("Main" "Identity model convergence")\nGatingWorkflowFiles=("main.yml" "identity-model-convergence.yml")\n'
REGISTRY = """# the registry
kind\tworkflow\tname\tproven\tnote
workflow\tmain.yml\t-\t2722\tthe gate
workflow\tidentity-model-convergence.yml\t-\t2691\tthe gate
job\tmain.yml\tci\taggregate
job\tmain.yml\ttimings\taggregate
job\tmain.yml\ttest (heavy1)\t2722
job\tmain.yml\tcommit rules\tunproven\tborn today; owner: ci
job\tidentity-model-convergence.yml\tpreflight\t2691
"""


class Lint(unittest.TestCase):
    def setUp(self):
        self.dir = tempfile.TemporaryDirectory()
        self.root = self.dir.name
        self.write(".github/workflows/main.yml", MAIN)
        self.write(".github/workflows/ci.yml", CI)
        self.write(".github/workflows/identity-model-convergence.yml", CONV)
        self.write("scripts/hooks/pre-push", HOOK)
        self.write(".github/gating-checks.tsv", REGISTRY)

    def tearDown(self):
        self.dir.cleanup()

    def write(self, path, text):
        full = os.path.join(self.root, path)
        os.makedirs(os.path.dirname(full), exist_ok=True)
        with open(full, "w") as f:
            f.write(text)

    def test_a_registry_in_step_with_the_workflows_is_clean(self):
        self.assertEqual(g.lint(self.root), [])

    def test_a_new_job_without_its_row_is_refused(self):
        self.write(".github/workflows/ci.yml", CI + "    e2e:\n        name: e2e (${{ matrix.shard }})\n        runs-on: ubuntu-26.04\n")
        problems = g.lint(self.root)
        self.assertEqual(len(problems), 1, problems)
        self.assertIn("'e2e (*)' has no row", problems[0])

    def test_a_new_job_lands_as_unproven_with_its_owner(self):
        self.write(".github/workflows/ci.yml", CI + "    e2e:\n        name: e2e (${{ matrix.shard }})\n        runs-on: ubuntu-26.04\n")
        self.write(".github/gating-checks.tsv", REGISTRY + "job\tmain.yml\te2e (rest)\tunproven\tborn today; owner: e2e\n")
        self.assertEqual(g.lint(self.root), [])
        self.write(".github/gating-checks.tsv", REGISTRY + "job\tmain.yml\te2e (rest)\tunproven\n")
        self.assertIn("says who owns it", " ".join(g.lint(self.root)))

    def test_a_row_naming_no_job_is_stale(self):
        self.write(".github/gating-checks.tsv", REGISTRY + "job\tmain.yml\tscript self-tests\t2700\n")
        self.assertIn("'script self-tests' names no job", " ".join(g.lint(self.root)))

    def test_a_new_gate_workflow_needs_a_proven_row(self):
        self.write("scripts/hooks/pre-push", HOOK.replace('"identity-model-convergence.yml")', '"identity-model-convergence.yml" "recording.yml")'))
        self.assertIn("recording.yml gates pushes", " ".join(g.lint(self.root)))

    def test_a_malformed_proven_is_refused(self):
        self.write(".github/gating-checks.tsv", REGISTRY.replace("\t2722\n", "\tyesterday\n", 1))
        self.assertIn("proven 'yesterday'", " ".join(g.lint(self.root)))


class ReportOnly(unittest.TestCase):
    rows = None

    def setUp(self):
        with tempfile.NamedTemporaryFile("w", suffix=".tsv", delete=False) as f:
            f.write(REGISTRY)
        self.rows = g.load(f.name)
        os.unlink(f.name)

    def verdict(self, *failed):
        jobs = [{"name": n, "conclusion": "failure"} for n in failed] + [{"name": "test (heavy1)", "conclusion": "success"}]
        return g.report_only(self.rows, "main.yml", jobs)

    def test_a_red_only_in_an_unproven_job_and_its_aggregates_reports(self):
        only, why = self.verdict("commit rules", "ci", "timings")
        self.assertTrue(only, why)
        self.assertIn("commit rules", why)

    def test_a_job_the_registry_does_not_know_is_unproven(self):
        self.assertTrue(self.verdict("e2e (brand new)", "ci")[0])

    def test_a_proven_job_failing_gates(self):
        only, why = self.verdict("test (heavy1)", "commit rules", "ci")
        self.assertFalse(only)
        self.assertIn("test (heavy1)", why)

    def test_aggregates_alone_gate(self):
        self.assertFalse(self.verdict("ci")[0])

    def test_the_same_job_name_in_another_workflow_is_not_this_ones(self):
        rows = self.rows + [{"kind": "job", "workflow": "identity-model-convergence.yml", "name": "commit rules", "proven": "2691"}]
        self.assertTrue(g.report_only(rows, "main.yml", [{"name": "commit rules", "conclusion": "failure"}])[0])

    def test_the_cli_exit_says_it(self):
        with tempfile.TemporaryDirectory() as d:
            os.makedirs(os.path.join(d, ".github"))
            with open(os.path.join(d, ".github/gating-checks.tsv"), "w") as f:
                f.write(REGISTRY)
            jobs = os.path.join(d, "jobs.json")
            with open(jobs, "w") as f:
                json.dump({"jobs": [{"name": "commit rules", "conclusion": "failure"}]}, f)
            run = lambda: subprocess.run([sys.executable, "-I", os.path.join(HERE, "gating_checks.py"), "report-only",
                                          "--root", d, "--workflow-file", "main.yml", "--jobs-json", jobs],
                                         capture_output=True, text=True).returncode
            self.assertEqual(run(), 0)
            with open(jobs, "w") as f:
                json.dump({"jobs": [{"name": "test (heavy1)", "conclusion": "failure"}]}, f)
            self.assertEqual(run(), 1)
            os.unlink(os.path.join(d, ".github/gating-checks.tsv"))
            self.assertEqual(run(), 2)


class Prove(unittest.TestCase):
    def test_a_green_run_proves_its_unproven_and_missing_jobs(self):
        with tempfile.NamedTemporaryFile("w", suffix=".tsv", delete=False) as f:
            f.write(REGISTRY)
        rows = g.load(f.name)
        jobs = [{"name": "commit rules", "conclusion": "success"}, {"name": "e2e (rest)", "conclusion": "success"},
                {"name": "test (heavy1)", "conclusion": "success"}, {"name": "flaky", "conclusion": "failure"}]
        changed = g.prove(rows, "main.yml", jobs, "2800")
        self.assertEqual(changed, ["commit rules", "e2e (rest)"])
        state = {(r["kind"], r["name"]): r["proven"] for r in rows}
        self.assertEqual(state[("job", "commit rules")], "2800")
        self.assertEqual(state[("job", "test (heavy1)")], "2722")
        self.assertNotIn(("job", "flaky"), state)
        g.save(f.name, rows)
        self.assertEqual(g.load(f.name)[-1]["name"], "e2e (rest)")
        os.unlink(f.name)


if __name__ == "__main__":
    unittest.main()
