#!/usr/bin/env python3
"""Tests for scripts/ci/convergence_paths.py over a scratch tree. Run: python3 scripts/ci/convergence_paths_test.py"""
import importlib.util
import os
import tempfile
import unittest

HERE = os.path.dirname(os.path.abspath(__file__))
spec = importlib.util.spec_from_file_location("cp", os.path.join(HERE, "convergence_paths.py"))
cp = importlib.util.module_from_spec(spec)
spec.loader.exec_module(cp)


class Generate(unittest.TestCase):
    def setUp(self):
        self.root = tempfile.mkdtemp()
        self.write(".github/convergence-paths.txt", "# hand\nworker/src/main/**\n.github/actions/whole/**\n")
        self.write(".github/workflows/identity-model-convergence.yml",
                   "jobs:\n  a:\n    steps:\n      - uses: ./.github/actions/setup\n      - uses: ./.github/actions/whole\n"
                   "      # scripts/ci/commented.sh\n      - uses: ./.github/actions/report-timings\n")
        self.write(".github/workflows/country-convergence-leg.yml", "")
        self.write(".github/actions/setup/action.yml", "runs:\n  steps:\n    - run: scripts/ci/helper.sh\n")
        self.write(".github/actions/whole/action.yml", "runs:\n  steps:\n    - run: scripts/ci/whole-helper.sh\n")
        self.write(".github/actions/report-timings/action.yml", "runs:\n  steps:\n    - run: scripts/ci/after.py\n")
        for f in ("scripts/ci/helper.sh", "scripts/ci/whole-helper.sh", "scripts/ci/after.py", "scripts/ci/commented.sh"):
            self.write(f, "")

    def write(self, path, text):
        full = os.path.join(self.root, path)
        os.makedirs(os.path.dirname(full), exist_ok=True)
        with open(full, "w") as f:
            f.write(text)

    def read(self):
        with open(os.path.join(self.root, cp.PATHS)) as f:
            return f.read()

    def test_write_lists_what_the_workflows_run_transitively_and_check_then_passes(self):
        self.assertEqual(cp.main(["--root", self.root, "--check"]), 1)
        cp.main(["--root", self.root, "--write"])
        block = cp.split(self.read())[1]
        self.assertIn(".github/actions/setup/action.yml", block)
        self.assertIn("scripts/ci/helper.sh", block)
        self.assertIn("scripts/ci/whole-helper.sh", block, "a file a whole-listed action runs is still listed")
        self.assertEqual(cp.main(["--root", self.root, "--check"]), 0)

    def test_hand_listed_paths_comments_and_after_the_verdict_files_stay_out(self):
        cp.main(["--root", self.root, "--write"])
        block = cp.split(self.read())[1]
        self.assertNotIn(".github/actions/whole/action.yml", block, "covered by the hand-written glob")
        self.assertNotIn("scripts/ci/commented.sh", block)
        self.assertNotIn("scripts/ci/after.py", block)
        self.assertTrue(self.read().startswith("# hand\nworker/src/main/**\n"))

    def test_a_stale_block_fails_the_check(self):
        cp.main(["--root", self.root, "--write"])
        self.write(".github/actions/setup/action.yml", "runs:\n  steps:\n    - run: scripts/ci/commented.sh\n")
        self.assertEqual(cp.main(["--root", self.root, "--check"]), 1)


if __name__ == "__main__":
    unittest.main()
