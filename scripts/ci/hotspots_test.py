#!/usr/bin/env python3
"""hotspots.py: sources ranked by relative churn x cognitive complexity over the window; change-coupling pairs only
across modules or packages, at the shared-commit floor, with their co-change degree; sweeps and test code left out."""

from __future__ import annotations

import os
import subprocess
import sys
import tempfile
import unittest

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(os.path.dirname(HERE))
SCRATCH_GIT = os.path.join(ROOT, "scripts/scratch-git.sh")
TOOL = os.path.join(HERE, "hotspots.py")

A = "worker/src/main/scala/a/A.scala"
B = "common/src/main/scala/b/B.scala"
C = "worker/src/main/scala/a/C.scala"
T = "worker/src/test/scala/a/ASpec.scala"


class HotspotsTest(unittest.TestCase):
    def setUp(self):
        self.saved = {k: os.environ.pop(k) for k in list(os.environ) if k.startswith("GIT_")}
        self.dir = tempfile.TemporaryDirectory()
        self.repo = os.path.join(self.dir.name, "repo")
        self.scratch("scratch_repo", "-q")
        self.n = 0

    def tearDown(self):
        self.dir.cleanup()
        os.environ.update(self.saved)

    def scratch(self, helper, *args):
        return subprocess.run(["bash", "-c", '. "$1"; "$2" "$3" "${@:4}"', "_", SCRATCH_GIT, helper, self.repo, *args],
                              check=True, capture_output=True, text=True).stdout.strip()

    def touch(self, *paths, lines=1):
        self.n += 1
        for path in paths:
            full = os.path.join(self.repo, path)
            os.makedirs(os.path.dirname(full), exist_ok=True)
            with open(full, "a") as f:
                f.write("".join(f"// {self.n}.{i}\n" for i in range(lines)))
        self.scratch("scratch_git", "add", "-A")
        self.scratch("scratch_git", "commit", "-qm", f"change {self.n}")

    def run_tool(self, files, *extra):
        path = os.path.join(self.dir.name, "files.tsv")
        with open(path, "w") as f:
            f.write("".join(f"{p}\t{lines}\t{cog}\n" for p, (lines, cog) in files.items()))
        env = {k: v for k, v in os.environ.items() if not k.startswith("GIT_")}
        env.update(GIT_CONFIG_GLOBAL="/dev/null", GIT_CONFIG_NOSYSTEM="1")
        done = subprocess.run([sys.executable, "-I", TOOL, "--files", path, *extra], cwd=self.repo, env=env,
                              capture_output=True, text=True)
        self.assertEqual(0, done.returncode, done.stderr)
        return done.stdout

    def test_hotspots_rank_by_relative_churn_times_complexity(self):
        self.touch(A, lines=10)
        self.touch(B, lines=10)
        self.touch(C, lines=10)
        self.touch(T, lines=50)
        # A: churn 10 over 100 lines x 50 = 5; B: 10 / 10 x 20 = 20; C: 10 / 10 x 0 = 0
        out = self.run_tool({A: (100, 50), B: (10, 20), C: (10, 0)})
        rows = [line for line in out.splitlines() if line.startswith("| 1 |") or line.startswith("| 2 |")][:2]
        self.assertIn(f"`{B}` | 20 | 10 | 10 | 20 |", rows[0])
        self.assertIn(f"`{A}` | 5 | 10 | 100 | 50 |", rows[1])
        self.assertNotIn(T, out)

    def test_change_coupling_pairs_across_packages_at_the_floor(self):
        for _ in range(5):
            self.touch(A, B)
        self.touch(A)
        for _ in range(5):
            self.touch(A, C)  # one package: not coupling worth naming
        out = self.run_tool({A: (10, 1), B: (10, 1), C: (10, 1)}, "--min-shared", "5")
        # A changed 11 times, B 5: 5 shared over a mean of 8
        self.assertIn(f"| 1 | `{B}` | `{A}` | 5 | 62% |", out)
        self.assertNotIn(f"`{A}` | `{C}`", out)
        self.assertNotIn("| 1 |", self.run_tool({A: (10, 1), C: (10, 1)}).split("## Change coupling")[1])  # B is gone
        self.assertNotIn("| 1 |", self.run_tool({A: (10, 1), B: (10, 1)}, "--min-shared", "6").split("## Change coupling")[1])

    def test_a_sweep_is_no_coupling(self):
        for _ in range(5):
            self.touch(A, B)
        out = self.run_tool({A: (10, 1), B: (10, 1)}, "--max-commit-files", "1")
        self.assertNotIn("| 1 |", out.split("## Change coupling")[1])


if __name__ == "__main__":
    unittest.main()
