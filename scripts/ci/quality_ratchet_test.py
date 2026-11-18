#!/usr/bin/env python3
"""quality_ratchet.py: a measured row past its baseline row fails, and so does a row-less key past its kind's limit (a
NEW method over a limit) and a measure whose limit rose past the baseline's; a baseline grown in a range passes only
when a commit changing it carries the ratchet's trailer (`Complexity: <why>`); falls pass and are said; `update` writes
the baseline. QUALITY_RATCHET_UNDER_TEST names another copy of the tool (quality-ratchet-mutation-test.sh's mutants)."""

from __future__ import annotations

import os
import subprocess
import sys
import tempfile
import unittest

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(os.path.dirname(HERE))
SCRATCH_GIT = os.path.join(ROOT, "scripts/scratch-git.sh")
TOOL = os.environ.get("QUALITY_RATCHET_UNDER_TEST") or os.path.join(HERE, "quality_ratchet.py")
COMPLEXITY = "test/resources/complexity-baseline.tsv"

ROWS = """# a comment
limit\tcognitive\t15
limit\tcyclomatic\t10
limit\tinner-defs\t2
cognitive\ta/src/main/scala/A.scala:A.f\t20
cyclomatic\ta/src/main/scala/A.scala:A.f\t12
inner-defs\ta/src/main/scala/B.scala:B.g\t4
"""


def rows(*extra: str, drop: tuple = ()) -> str:
    kept = [line for line in ROWS.splitlines() if not any(line.startswith(d) for d in drop)]
    return "\n".join(kept + list(extra)) + "\n"


class QualityRatchetTest(unittest.TestCase):
    def setUp(self):
        self.saved = {k: os.environ.pop(k) for k in list(os.environ) if k.startswith("GIT_")}
        self.dir = tempfile.TemporaryDirectory()
        self.repo = os.path.join(self.dir.name, "repo")
        self.scratch("scratch_repo", "-q")
        self.base = self.commit({COMPLEXITY: ROWS}, "base")

    def tearDown(self):
        self.dir.cleanup()
        os.environ.update(self.saved)

    def scratch(self, helper, *args):
        return subprocess.run(["bash", "-c", '. "$1"; "$2" "$3" "${@:4}"', "_", SCRATCH_GIT, helper, self.repo, *args],
                              check=True, capture_output=True, text=True).stdout.strip()

    def commit(self, files, message):
        for path, text in files.items():
            full = os.path.join(self.repo, path)
            os.makedirs(os.path.dirname(full), exist_ok=True)
            with open(full, "w") as f:
                f.write(text)
        self.scratch("scratch_git", "add", "-A")
        self.scratch("scratch_git", "commit", "-qm", message)
        return self.scratch("scratch_git", "rev-parse", "HEAD")

    def run_tool(self, command, current, *extra):
        path = os.path.join(self.dir.name, "current.tsv")
        with open(path, "w") as f:
            f.write(current)
        env = {k: v for k, v in os.environ.items() if not k.startswith("GIT_")}
        env.update(GIT_CONFIG_GLOBAL="/dev/null", GIT_CONFIG_NOSYSTEM="1")
        done = subprocess.run([sys.executable, "-I", TOOL, command, "--current", path, *extra], cwd=self.repo, env=env,
                              capture_output=True, text=True)
        return done.returncode, done.stdout + done.stderr

    def check(self, current, rng=None):
        return self.run_tool("check", current, *(["--range", rng] if rng else []))

    def assert_fails(self, current, *said, rng=None):
        rc, out = self.check(current, rng)
        self.assertEqual(1, rc, out)
        for s in said:
            self.assertIn(s, out)

    def assert_passes(self, current, rng=None):
        rc, out = self.check(current, rng)
        self.assertEqual(0, rc, out)
        return out

    # ── the measured rows against the baseline ──

    def test_the_baseline_itself_passes(self):
        self.assertIn("complexity: within", self.assert_passes(ROWS))

    def test_a_new_method_over_a_limit_fails(self):
        self.assert_fails(rows("cognitive\ta/src/main/scala/C.scala:C.h\t16"),
                          "cognitive a/src/main/scala/C.scala:C.h: 16, past the limit 15 (no baseline row)")

    def test_a_new_method_with_too_many_inner_defs_fails(self):
        self.assert_fails(rows("inner-defs\ta/src/main/scala/C.scala:C.h\t3"), "inner-defs a/src/main/scala/C.scala:C.h: 3")

    def test_a_source_moved_to_another_package_keeps_its_rows(self):
        moved = rows("cognitive\tb/src/main/scala/x/A.scala:A.f\t20", "cyclomatic\tb/src/main/scala/x/A.scala:A.f\t12",
                     "method-lines\tb/src/main/scala/x/A.scala:A.f\t70", "file-lines\tb/src/main/scala/x/A.scala\t900",
                     drop=("cognitive\ta/src/main/scala/A", "cyclomatic\ta/", "method-lines\ta/", "file-lines\ta/"))
        self.assert_passes(moved)
        head = self.commit({COMPLEXITY: moved, LENGTH: moved}, "move A to package x")
        self.assert_passes(moved, rng=f"{self.base}..{head}")

    def test_a_moved_source_growing_fails(self):
        self.assert_fails(rows("cognitive\tb/src/main/scala/x/A.scala:A.f\t21", drop=("cognitive\ta/src/main/scala/A",)),
                          "cognitive b/src/main/scala/x/A.scala:A.f: 21, past the limit 15 (no baseline row)")
        # ...and a copy beside the original is no move: the original is still measured
        self.assert_fails(rows("cognitive\tb/src/main/scala/x/A.scala:A.f\t20"), "b/src/main/scala/x/A.scala:A.f: 20")

    def test_a_frozen_method_rising_fails(self):
        self.assert_fails(rows("cyclomatic\ta/src/main/scala/A.scala:A.f\t13", drop=("cyclomatic\ta/",)),
                          "cyclomatic a/src/main/scala/A.scala:A.f: 13, past its baseline 12")

    def test_a_frozen_method_at_its_row_passes(self):
        self.assert_passes(ROWS)

    def test_a_measure_whose_limit_rose_fails(self):
        self.assert_fails(rows("limit\tcognitive\t25", drop=("limit\tcognitive",)),
                          "the measure's cognitive limit 25 is past the baseline's 15")

    def test_falls_pass_and_are_said(self):
        out = self.assert_passes(rows("cognitive\ta/src/main/scala/A.scala:A.f\t18", drop=("cognitive\ta/", "inner-defs")))
        self.assertIn("complexity fell", out)
        self.assertIn("cognitive a/src/main/scala/A.scala:A.f 20 -> 18", out)
        self.assertIn("inner-defs a/src/main/scala/B.scala:B.g 4 -> within the limit", out)

    # ── the baseline's growth over a range ──

    def test_a_grown_baseline_without_a_trailer_fails(self):
        grown = rows("cognitive\ta/src/main/scala/C.scala:C.h\t16")
        head = self.commit({COMPLEXITY: grown}, "a hairy method")
        self.assert_fails(grown, "THE COMPLEXITY BASELINE GREW", "cognitive a/src/main/scala/C.scala:C.h raised <=15 -> 16",
                          rng=f"{self.base}..{head}")

    def test_a_grown_baseline_with_a_trailer_passes(self):
        grown = rows("cognitive\ta/src/main/scala/A.scala:A.f\t22", drop=("cognitive\ta/",))
        head = self.commit({COMPLEXITY: grown}, "a hairy method\n\nComplexity: the parser's one state machine")
        self.assert_passes(grown, rng=f"{self.base}..{head}")

    def test_another_ratchets_trailer_does_not_count(self):
        grown = rows("cognitive\ta/src/main/scala/A.scala:A.f\t22", drop=("cognitive\ta/",))
        head = self.commit({COMPLEXITY: grown}, "a hairy method\n\nCoupling: not this one")
        self.assert_fails(grown, "cognitive a/src/main/scala/A.scala:A.f raised 20 -> 22", rng=f"{self.base}..{head}")

    def test_a_trailer_on_a_commit_not_changing_the_baseline_does_not_count(self):
        grown = rows("cognitive\ta/src/main/scala/C.scala:C.h\t16")
        self.commit({COMPLEXITY: grown}, "a hairy method")
        head = self.commit({"README": "x\n"}, "docs\n\nComplexity: not this commit's")
        self.assert_fails(grown, "C.scala:C.h raised", rng=f"{self.base}..{head}")

    def test_an_empty_trailer_does_not_count(self):
        grown = rows("cognitive\ta/src/main/scala/C.scala:C.h\t16")
        head = self.commit({COMPLEXITY: grown}, "a hairy method\n\nComplexity: ")
        self.assert_fails(grown, "C.scala:C.h raised", rng=f"{self.base}..{head}")

    def test_a_raised_limit_is_growth(self):
        grown = rows("limit\tcognitive\t25", drop=("limit\tcognitive",))
        head = self.commit({COMPLEXITY: grown}, "looser")
        self.assert_fails(grown, "limit cognitive raised 15 -> 25", rng=f"{self.base}..{head}")

    def test_a_shrunk_baseline_needs_no_trailer(self):
        shrunk = rows(drop=("inner-defs",))
        head = self.commit({COMPLEXITY: shrunk}, "split B.g")
        self.assert_passes(shrunk, rng=f"{self.base}..{head}")

    # ── update, and malformed input ──

    def test_update_writes_the_measured_rows_under_the_header(self):
        rc, out = self.run_tool("update", rows("cognitive\ta/src/main/scala/C.scala:C.h\t16"))
        self.assertEqual(0, rc, out)
        with open(os.path.join(self.repo, COMPLEXITY)) as f:
            written = f.read()
        self.assertTrue(written.startswith("# THE COMPLEXITY BASELINE"))
        self.assertIn("limit\tcognitive\t15\n", written)
        self.assertIn("cognitive\ta/src/main/scala/C.scala:C.h\t16\n", written)
        self.assertEqual(0, self.check(rows("cognitive\ta/src/main/scala/C.scala:C.h\t16"))[0])

    def test_a_malformed_row_is_an_error_not_a_pass(self):
        rc, out = self.check(ROWS + "cognitive\tx\n")
        self.assertEqual(2, rc, out)

    def test_a_measure_without_a_limit_row_is_an_error_not_a_pass(self):
        rc, out = self.check(rows(drop=("limit\tcyclomatic",)))
        self.assertEqual(2, rc, out)


if __name__ == "__main__":
    unittest.main()
