#!/usr/bin/env python3
"""quality_ratchet.py: a measured row past its baseline row fails, and so does a row-less key past its kind's limit (a
NEW method over a limit) and a measure whose limit rose past the baseline's; a baseline grown in a range passes only
when a commit changing it carries the ratchet's trailer (`Complexity: <why>`, `Length: <why>`), each ratchet
judged on its own rows, baseline and trailer; a source moved keeps its rows only when git's rename detection followed it
and no value rose; falls pass and are said; `update` writes
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
LENGTH = "test/resources/length-baseline.tsv"
DUPLICATION = "test/resources/duplication-baseline.tsv"

ROWS = """# a comment
limit\tcognitive\t15
limit\tcyclomatic\t10
limit\tinner-defs\t2
cognitive\ta/src/main/scala/A.scala:A.f\t20
cyclomatic\ta/src/main/scala/A.scala:A.f\t12
inner-defs\ta/src/main/scala/B.scala:B.g\t4
limit\tmethod-lines\t50
limit\tfile-lines\t800
method-lines\ta/src/main/scala/A.scala:A.f\t70
file-lines\ta/src/main/scala/A.scala\t900
limit\tcopies\t0
copies\ta/src/main/scala/A.scala|a/src/main/scala/B.scala\t2
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
        self.base = self.commit({COMPLEXITY: ROWS, LENGTH: ROWS, DUPLICATION: ROWS}, "base")

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
        out = self.assert_passes(ROWS)
        self.assertIn("complexity: within", out)
        self.assertIn("length: within", out)
        self.assertIn("duplication: within", out)

    def test_a_new_method_over_a_limit_fails(self):
        self.assert_fails(rows("cognitive\ta/src/main/scala/C.scala:C.h\t16"),
                          "cognitive a/src/main/scala/C.scala:C.h: 16, past the limit 15 (no baseline row)")

    def test_a_new_method_with_too_many_inner_defs_fails(self):
        self.assert_fails(rows("inner-defs\ta/src/main/scala/C.scala:C.h\t3"), "inner-defs a/src/main/scala/C.scala:C.h: 3")

    # A move: the baseline's sources committed, then `git mv`d (or replaced) in a commit of their own -- git's rename
    # detection, over the repository's history, is what pairs a vanished row with its new key.
    A_OLD, A_NEW = "a/src/main/scala/A.scala", "b/src/main/scala/x/A.scala"
    A_SOURCE = "".join(f"object A {{ def f{i}(x: Int): Int = x * {i} + {i * 7} }}\n" for i in range(30))
    MOVED = ("cognitive\tb/src/main/scala/x/A.scala:A.f\t20", "cyclomatic\tb/src/main/scala/x/A.scala:A.f\t12",
             "method-lines\tb/src/main/scala/x/A.scala:A.f\t70", "file-lines\tb/src/main/scala/x/A.scala\t900",
             "copies\ta/src/main/scala/B.scala|b/src/main/scala/x/A.scala\t2")
    MOVED_FROM = ("cognitive\ta/src/main/scala/A", "cyclomatic\ta/", "method-lines\ta/", "file-lines\ta/", "copies\ta/")

    def move_a(self, new_text=None):
        """A.scala and B.scala committed, then A.scala moved to package x: `git mv` (new_text None), or deleted and an
        unrelated source of the same name written there."""
        self.commit({self.A_OLD: self.A_SOURCE, "a/src/main/scala/B.scala": "object B\n"}, "the sources")
        if new_text is None:
            os.makedirs(os.path.dirname(os.path.join(self.repo, self.A_NEW)), exist_ok=True)
            self.scratch("scratch_git", "mv", self.A_OLD, self.A_NEW)
            self.scratch("scratch_git", "commit", "-qm", "move A to package x")
        else:
            os.remove(os.path.join(self.repo, self.A_OLD))
            self.commit({self.A_NEW: new_text}, "drop A, write another A in package x")
        return self.scratch("scratch_git", "rev-parse", "HEAD")

    def test_a_source_moved_to_another_package_keeps_its_rows(self):
        self.move_a()
        moved = rows(*self.MOVED, drop=self.MOVED_FROM)
        self.assert_passes(moved)
        head = self.commit({COMPLEXITY: moved, LENGTH: moved, DUPLICATION: moved}, "the baselines follow the move")
        self.assert_passes(moved, rng=f"{self.base}..{head}")

    def test_a_moved_source_lowering_a_value_keeps_its_rows(self):
        self.move_a()
        self.assert_passes(rows("cognitive\tb/src/main/scala/x/A.scala:A.f\t18", drop=("cognitive\ta/src/main/scala/A",)))

    def test_a_moved_source_growing_fails(self):
        # THE MOVE LOOPHOLE: a source moved and one method's score raised, no trailer -- a rise, not a move
        self.move_a()
        raised = rows("cognitive\tb/src/main/scala/x/A.scala:A.f\t21", drop=("cognitive\ta/src/main/scala/A",))
        self.assert_fails(raised, "cognitive b/src/main/scala/x/A.scala:A.f: 21, past the limit 15 (no baseline row)")
        for kind, row in (("method-lines", "method-lines\tb/src/main/scala/x/A.scala:A.f\t71"),
                          ("file-lines", "file-lines\tb/src/main/scala/x/A.scala\t901"),
                          ("copies", "copies\ta/src/main/scala/B.scala|b/src/main/scala/x/A.scala\t3")):
            self.assert_fails(rows(row, drop=(f"{kind}\ta/",)), row.split("\t")[1])
        # ...and a copy beside the original is no move: the original is still measured
        self.assert_fails(rows("cognitive\tb/src/main/scala/x/A.scala:A.f\t20"), "b/src/main/scala/x/A.scala:A.f: 20")
        # ...and recorded in the baselines with no trailer, the growth fails the range
        head = self.commit({COMPLEXITY: raised}, "move A, a hairier A.f")
        self.assert_fails(raised, "cognitive b/src/main/scala/x/A.scala:A.f raised <=15 -> 21", rng=f"{self.base}..{head}")

    def test_a_same_named_source_git_does_not_call_a_rename_is_no_move(self):
        self.move_a("".join(f"object A {{ val other{i} = \"{i}\" }}\n" for i in range(30)))
        unrelated = rows(*self.MOVED[:2], drop=self.MOVED_FROM[:2])
        self.assert_fails(unrelated, "cognitive b/src/main/scala/x/A.scala:A.f: 20, past the limit 15 (no baseline row)")
        head = self.commit({COMPLEXITY: unrelated}, "the baseline re-keyed")
        self.assert_fails(unrelated, "cognitive b/src/main/scala/x/A.scala:A.f raised <=15 -> 20", rng=f"{self.base}..{head}")

    def test_a_moved_source_keeps_only_its_own_method_names(self):
        self.move_a()
        self.assert_fails(rows("cognitive\tb/src/main/scala/x/A.scala:A.renamed\t20", drop=("cognitive\ta/src/main/scala/A",)),
                          "A.renamed: 20, past the limit 15")

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

    def test_a_new_long_method_fails(self):
        self.assert_fails(rows("method-lines\ta/src/main/scala/C.scala:C.h\t51"),
                          "LENGTH GREW", "method-lines a/src/main/scala/C.scala:C.h: 51, past the limit 50 (no baseline row)")

    def test_a_new_long_file_fails(self):
        self.assert_fails(rows("file-lines\ta/src/main/scala/C.scala\t801"),
                          "file-lines a/src/main/scala/C.scala: 801, past the limit 800 (no baseline row)")

    def test_a_frozen_file_growing_fails_and_shrinking_passes(self):
        self.assert_fails(rows("file-lines\ta/src/main/scala/A.scala\t901", drop=("file-lines\ta/",)),
                          "file-lines a/src/main/scala/A.scala: 901, past its baseline 900")
        out = self.assert_passes(rows("file-lines\ta/src/main/scala/A.scala\t850", drop=("file-lines\ta/",)))
        self.assertIn("file-lines a/src/main/scala/A.scala 900 -> 850", out)

    def test_a_new_clone_group_fails(self):
        self.assert_fails(rows("copies\ta/src/main/scala/C.scala|a/src/main/scala/D.scala\t2"), "DUPLICATION GREW",
                          "copies a/src/main/scala/C.scala|a/src/main/scala/D.scala: 2, past the limit 0 (no baseline row)")

    def test_a_clone_group_gaining_a_copy_fails(self):
        self.assert_fails(rows("copies\ta/src/main/scala/A.scala|a/src/main/scala/B.scala\t3", drop=("copies\ta/",)),
                          "copies a/src/main/scala/A.scala|a/src/main/scala/B.scala: 3, past its baseline 2")
        # a copy in a third file: another file set, a new row
        self.assert_fails(rows("copies\ta/src/main/scala/A.scala|a/src/main/scala/B.scala|a/src/main/scala/C.scala\t3",
                               drop=("copies\ta/",)), "a/src/main/scala/C.scala: 3, past the limit 0")

    def test_an_extracted_clone_is_a_fall(self):
        out = self.assert_passes(rows(drop=("copies\ta/",)))
        self.assertIn("copies a/src/main/scala/A.scala|a/src/main/scala/B.scala 2 -> within the limit", out)

    def test_a_grown_duplication_baseline_needs_its_trailer(self):
        grown = rows("copies\ta/src/main/scala/C.scala|a/src/main/scala/D.scala\t2")
        head = self.commit({DUPLICATION: grown}, "a copy\n\nLength: not this ratchet's")
        self.assert_fails(grown, "THE DUPLICATION BASELINE GREW", rng=f"{self.base}..{head}")
        restored = self.commit({DUPLICATION: ROWS}, "no copy")
        head = self.commit({DUPLICATION: grown}, "a copy\n\nDuplication: two clients of one vendor's markup")
        self.assert_passes(grown, rng=f"{restored}..{head}")

    def test_one_ratchet_can_be_checked_alone(self):
        rc, out = self.run_tool("check", rows("file-lines\ta/src/main/scala/C.scala\t801"), "--ratchet", "complexity")
        self.assertEqual(0, rc, out)
        self.assertNotIn("length", out)

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

    def test_a_trailer_outside_the_final_block_is_refused_saying_where_it_is(self):
        grown = rows("cognitive\ta/src/main/scala/A.scala:A.f\t22", drop=("cognitive\ta/",))
        head = self.commit({COMPLEXITY: grown}, "a hairy method\n\nComplexity: the parser's one state machine\n\n"
                                                "Co-Authored-By: A <a@b.c>")
        self.assert_fails(grown, "THE COMPLEXITY BASELINE GREW",
                          f"{head[:9]}: found `Complexity: the parser's one state machine` on line 3, but it is not in"
                          " the final trailer block (it must sit with Co-Authored-By, no blank line between); git does"
                          " not read it as a trailer", rng=f"{self.base}..{head}")

    def test_a_trailer_beside_co_authored_by_passes(self):
        grown = rows("cognitive\ta/src/main/scala/A.scala:A.f\t22", drop=("cognitive\ta/",))
        head = self.commit({COMPLEXITY: grown}, "a hairy method\n\nComplexity: the parser's one state machine\n"
                                                "Co-Authored-By: A <a@b.c>")
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

    # ── a batch push: each rise justified on the commit making it (trailers.py, THE ATTRIBUTION RULE) ──

    def test_a_batch_where_only_the_other_commit_carries_the_trailer_fails_naming_the_raiser(self):
        own = rows("cognitive\ta/src/main/scala/A.scala:A.f\t22", drop=("cognitive\ta/",))
        self.commit({COMPLEXITY: own}, "change A: a hairy method\n\nComplexity: the parser's one state machine")
        both = own + "cognitive\ta/src/main/scala/C.scala:C.h\t16\n"
        head = self.commit({COMPLEXITY: both}, "change B: another hairy method")
        self.assert_fails(both, "THE COMPLEXITY BASELINE GREW", "cognitive a/src/main/scala/C.scala:C.h raised <=15 -> 16",
                          f"{head[:9]} change B: another hairy method: cognitive a/src/main/scala/C.scala:C.h raised"
                          " <=15 -> 16", rng=f"{self.base}..{head}")

    def test_a_batch_where_the_later_commit_carries_the_trailer_for_the_earlier_ones_rise_fails(self):
        grown = rows("cognitive\ta/src/main/scala/C.scala:C.h\t16")
        first = self.commit({COMPLEXITY: grown}, "change A: a hairy method")
        regrown = grown.replace("A.f\t20", "A.f\t21")
        head = self.commit({COMPLEXITY: regrown}, "change B\n\nComplexity: B's own")
        _, out = self.check(regrown, f"{self.base}..{head}")
        self.assert_fails(regrown, f"{first[:9]} change A: a hairy method: cognitive", rng=f"{self.base}..{head}")
        self.assertNotIn(f"{head[:9]} change B", out)

    def test_a_batch_whose_commits_each_justify_their_own_rise_passes(self):
        own = rows("cognitive\ta/src/main/scala/A.scala:A.f\t22", drop=("cognitive\ta/",))
        self.commit({COMPLEXITY: own}, "change A\n\nComplexity: the parser's one state machine")
        both = own + "cognitive\ta/src/main/scala/C.scala:C.h\t16\n"
        head = self.commit({COMPLEXITY: both}, "change B\n\nComplexity: the scheduler's one table")
        self.assert_passes(both, rng=f"{self.base}..{head}")

    def test_a_batch_of_a_justified_rise_and_a_trailerless_fall_passes(self):
        own = rows("cognitive\ta/src/main/scala/A.scala:A.f\t22", drop=("cognitive\ta/",))
        self.commit({COMPLEXITY: own}, "change A\n\nComplexity: the parser's one state machine")
        shrunk = "".join(line + "\n" for line in own.splitlines() if not line.startswith("inner-defs"))
        self.commit({"README": "x\n"}, "change B: docs")
        head = self.commit({COMPLEXITY: shrunk}, "change C: split B.g")
        self.assert_passes(shrunk, rng=f"{self.base}..{head}")

    def test_an_empty_trailer_does_not_count(self):
        grown = rows("cognitive\ta/src/main/scala/C.scala:C.h\t16")
        head = self.commit({COMPLEXITY: grown}, "a hairy method\n\nComplexity: ")
        self.assert_fails(grown, "C.scala:C.h raised", rng=f"{self.base}..{head}")

    def test_a_raised_limit_is_growth(self):
        grown = rows("limit\tcognitive\t25", drop=("limit\tcognitive",))
        head = self.commit({COMPLEXITY: grown}, "looser")
        self.assert_fails(grown, "limit cognitive raised 15 -> 25", rng=f"{self.base}..{head}")

    def test_a_grown_length_baseline_needs_the_length_trailer_not_the_complexity_one(self):
        grown = rows("file-lines\ta/src/main/scala/C.scala\t801")
        head = self.commit({LENGTH: grown}, "a long file\n\nComplexity: not this ratchet's")
        self.assert_fails(grown, "THE LENGTH BASELINE GREW", "file-lines a/src/main/scala/C.scala raised <=800 -> 801",
                          rng=f"{self.base}..{head}")
        restored = self.commit({LENGTH: ROWS}, "a short file")
        head = self.commit({LENGTH: grown}, "a long file\n\nLength: the generated roster's one table")
        self.assert_passes(grown, rng=f"{restored}..{head}")

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
