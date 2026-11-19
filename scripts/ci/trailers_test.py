#!/usr/bin/env python3
"""trailers.py: a known trailer key (`Coupling:`, `Fixes-Red:`, ...) written outside the message's final trailer block --
a paragraph of its own above `Co-Authored-By:`, indented, in the body -- is found with its line number and said plainly;
one in the final block, beside Co-Authored-By with no blank line between, is a trailer and is not; `lint` warns about
every misplaced line of a range and always exits 0."""

from __future__ import annotations

import os
import subprocess
import sys
import tempfile
import unittest

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(os.path.dirname(HERE))
SCRATCH_GIT = os.path.join(ROOT, "scripts/scratch-git.sh")
TOOL = os.path.join(HERE, "trailers.py")
sys.path.insert(0, HERE)
import trailers  # noqa: E402

CO = "Co-Authored-By: A <a@b.c>"


class MisplacedTest(unittest.TestCase):
    def test_a_trailer_in_a_paragraph_of_its_own_is_misplaced_with_its_line(self):
        found = trailers.misplaced(f"split it\n\nCoupling: split of X\n\n{CO}\n")
        self.assertEqual([("Coupling", 3, "Coupling: split of X")], [(m.key, m.line, m.text) for m in found])

    def test_the_explanation_names_the_line_and_the_rule(self):
        (m,) = trailers.misplaced(f"split it\n\nCoupling: split of X\n\n{CO}\n")
        self.assertEqual("found `Coupling: split of X` on line 3, but it is not in the final trailer block (it must sit"
                         " with Co-Authored-By, no blank line between); git does not read it as a trailer",
                         trailers.explain(m))

    def test_a_trailer_beside_co_authored_by_is_placed(self):
        self.assertEqual([], trailers.misplaced(f"split it\n\nSome prose.\n\nCoupling: split of X\n{CO}\n"))

    def test_every_known_key_is_judged_and_case_does_not_hide_one(self):
        message = "s\n\n" + "\n".join(f"{k.lower()}: v" for k in trailers.KNOWN_KEYS) + f"\nprose stops the block\n\n{CO}\n"
        self.assertEqual(list(trailers.KNOWN_KEYS), [m.key for m in trailers.misplaced(message)])

    def test_an_indented_trailer_in_the_final_block_is_folded_into_the_one_above_not_read(self):
        self.assertEqual([("Complexity", 4)],
                         [(m.key, m.line) for m in trailers.misplaced(f"s\n\n{CO}\n  Complexity: why\n")])

    def test_a_final_paragraph_git_reads_no_trailers_from_misplaces_its_key_lines(self):
        found = trailers.misplaced("s\n\nThis paragraph is prose, not trailers at all, and long.\nMore prose here.\n"
                                   "And more prose.\nLength: why\n")
        self.assertEqual([("Length", 6)], [(m.key, m.line) for m in found])

    def test_a_key_mentioned_mid_line_or_an_unknown_key_is_no_trailer_line(self):
        self.assertEqual([], trailers.misplaced(f"s\n\nIt explains the Coupling: trailer in prose.\nSee: also\n\n{CO}\n"))

    def test_the_subject_is_never_judged(self):
        self.assertEqual([], trailers.misplaced(f"Coupling: the ratchet reads the matrix\n\n{CO}\n"))

    def test_a_key_filter_judges_only_that_key(self):
        message = f"s\n\nCoupling: a\nLength: b\n\n{CO}\n"
        self.assertEqual(["Length"], [m.key for m in trailers.misplaced(message, ("Length",))])


class RangeTest(unittest.TestCase):
    def setUp(self):
        self.saved = {k: os.environ.pop(k) for k in list(os.environ) if k.startswith("GIT_")}
        self.dir = tempfile.TemporaryDirectory()
        self.repo = os.path.join(self.dir.name, "repo")
        self.scratch("scratch_repo", "-q")
        self.base = self.commit({"a.txt": "a\n"}, "base")

    def tearDown(self):
        self.dir.cleanup()
        os.environ.update(self.saved)

    def scratch(self, helper, *args):
        return subprocess.run(["bash", "-c", '. "$1"; "$2" "$3" "${@:4}"', "_", SCRATCH_GIT, helper, self.repo, *args],
                              check=True, capture_output=True, text=True).stdout.strip()

    def commit(self, files, message):
        for path, text in files.items():
            with open(os.path.join(self.repo, path), "w") as f:
                f.write(text)
        self.scratch("scratch_git", "add", "-A")
        self.scratch("scratch_git", "commit", "-qm", message)
        return self.scratch("scratch_git", "rev-parse", "HEAD")

    def lint(self, rng):
        env = {k: v for k, v in os.environ.items() if not k.startswith("GIT_")}
        env.update(GIT_CONFIG_GLOBAL="/dev/null", GIT_CONFIG_NOSYSTEM="1")
        done = subprocess.run([sys.executable, "-I", TOOL, "lint", "--range", rng], cwd=self.repo, env=env,
                              capture_output=True, text=True)
        return done.returncode, done.stdout + done.stderr

    def test_lint_warns_about_a_misplaced_line_naming_the_commit_and_still_passes(self):
        head = self.commit({"a.txt": "b\n"}, f"raise it\n\nComplexity: the one state machine\n\n{CO}")
        rc, out = self.lint(f"{self.base}..{head}")
        self.assertEqual(0, rc, out)
        self.assertIn(f"{head[:9]}: found `Complexity: the one state machine` on line 3, but it is not in the final"
                      " trailer block", out)

    def test_lint_is_silent_on_a_placed_trailer(self):
        head = self.commit({"a.txt": "b\n"}, f"raise it\n\nComplexity: the one state machine\n{CO}")
        self.assertEqual((0, ""), self.lint(f"{self.base}..{head}"))

    def test_notes_name_only_commits_changing_the_path(self):
        os.chdir(self.repo)
        try:
            self.commit({"b.txt": "b\n"}, f"other\n\nLength: not this one\n\n{CO}")
            head = self.commit({"a.txt": "c\n"}, f"this\n\nLength: this one\n\n{CO}")
            notes = trailers.misplaced_notes(f"{self.base}..{head}", "Length", "a.txt")
        finally:
            os.chdir(ROOT)
        self.assertEqual(1, len(notes), notes)
        self.assertIn("`Length: this one` on line 3", notes[0])


    def rises(self, rng, rose):
        os.chdir(self.repo)
        try:
            return trailers.unjustified_rises(rng, "a.txt", "Length", rose)
        finally:
            os.chdir(ROOT)

    @staticmethod
    def longer(before, after):
        return [f"{len(before)} -> {len(after)}"] if len(after) > len(before) else []

    def test_each_commit_is_judged_on_its_own_step(self):
        self.commit({"a.txt": "ab\n"}, f"A grows it\n\nLength: A's why\n{CO}")
        self.commit({"a.txt": "a\n"}, "B shrinks it")
        head = self.commit({"a.txt": "abcd\n"}, "C grows it")
        self.assertEqual([f"{head[:9]} C grows it: 2 -> 5"], self.rises(f"{self.base}..{head}", self.longer))

    def test_a_trailer_on_a_commit_not_changing_the_path_justifies_nothing(self):
        first = self.commit({"a.txt": "ab\n"}, "A grows it")
        head = self.commit({"b.txt": "b\n"}, f"B\n\nLength: not A's\n{CO}")
        self.assertEqual([f"{first[:9]} A grows it: 2 -> 3"], self.rises(f"{self.base}..{head}", self.longer))

    def test_a_growth_no_single_step_makes_needs_the_trailer_on_some_commit_changing_the_path(self):
        self.commit({"a.txt": "b\n"}, "A")
        head = self.commit({"a.txt": "c\n"}, "B")
        whole = lambda before, after: []  # noqa: E731 -- each step within its parent's limits
        self.assertEqual([f"no commit of {self.base}..{head} changing a.txt carries `Length:`"],
                         self.rises(f"{self.base}..{head}", whole))
        head = self.commit({"a.txt": "d\n"}, f"C\n\nLength: the range's why\n{CO}")
        self.assertEqual([], self.rises(f"{self.base}..{head}", whole))


if __name__ == "__main__":
    unittest.main()
