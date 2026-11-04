#!/usr/bin/env python3
"""population_walk_diff.py: a flip only the head has refuses (the listing, slice and chain named); one the base has too,
a known one, or a stale known row passes; a known row the change cures refuses; a slice the base could not walk is
judged on the known rows alone; no candidate passes.
Run: python3 scripts/ci/population_walk_diff_test.py"""
import os
import subprocess
import sys
import tempfile
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import population_walk_diff as t  # noqa: E402

TOOL = os.path.join(os.path.dirname(os.path.abspath(__file__)), "population_walk_diff.py")
SLICE = "drift-slices/uk/poole-hen-0a1b2c3d"


def candidate(directory, slice_name, head=(), known=()):
    lines = [f"slice\t{slice_name}"]
    lines += [f"head\t{v}\t{r}\t{v} | {r}: 5 -> none, 6 -> 7, 8 -> none [chain {c}, film-none-film]" for v, r, c in head]
    lines += [f"known\t{v}\t{r}\t{n}" for v, r, n in known]
    with open(os.path.join(directory, slice_name.replace("/", "__") + ".tsv"), "w", encoding="utf-8") as fh:
        fh.write("\n".join(lines) + "\n")


def base_rows(path, walked=(), failed=()):
    with open(path, "w", encoding="utf-8") as fh:
        for s, rows in walked:
            fh.write(f"# {s}: {len(rows)} flip(s) in 1.0s\n")
            for v, r in rows:
                fh.write(f"{s}\t{v}\t{r}\tfilm-none-film: 5 -> none\n")
        for s in failed:
            fh.write(f"# {s}: FAILED java.lang.RuntimeException: boom\n")


class Judge(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.dir = os.path.join(self.tmp.name, "cands")
        os.makedirs(self.dir)
        self.base = os.path.join(self.tmp.name, "base.txt")

    def tearDown(self):
        self.tmp.cleanup()

    def run_tool(self):
        p = subprocess.run([sys.executable, TOOL, "judge", self.dir, self.base], capture_output=True, text=True)
        return p.returncode, p.stdout

    def test_a_flip_only_the_head_has_is_refused_with_listing_slice_and_chain(self):
        candidate(self.dir, SLICE, head=[("Lighthouse Poole", "PFF: Hen", 3)])
        base_rows(self.base, walked=[(SLICE, [])])
        rc, out = self.run_tool()
        self.assertEqual(rc, 1, out)
        self.assertIn(f"NEW flip the base does not have: {SLICE}: Lighthouse Poole | PFF: Hen", out)
        self.assertIn("[chain 3,", out)

    def test_a_flip_the_base_has_too_passes_said_as_pre_existing(self):
        candidate(self.dir, SLICE, head=[("Lighthouse Poole", "PFF: Hen", 3)])
        base_rows(self.base, walked=[(SLICE, [("Lighthouse Poole", "PFF: Hen")])])
        rc, out = self.run_tool()
        self.assertEqual(rc, 0, out)
        self.assertIn("pre-existing", out)

    def test_a_known_flip_passes_and_only_the_new_one_refuses(self):
        candidate(self.dir, SLICE, head=[("A", "Hen", 0), ("B", "Hen", 1)], known=[("A", "Hen", "note")])
        base_rows(self.base, walked=[(SLICE, [("A", "Hen")])])
        rc, out = self.run_tool()
        self.assertEqual(rc, 1, out)
        self.assertIn("B | Hen", out)
        self.assertNotIn("A | Hen", out.replace("population walk: 1", ""))

    def test_a_known_row_the_change_cures_refuses_until_removed(self):
        candidate(self.dir, SLICE, head=[], known=[("A", "Hen", "note")])
        base_rows(self.base, walked=[(SLICE, [("A", "Hen")])])
        rc, out = self.run_tool()
        self.assertEqual(rc, 1, out)
        self.assertIn("CURED known row", out)

    def test_a_stale_known_row_main_holds_passes(self):
        candidate(self.dir, SLICE, head=[], known=[("A", "Hen", "note")])
        base_rows(self.base, walked=[(SLICE, [])])
        rc, out = self.run_tool()
        self.assertEqual(rc, 0, out)
        self.assertIn("stale known row", out)

    def test_a_slice_the_base_could_not_walk_is_judged_on_the_known_rows(self):
        candidate(self.dir, SLICE, head=[("B", "Hen", 1)])
        base_rows(self.base, failed=[SLICE])
        rc, out = self.run_tool()
        self.assertEqual(rc, 1, out)
        self.assertIn("base unwalked", out)
        self.assertIn("FAILED", out)

    def test_no_base_file_is_strict(self):
        candidate(self.dir, SLICE, head=[("B", "Hen", 1)])
        p = subprocess.run([sys.executable, TOOL, "judge", self.dir, os.devnull], capture_output=True, text=True)
        self.assertEqual(p.returncode, 1, p.stdout)

    def test_no_candidate_passes(self):
        base_rows(self.base)
        self.assertEqual(self.run_tool()[0], 0)

    def test_slices_lists_the_candidates(self):
        candidate(self.dir, SLICE, head=[("B", "Hen", 1)])
        p = subprocess.run([sys.executable, TOOL, "slices", self.dir], capture_output=True, text=True)
        self.assertEqual(p.stdout.split(), [SLICE])

    def test_differential(self):
        self.assertEqual(t.differential([1, 2, 3], [3, 4]), ([1, 2], [4], [3]))


if __name__ == "__main__":
    unittest.main()
