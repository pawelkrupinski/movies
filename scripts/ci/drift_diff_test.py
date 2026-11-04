#!/usr/bin/env python3
"""drift_diff.py: only a flip the head shows on a seed and its base does not fails; one both show is pre-existing
(backlog) and passes; one only the base shows is removed; a TEMPORARY flips-allow row passes a head-only flip; the same
identity code makes every flip pre-existing.

    python3 scripts/ci/drift_diff_test.py
"""
import os
import subprocess
import sys
import tempfile
import unittest

SCRIPT = os.path.join(os.path.dirname(os.path.abspath(__file__)), "drift_diff.py")
HEADER = "country\tseed\tvenue\trawTitle\tfilms\treasons\ttakes\tcause\n"


def tsv(rows, header=HEADER):
    path = os.path.join(tempfile.mkdtemp(), "f.tsv")
    with open(path, "w") as f:
        f.write(header + "".join("\t".join(r) + "\n" for r in rows))
    return path


def flip(venue, title, seed="8"):
    return ["de", seed, venue, title, "tmdb:1", "back to a film it left", "tmdb:1 [1 boot] -> none [4 scrape] -> tmdb:1 [8 fill]", "population"]


class DiffTest(unittest.TestCase):
    def run_diff(self, *args):
        res = subprocess.run([sys.executable, SCRIPT, "--cc", "de", "--seed", "8", *args], capture_output=True, text=True)
        return res.returncode, res.stdout

    def test_a_flip_both_show_is_pre_existing(self):
        code, out = self.run_diff("--head", tsv([flip("CinemaxX Regensburg", "Titans")]), "--base", tsv([flip("CinemaxX Regensburg", "Titans")]))
        self.assertEqual(code, 0)
        self.assertIn("      pre-existing (backlog): CinemaxX Regensburg | Titans [seed 8] -- in the base too, not failed; "
                      "not in the backlog yet: the explorer files it", out)
        self.assertTrue(out.endswith("new=0 pre-existing=1 removed=0\n"))

    def test_a_flip_the_head_adds_fails_named(self):
        code, out = self.run_diff("--head", tsv([flip("A", "x"), flip("B", "y")]), "--base", tsv([flip("A", "x")]))
        self.assertEqual(code, 1)
        self.assertIn("      B | y: tmdb:1 [1 boot] -> none [4 scrape] -> tmdb:1 [8 fill] [seed 8]\n", out)
        self.assertIn("new=1 pre-existing=1 removed=0", out)

    def test_other_seeds_rows_are_not_this_seeds(self):
        code, _ = self.run_diff("--head", tsv([flip("A", "x", "9")]), "--base", tsv([]))
        self.assertEqual(code, 0)

    def test_removed(self):
        code, out = self.run_diff("--head", tsv([]), "--base", tsv([flip("A", "x")]))
        self.assertEqual(code, 0)
        self.assertIn("      removed: A | x [seed 8]", out)

    def test_same_code_is_all_pre_existing(self):
        code, out = self.run_diff("--head", tsv([flip("A", "x")]), "--same-code")
        self.assertEqual(code, 0)
        self.assertIn("pre-existing=1", out)

    def test_temporary_allow_row_and_backlog_row(self):
        allow = tsv([["de", "*", "y", "TEMPORARY owner: a; remove when b; c"]], "country\tvenue\trawTitle\treason\n")
        backlog = tsv([["drift", "de", "8", "", "A", "x", "", "2026-11-02", "owner", "n"]],
                      "kind\tcountry\tseed\tslice\tvenue\trawTitle\ttakes\tfirstSeen\towner\tnote\n")
        code, out = self.run_diff("--head", tsv([flip("A", "x"), flip("B", "y")]), "--base", tsv([flip("A", "x")]),
                                  "--allow", allow, "--backlog", backlog)
        self.assertEqual(code, 0)
        self.assertIn("allow-listed (identity-flips-allow.tsv): B | y [seed 8]", out)
        self.assertIn("backlog: owner owner, first seen 2026-11-02", out)

    def test_unreadable_is_2(self):
        self.assertEqual(self.run_diff("--head", "/nonexistent", "--same-code")[0], 2)


if __name__ == "__main__":
    unittest.main()
