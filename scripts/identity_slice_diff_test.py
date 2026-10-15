#!/usr/bin/env python3
"""identity_slice_diff.py: every listing whose take moved between two slices, tallied by its verdict at each ref.

    python3 scripts/identity_slice_diff_test.py
"""
import contextlib
import io
import os
import sys
import tempfile
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import identity_slice_diff as diff  # noqa: E402

HEADER = "selector\tkey\tvenue\trawTitle\tfilm\ttitle\tbasis\tconfidence\tmodel\trules\tblocker\trunnerUp\tverdict\texplanation\n"


def take(venue, title, film, film_title, basis, verdict, why=""):
    """One line of IdentitySlice's --out."""
    return "\t".join(["k", f"{venue}|{title}", venue, title, film, film_title, basis, "0.9", film, "", "", "", verdict, why]) + "\n"


BASE = [take("Kino", "Lalka", "tmdb:1", "Lalka (1968)", "OwnMatch", "WRONG", "title + year"),
        take("Kino", "Emil", "tmdb:2", "Emil (2001)", "OwnMatch", "RIGHT"),
        take("Kino", "Same", "tmdb:3", "Same (2000)", "OwnMatch", "RIGHT"),
        take("Odeon", "Was None", "none", "", "BelowThreshold", "-"),
        take("Odeon", "Unjudged", "tmdb:5", "", "OwnMatch", "UNJUDGED"),
        take("Odeon", "Gone", "tmdb:6", "Gone (1999)", "Pooled", "UNJUDGED")]
HEAD = [take("Kino", "Lalka", "tmdb:10", "Lalka (2026)", "Search", "RIGHT", "x" * 300),
        take("Kino", "Emil", "none", "", "BelowThreshold", "-", "no candidate"),
        take("Kino", "Same", "tmdb:3", "Same (2000)", "OwnMatch", "RIGHT"),
        take("Odeon", "Was None", "tmdb:7", "Bad (1990)", "Search", "WRONG", "searched"),
        take("Odeon", "Unjudged", "tmdb:8", "Other (2011)", "Search", "UNJUDGED"),
        take("Odeon", "New", "tmdb:9", "Won (2020)", "Search", "RIGHT")]

GOLDEN = f"""
7 listing(s) decided; 6 moved between base1 and head1

CURED (1):
  Kino | Lalka: tmdb:1 'Lalka (1968)' via OwnMatch [WRONG] -> tmdb:10 'Lalka (2026)' via Search [RIGHT]
      {"x" * 220}

LOST (1):
  Kino | Emil: tmdb:2 'Emil (2001)' via OwnMatch [RIGHT] -> none via BelowThreshold [-]
      no candidate

NEW-WRONG (1):
  Odeon | Was None: none via BelowThreshold [-] -> tmdb:7 'Bad (1990)' via Search [WRONG]
      searched

WON (1):
  Odeon | New: not decided -> tmdb:9 'Won (2020)' via Search [RIGHT]

MOVED (UNJUDGED) (2):
  Odeon | Gone: tmdb:6 'Gone (1999)' via Pooled [UNJUDGED] -> not decided
  Odeon | Unjudged: tmdb:5 via OwnMatch [UNJUDGED] -> tmdb:8 'Other (2011)' via Search [UNJUDGED]

TALLY cured 1, lost 1, new-wrong 1, won 1, moved (unjudged) 2
"""


class SliceDiffTest(unittest.TestCase):
    def write(self, name, lines):
        path = os.path.join(self.dir.name, name)
        with open(path, "w", encoding="utf-8") as f:
            f.write(HEADER + "".join(lines))
        return path

    def setUp(self):
        self.dir = tempfile.TemporaryDirectory()
        self.addCleanup(self.dir.cleanup)

    def test_every_move_is_listed_under_its_verdicts_and_tallied(self):
        out = io.StringIO()
        with contextlib.redirect_stdout(out):
            code = diff.main([self.write("base.tsv", BASE), self.write("head.tsv", HEAD), "base1", "head1", "labels.tsv"])
        self.assertEqual(0, code)
        self.assertEqual(GOLDEN, out.getvalue())

    def test_two_identical_slices_move_nothing(self):
        out = io.StringIO()
        with contextlib.redirect_stdout(out):
            diff.main([self.write("a.tsv", BASE), self.write("b.tsv", BASE), "a", "b", "labels.tsv"])
        self.assertEqual("\n6 listing(s) decided; 0 moved between a and b\n\n"
                         "TALLY cured 0, lost 0, new-wrong 0, won 0, moved (unjudged) 0\n", out.getvalue())


if __name__ == "__main__":
    unittest.main()
