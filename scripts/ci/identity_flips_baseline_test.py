#!/usr/bin/env python3
"""identity_flips_baseline.py: the known-flips baseline is generated from a run's flips TSVs, one row per country, seed,
venue and raw title, never typed -- and the checked-in file reads as the drift check reads it.

    python3 scripts/ci/identity_flips_baseline_test.py
"""
import os
import sys
import tempfile
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import identity_flips_baseline as baseline  # noqa: E402

ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
FLIPS = "country\tseed\tvenue\trawTitle\tfilms\treasons\ttakes\n"


class FromTsvs(unittest.TestCase):
    def tsv(self, text):
        f = tempfile.NamedTemporaryFile("w", suffix=".tsv", delete=False, encoding="utf-8")
        f.write(text)
        f.close()
        self.addCleanup(os.unlink, f.name)
        return f.name

    def test_one_row_per_listing_and_seed_sorted_with_its_takes_and_owner(self):
        de = self.tsv(FLIPS + "de\t2\tWerkstattkino e.V.\tCAMOUFLAGE\ttmdb:1735950 -> tmdb:56085 -> tmdb:1735950\tback to a film it left\tx\n"
                      "de\t2\tWerkstattkino e.V.\tCAMOUFLAGE\ttmdb:1735950 -> tmdb:56085\tagain\tx\n"
                      "de\t1\tCinemaxX Dammtor\tTitans\ttmdb:1 -> tmdb:2 -> tmdb:1\tback to a film it left\tx\n")
        rows = baseline.rows_of([de], "2026-10-29", "red-fixer", "convergence #2527")
        self.assertEqual([r[:4] for r in rows], [["de", "1", "CinemaxX Dammtor", "Titans"], ["de", "2", "Werkstattkino e.V.", "CAMOUFLAGE"]])
        self.assertEqual(rows[1][4:7], ["tmdb:1735950 -> tmdb:56085 -> tmdb:1735950", "2026-10-29", "red-fixer"])
        self.assertIn("back to a film it left", rows[1][7])

    def test_no_flips_is_a_header_alone_the_strict_gate(self):
        rows = baseline.rows_of([self.tsv(FLIPS)], "2026-10-29", "x", "")
        text = baseline.render(rows)
        self.assertEqual([l for l in text.splitlines() if l and not l.startswith("#")], [baseline.HEADER.splitlines()[-1]])

    def test_a_file_that_is_no_flips_tsv_is_refused(self):
        with self.assertRaises(ValueError):
            baseline.rows_of([self.tsv("country\tseed\tcheckpoint\n")], "2026-10-29", "x", "")

    def test_the_cli_writes_the_whole_file(self):
        out = os.path.join(tempfile.mkdtemp(), "b.tsv")
        de = self.tsv(FLIPS + "de\t1\tV\tT\ta -> b -> a\tr\tx\n")
        self.assertEqual(0, baseline.main(["from-tsvs", "--first-seen", "2026-10-29", "--owner", "o", "--out", out, de]))
        with open(out, encoding="utf-8") as f:
            self.assertTrue(f.read().endswith("de\t1\tV\tT\ta -> b -> a\t2026-10-29\to\tr\n"))
        self.assertEqual(2, baseline.main(["from-tsvs", de]))


class CheckedIn(unittest.TestCase):
    def test_the_checked_in_baseline_carries_the_generated_header_and_full_rows(self):
        with open(os.path.join(ROOT, "test/resources/fixtures/identity-flips-baseline.tsv"), encoding="utf-8") as f:
            text = f.read()
        self.assertTrue(text.startswith(baseline.HEADER))
        for line in text[len(baseline.HEADER):].splitlines():
            cols = line.split("\t")
            self.assertEqual(8, len(cols), line)
            self.assertTrue(cols[1].isdigit() and cols[5] and cols[6], line)


if __name__ == "__main__":
    unittest.main()
