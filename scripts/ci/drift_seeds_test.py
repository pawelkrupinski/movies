#!/usr/bin/env python3
"""drift_seeds.py: the convergence legs' takes-drift rows replay their FIXED seeds (drift-gate-seeds.tsv) -- or the seeds
a dispatch pins; the explorer grows the set (`add`); and a red listing's drift slice is found.

    python3 scripts/ci/drift_seeds_test.py
"""
import gzip
import json
import os
import subprocess
import sys
import tempfile
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import drift_seeds as ds  # noqa: E402

SCRIPT = os.path.join(os.path.dirname(os.path.abspath(__file__)), "drift_seeds.py")


class PinsTest(unittest.TestCase):
    def test_forms(self):
        self.assertEqual(ds.parse_pins(""), (None, None))
        self.assertEqual(ds.parse_pins("37,38"), (37, 38))
        self.assertEqual(ds.parse_pins("37"), (37, None))
        self.assertEqual(ds.parse_pins(",38"), (None, 38))

    def test_malformed(self):
        for bad in ("a,b", "0", "1,2,3", "-3"):
            with self.assertRaises(ValueError, msg=bad):
                ds.parse_pins(bad)

    def test_unpinned_rows_replay_their_fixed_set(self):
        self.assertEqual(ds.resolve(5, ""), (None, None))

    def test_the_two_rows_never_share_a_pin(self):
        with self.assertRaises(ValueError):
            ds.resolve(5, "3,3")


class FixedSetTest(unittest.TestCase):
    """FIXED SEEDS: a row replays the seeds drift-gate-seeds.tsv assigns it, the same on every run; `add` grows the set."""

    def setUp(self):
        self.path = os.path.join(tempfile.mkdtemp(), "seeds.tsv")
        with open(self.path, "w") as f:
            f.write("# comment\ncountry\tseed\trow\tadded\twhy\nde\t8\t1\t2026-11-04\tTitans\nde\t64\t2\t2026-11-04\tBuddies\n"
                    "es\t1\t1\t2026-11-04\tspread\nes\t23\t1\t2026-11-04\tspread\n")

    def test_a_rows_seeds(self):
        self.assertEqual(ds.seeds_of_row("de", 1, "", self.path), [8])
        self.assertEqual(ds.seeds_of_row("de", 2, "", self.path), [64])
        self.assertEqual(ds.seeds_of_row("es", 1, "", self.path), [1, 23])
        self.assertEqual(ds.seeds_of_row("es", 2, "", self.path), [])

    def test_a_pin_replaces_the_set_and_leaves_the_other_row_out(self):
        self.assertEqual(ds.seeds_of_row("de", 1, "37,", self.path), [37])
        self.assertEqual(ds.seeds_of_row("de", 2, "37,", self.path), [])

    def test_add_goes_to_the_emptier_row_once(self):
        self.assertTrue(ds.add("es", 9, "Kino X | Y", "2026-11-05", self.path))
        self.assertEqual(ds.seeds_of_row("es", 2, "", self.path), [9])
        self.assertFalse(ds.add("es", 9, "again", "2026-11-05", self.path))
        self.assertTrue(ds.add("pl", 3, "new country", "2026-11-05", self.path))
        self.assertEqual(ds.fixed("pl", None, self.path), [3])
        self.assertEqual(ds.fixed("pl", 1, self.path), [3])

    def test_malformed_row(self):
        with open(self.path, "a") as f:
            f.write("de\tx\t1\t\t\n")
        with self.assertRaises(ValueError):
            ds.read_set(self.path)

    def test_the_committed_set_is_readable_and_covers_every_country_on_both_rows(self):
        for cc in ("pl", "de", "uk", "es", "us"):
            for row in (1, 2):
                self.assertTrue(ds.fixed(cc, row), f"{cc} row {row}")


class CommandTest(unittest.TestCase):
    def run_script(self, *args):
        return subprocess.run([sys.executable, SCRIPT, *args], capture_output=True, text=True)

    def test_outputs_for_github_output(self):
        res = self.run_script("outputs", "--run-number", "2", "--pins", "", "--drift-only", "false")
        self.assertEqual(res.stdout, "drift-seed-1=\ndrift-seed-2=\n")
        res = self.run_script("outputs", "--run-number", "2", "--pins", ",40", "--drift-only", "true")
        self.assertEqual(res.stdout, "drift-seed-1=\ndrift-seed-2=40\n")

    def test_seeds_of_a_row(self):
        self.assertEqual(self.run_script("seeds", "--cc", "de", "--row", "1", "--pins", "37,38").stdout.strip(), "37")
        self.assertTrue(self.run_script("seeds", "--cc", "de", "--row", "1").stdout.strip())

    def test_malformed_exits_2(self):
        self.assertEqual(self.run_script("seeds", "--cc", "de", "--row", "3").returncode, 2)
        self.assertEqual(self.run_script("pins", "x").returncode, 2)
        self.assertEqual(self.run_script("pins", "5").stdout.strip(), "5,")


class SlicesTest(unittest.TestCase):
    def test_a_red_listing_names_its_slice(self):
        with tempfile.TemporaryDirectory() as tmp:
            for name, core in (("metropol-madoka-1", ["P\0Metropol\0Madoka\0"]),
                               ("dammtor-titans-2", ["N\0Dammtor\0https://x\0Titans"]),
                               ("other-3", ["N\0Other\0https://y\0Else"])):
                d = os.path.join(tmp, "drift-slices", "de", name)
                os.makedirs(d)
                with gzip.open(os.path.join(d, "slice.json.gz"), "wt") as f:
                    json.dump({"country": "de", "core": core, "context": []}, f)
            tsv = os.path.join(tmp, "flips-de.tsv")
            with open(tsv, "w") as f:
                f.write("country\tseed\tvenue\trawTitle\tfilms\treasons\ttakes\n")
                f.write("de\t37\tMetropol\tMadoka\ta|b\tr\tt\n")
                f.write("de\t37\tDammtor\tTitans\ta|b\tr\tt\n")
            found = ds.slices("de", tmp, [tsv, os.path.join(tmp, "missing.tsv")])
            self.assertEqual([os.path.basename(p) for p in found], ["dammtor-titans-2", "metropol-madoka-1"])
            self.assertEqual(ds.slices("uk", tmp, [tsv]), [])

    def test_the_committed_slices_are_readable(self):
        root = os.path.join(os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))),
                            "test", "resources", "fixtures")
        with tempfile.TemporaryDirectory() as tmp:
            tsv = os.path.join(tmp, "flips-de.tsv")
            with open(tsv, "w") as f:
                f.write("country\tseed\tvenue\trawTitle\n" "de\t3\tMetropol-Kino-Center\tMadoka Magica - Der Film: Rebellion\n")
            self.assertTrue(any("metropol-madoka" in p for p in ds.slices("de", root, [tsv])))


if __name__ == "__main__":
    unittest.main()
