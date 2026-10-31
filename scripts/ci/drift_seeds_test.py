#!/usr/bin/env python3
"""drift_seeds.py: the convergence legs' takes-drift rows replay the run's rotating seeds -- two distinct ones, the same
on a rerun, every seed of 1..64 within 32 runs -- or the seeds a dispatch pins; and a red listing's drift slice is found.

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


class RotationTest(unittest.TestCase):
    def test_two_distinct_seeds_in_range_every_run(self):
        for n in range(1, 500):
            one, two = ds.rotation(n)
            self.assertNotEqual(one, two, n)
            self.assertTrue(1 <= one <= ds.RANGE and 1 <= two <= ds.RANGE, (n, one, two))

    def test_deterministic_per_run(self):
        self.assertEqual(ds.rotation(2190), ds.rotation(2190))

    def test_every_seed_within_half_the_range_of_runs(self):
        for start in (1, 17, 2190):
            seen = {s for n in range(start, start + ds.RANGE // 2) for s in ds.rotation(n)}
            self.assertEqual(seen, set(range(1, ds.RANGE + 1)), start)

    def test_consecutive_runs_replay_new_seeds(self):
        self.assertEqual(ds.rotation(1), (1, 2))
        self.assertEqual(ds.rotation(2), (3, 4))
        self.assertEqual(ds.rotation(33), (1, 2))

    def test_no_run_zero(self):
        with self.assertRaises(ValueError):
            ds.rotation(0)


class PinsTest(unittest.TestCase):
    def test_forms(self):
        self.assertEqual(ds.parse_pins(""), (None, None))
        self.assertEqual(ds.parse_pins("both"), (None, None))
        self.assertEqual(ds.parse_pins("7,40"), (7, 40))
        self.assertEqual(ds.parse_pins("7,"), (7, None))
        self.assertEqual(ds.parse_pins(",40"), (None, 40))
        self.assertEqual(ds.parse_pins("11"), (11, None))

    def test_malformed(self):
        for bad in ("a,b", "0", "1,2,3", "-3"):
            with self.assertRaises(ValueError, msg=bad):
                ds.parse_pins(bad)

    def test_unpinned_is_the_rotation(self):
        self.assertEqual(ds.resolve(5, ""), ds.rotation(5))
        self.assertEqual(ds.resolve(5, "", drift_only=True), ds.rotation(5))

    def test_pinned_rows_replay_their_pins(self):
        self.assertEqual(ds.resolve(5, "37,38"), (37, 38))

    def test_drift_only_leaves_an_unpinned_row_out(self):
        self.assertEqual(ds.resolve(5, ",38", drift_only=True), (None, 38))
        self.assertEqual(ds.resolve(5, "37", drift_only=True), (37, None))

    def test_full_run_fills_an_unpinned_row_with_a_distinct_rotating_seed(self):
        one, two = ds.resolve(1, ",1")  # the rotation's first seed is 1, pinned to row 2
        self.assertEqual(two, 1)
        self.assertNotEqual(one, two)
        self.assertEqual(ds.resolve(1, "9,"), (9, 1))

    def test_the_two_rows_never_share_a_pin(self):
        with self.assertRaises(ValueError):
            ds.resolve(5, "3,3")


class CommandTest(unittest.TestCase):
    def run_script(self, *args):
        return subprocess.run([sys.executable, SCRIPT, *args], capture_output=True, text=True)

    def test_outputs_for_github_output(self):
        res = self.run_script("outputs", "--run-number", "2", "--pins", "", "--drift-only", "false")
        self.assertEqual(res.stdout, "drift-seed-1=3\ndrift-seed-2=4\n")
        res = self.run_script("outputs", "--run-number", "2", "--pins", ",40", "--drift-only", "true")
        self.assertEqual(res.stdout, "drift-seed-1=\ndrift-seed-2=40\n")

    def test_seed_of_a_row(self):
        self.assertEqual(self.run_script("seed", "--row", "2", "--run-number", "2").stdout.strip(), "4")
        self.assertEqual(self.run_script("seed", "--row", "1", "--run-number", "2", "--pins", "37,38").stdout.strip(), "37")

    def test_malformed_exits_2(self):
        self.assertEqual(self.run_script("seed", "--row", "3", "--run-number", "2").returncode, 2)
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
