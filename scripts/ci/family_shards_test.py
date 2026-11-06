#!/usr/bin/env python3
"""family_shards.py: the plan names every country's shards (all five by default), narrows to the countries asked for,
and refuses an unknown one.

    python3 scripts/ci/family_shards_test.py
"""
import os
import subprocess
import sys
import tempfile
import unittest

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import family_shards  # noqa: E402


class PlanTest(unittest.TestCase):
    def test_every_country_by_default(self):
        matrix, expect, codes = family_shards.plan([])
        self.assertEqual(len(matrix["include"]), sum(family_shards.SHARDS.values()))
        self.assertEqual(set(codes.split()), {"us", "uk", "de", "pl", "es"})
        self.assertIn(f"de={family_shards.SHARDS['de']}", expect.split(","))

    def test_rows_are_numbered_one_to_n(self):
        matrix, _, _ = family_shards.plan(["pl"])
        n = family_shards.SHARDS["pl"]
        self.assertEqual([(r["code"], r["index"], r["shards"], r["name"]) for r in matrix["include"]],
                         [("pl", i, n, f"pl {i}/{n}") for i in range(1, n + 1)])

    def test_unknown_country_is_refused(self):
        self.assertEqual(family_shards.main(["plan", "--countries", "de xx"]), 2)
        self.assertEqual(family_shards.main(["plan", "--countries", "de,pl"]), 0)


class WeekTest(unittest.TestCase):
    def test_the_plan_names_the_iso_week_the_orders_are_salted_with(self):
        out = subprocess.run([sys.executable, "-I", os.path.join(HERE, "family_shards.py"), "plan", "--countries", "es",
                              "--date", "2026-11-06"], capture_output=True, text=True, check=True).stdout
        self.assertIn("week=45", out.split())


class CostsTest(unittest.TestCase):
    def test_costs_are_the_walks_family_tier_subsets_and_seconds(self):
        with tempfile.TemporaryDirectory() as src, tempfile.TemporaryDirectory() as out:
            os.makedirs(os.path.join(src, "identity-family-de-1of2"))
            with open(os.path.join(src, "identity-family-de-1of2", "family-walks-de.tsv"), "w", encoding="utf-8") as f:
                f.write("family\ttier\tsubsets\tseconds\tflips\trules\nB\tlattice 2^2\t4\t1.50\t0\ta b\nA\t8 orders x 8 steps\t57\t9.00\t1\tc\n")
            self.assertEqual(family_shards.main(["costs", "--from", src, "--out", out]), 0)
            with open(os.path.join(out, "family-costs-de.tsv"), encoding="utf-8") as f:
                self.assertEqual(f.read(), "family\ttier\tsubsets\tseconds\nA\t8 orders x 8 steps\t57\t9.00\nB\tlattice 2^2\t4\t1.50\n")

    def test_every_country_has_a_cost_file(self):
        for code in family_shards.SHARDS:
            self.assertTrue(os.path.exists(os.path.join(HERE, "..", "..", "test", "resources", "fixtures", f"family-costs-{code}.tsv")), code)


if __name__ == "__main__":
    unittest.main()
