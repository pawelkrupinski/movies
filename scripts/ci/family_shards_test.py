#!/usr/bin/env python3
"""family_shards.py: the plan names every country's shards (all five by default), narrows to the countries asked for,
and refuses an unknown one.

    python3 scripts/ci/family_shards_test.py
"""
import os
import sys
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import family_shards  # noqa: E402


class PlanTest(unittest.TestCase):
    def test_every_country_by_default(self):
        matrix, expect, codes = family_shards.plan([])
        self.assertEqual(len(matrix["include"]), sum(family_shards.SHARDS.values()))
        self.assertEqual(set(codes.split()), {"us", "uk", "de", "pl", "es"})
        self.assertIn("de=4", expect.split(","))

    def test_rows_are_numbered_one_to_n(self):
        matrix, _, _ = family_shards.plan(["pl"])
        self.assertEqual([(r["code"], r["index"], r["shards"], r["name"]) for r in matrix["include"]],
                         [("pl", 1, 2, "pl 1/2"), ("pl", 2, 2, "pl 2/2")])

    def test_unknown_country_is_refused(self):
        self.assertEqual(family_shards.main(["plan", "--countries", "de xx"]), 2)
        self.assertEqual(family_shards.main(["plan", "--countries", "de,pl"]), 0)


if __name__ == "__main__":
    unittest.main()
