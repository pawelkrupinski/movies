#!/usr/bin/env python3
"""drift_explorer.py: the explorer picks seeds off the gate, publishes the flips no backlog row names, and agentctl's
filing appends each as a backlog row with its seed on the gate.

    python3 scripts/ci/drift_explorer_test.py
"""
import os
import sys
import tempfile
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import drift_explorer as explorer  # noqa: E402

FLIPS = "country\tseed\tvenue\trawTitle\tfilms\treasons\ttakes\n"
BACKLOG = "# the backlog\n" + "\t".join(explorer.BACKLOG_COLUMNS) + "\n" \
          "drift\tde\t47\t\tCinemaxX Dammtor\tTitans\t\t2026-11-02\towner\tknown\n"
SEEDS = "# seeds\ncountry\tseed\trow\tadded\twhy\nde\t8\t1\t2026-11-04\tx\nde\t64\t2\t2026-11-04\ty\n"


class DriftExplorerTest(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.backlog = self.write("backlog.tsv", BACKLOG)
        self.seeds = self.write("seeds.tsv", SEEDS)

    def tearDown(self):
        self.tmp.cleanup()

    def write(self, name, text):
        path = os.path.join(self.tmp.name, name)
        with open(path, "w", encoding="utf-8") as f:
            f.write(text)
        return path

    def test_picks_seeds_off_the_gate_the_same_for_the_same_salt(self):
        one = explorer.pick("de", 5, "41780196052", self.seeds)
        self.assertEqual(5, len(set(one)))
        self.assertFalse({8, 64} & set(one))
        self.assertTrue(all(3 <= s < 1000 for s in one))
        self.assertEqual(one, explorer.pick("de", 5, "41780196052", self.seeds))
        self.assertNotEqual(one, explorer.pick("de", 5, "other", self.seeds))

    def test_new_flips_leave_out_the_backlogs_listings_one_row_per_listing_its_lowest_seed(self):
        flips = self.write("flips-de.tsv", FLIPS
                           + "de\t311\tCinemaxX Dammtor\tTitans\t\t\ta -> b -> a\n"
                           + "de\t311\tCinemaxX Regensburg\tTitans\t\t\ta -> b -> a\n"
                           + "de\t77\tCinemaxX Regensburg\tTitans\t\t\tnone -> a -> none\n")
        rows = explorer.new_flips(self.backlog, "2026-11-05", "run:9/identity-drift-de-takes-drift-1", [flips])
        self.assertEqual([["CinemaxX Regensburg", "Titans", "de", "77", "2026-11-05", "run:9/identity-drift-de-takes-drift-1",
                           "none -> a -> none"]], rows)

    def test_a_file_that_is_no_flips_tsv_is_refused(self):
        with self.assertRaises(ValueError):
            explorer.new_flips(self.backlog, "2026-11-05", "", [self.write("x.tsv", "a\tb\n")])

    def test_filing_appends_the_new_flips_and_their_seeds_once(self):
        asset = self.write("new-flips.tsv", "\t".join(explorer.NEW_HEADER) + "\n"
                           + "CinemaxX Regensburg\tTitans\tde\t77\t2026-11-05\trun:9/a\tnone -> a -> none\n"
                           + "CinemaxX Dammtor\tTitans\tde\t311\t2026-11-05\trun:9/a\tx\n")
        self.assertEqual(["filed de 77 CinemaxX Regensburg | Titans"], explorer.file(asset, self.backlog, self.seeds, "fixer"))
        rows = explorer.read_tsv(self.backlog)
        self.assertEqual(("drift", "de", "77", "CinemaxX Regensburg"), (rows[-1]["kind"], rows[-1]["country"], rows[-1]["seed"],
                                                                         rows[-1]["venue"]))
        self.assertIn(77, explorer.drift_seeds.fixed("de", path=self.seeds))
        self.assertEqual([], explorer.file(asset, self.backlog, self.seeds, "fixer"))  # filed already: nothing again
        self.assertEqual(2, len(explorer.read_tsv(self.backlog)))


if __name__ == "__main__":
    unittest.main()
