#!/usr/bin/env python3
"""shard_weights.py: a suite's weight is its median over runs (once a run), a suite seen too rarely keeps its row, a
class gone from the tree loses it; and the checked-in identity weights name every spec the landing check measured
at a minute or more, so the shards split on what they cost.
Run: python3 scripts/ci/shard_weights_test.py"""
import json
import os
import subprocess
import sys
import tempfile
import unittest

sys.dont_write_bytecode = True  # `python3 -I` ignores PYTHONDONTWRITEBYTECODE: no __pycache__ in the tree
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import shard_tests  # noqa: E402
import shard_weights as w  # noqa: E402

TOOL = os.path.join(os.path.dirname(os.path.abspath(__file__)), "shard_weights.py")


class WeightsTest(unittest.TestCase):

    def test_a_suites_weight_is_its_median_over_runs_each_run_once(self):
        samples = [("1", "A", 100.0), ("1", "A", 100.0), ("2", "A", 300.0), ("3", "A", 200.0), ("1", "B", 9.0)]
        self.assertEqual(w.medians(samples, 3), {"A": 200.0})
        self.assertEqual(w.medians(samples, 1), {"A": 200.0, "B": 9.0})

    def test_a_rarely_measured_row_is_kept_and_a_gone_class_dropped(self):
        rows = w.merge({"A": 200.0}, {"A": 50.0, "Rare": 80.0, "Gone": 999.0}, lambda c: c != "Gone")
        self.assertEqual(rows, [("A", 200.0), ("Rare", 80.0)])

    def test_refresh_writes_a_file_shard_tests_reads(self):
        with tempfile.TemporaryDirectory() as d:
            samples, current = os.path.join(d, "s.jsonl"), os.path.join(d, "cur.tsv")
            with open(samples, "w") as fh:
                for run, secs in (("1", 400), ("2", 450), ("3", 500)):
                    fh.write(json.dumps({"run_id": run, "suite": "services.identity.FamilyIndexCompletenessSpec",
                                         "time_s": str(secs)}) + "\n")
            with open(current, "w") as fh:
                fh.write("# class\tseconds\nscripts.NoSuchThingAnywhereSpec\t10\n")
            out = subprocess.run([sys.executable, TOOL, "refresh", "--samples", samples, "--current", current],
                                 capture_output=True, text=True, check=True).stdout
            target = os.path.join(d, "new.tsv")
            with open(target, "w") as fh:
                fh.write(out)
            self.assertEqual(shard_tests.read_weights(target), {"services.identity.FamilyIndexCompletenessSpec": 450.0})

    # 2026-11-18: FamilyIndexCompletenessSpec (median ~405 s over 149 landing checks) had no row and weighed 3 s; the
    # file's last refresh (11-04) predated it, FamilyConvergence and FamilyConvergenceSlices
    def test_the_identity_weights_name_every_spec_measured_at_a_minute_or_more(self):
        measured_long = [  # victoria-logs-ci, identity shard jobs 2026-11-10..18, median >= 60 s
            "services.identity.FamilyIndexCompletenessSpec", "services.identity.FamilyConvergenceSlicesSpec",
            "services.identity.FamilyConvergenceSpec", "services.identity.PopulationMonotonicityPart2Spec",
            "services.identity.ReadSetClosurePart2Spec", "scripts.IdentityDriftSlicesShard3Spec",
            "scripts.IdentityDriftSlicesSpec", "services.identity.agreement.IncrementalWholeEquivalenceSpec",
            "services.identity.UnmatchedClustersRatchetSpec", "services.identity.agreement.LateInputEquivalenceSpec",
            "scripts.IdentityRefitMeasureAllSpec"]
        weights = shard_tests.read_weights("identity")
        self.assertEqual([c for c in measured_long if c not in weights], [])
        self.assertGreater(weights["services.identity.FamilyIndexCompletenessSpec"], 100)


if __name__ == "__main__":
    unittest.main()
