#!/usr/bin/env python3
"""Unit tests for scripts/copy-releases-to-github's plan: caches skipped, what github.com holds at the same size kept,
stale assets listed (pruned only when asked), and the overflow past github.com's 1000 assets in part releases.

    python3 scripts/copy_releases_to_github_test.py
"""
import importlib.machinery
import importlib.util
import os
import sys
import unittest

HERE = os.path.dirname(os.path.abspath(__file__))
_loader = importlib.machinery.SourceFileLoader("copy_releases", os.path.join(HERE, "copy-releases-to-github"))
_spec = importlib.util.spec_from_loader("copy_releases", _loader)
cr = importlib.util.module_from_spec(_spec)
sys.modules["copy_releases"] = cr  # its dataclasses look their module up there (Python <= 3.13 fails without)
_loader.exec_module(cr)


def rel(*assets):
    return cr.Release("convergence-fixtures", assets=[cr.Asset(n, s) for n, s in assets])


class PlanTest(unittest.TestCase):
    def test_a_missing_release_is_created_with_every_asset(self):
        [p] = cr.plan_release(rel(("hermetic-pl.txt", 10)), None, with_caches=False, prune=False)
        self.assertTrue(p.create)
        self.assertEqual([a.name for a in p.upload], ["hermetic-pl.txt"])

    def test_caches_are_not_copied_unless_asked(self):
        r = rel(("takes-final-pl-abc1234.gz", 5), ("measure-result-k.json", 5), ("enrichment-pl.tar.zst", 9))
        [p] = cr.plan_release(r, {}, with_caches=False, prune=False)
        self.assertEqual([a.name for a in p.upload], ["enrichment-pl.tar.zst"])
        self.assertEqual(len(p.skipped_caches), 2)
        [p] = cr.plan_release(r, {}, with_caches=True, prune=False)
        self.assertEqual(len(p.upload), 3)

    def test_same_size_is_kept_and_another_size_reuploaded(self):
        r = rel(("a", 1), ("b", 2))
        [p] = cr.plan_release(r, {"a": 1, "b": 3}, with_caches=False, prune=False)
        self.assertEqual(p.present, ["a"])
        self.assertEqual([a.name for a in p.upload], ["b"])

    def test_stale_assets_are_listed_and_pruned_only_when_asked(self):
        [p] = cr.plan_release(rel(("a", 1)), {"a": 1, "old": 4}, with_caches=False, prune=False)
        self.assertEqual(p.stale, ["old"])
        self.assertEqual(p.final_count(), 2)
        [p] = cr.plan_release(rel(("a", 1)), {"a": 1, "old": 4}, with_caches=False, prune=True)
        self.assertEqual(p.final_count(), 1)

    def test_github_com_own_fixture_data_is_kept_not_stale(self):
        have = {"a": 1, "fill-de-573-41209071921-1-convergence.tar.zst": 9, "refused-uk-573-41209071921-1.tsv": 1,
                "fill-de-573-4200-1-convergence.tar.zst": 9, "prod-latest-de.txt": 1}
        [p] = cr.plan_release(rel(("a", 1)), have, with_caches=False, prune=True)
        self.assertEqual(p.own, ["fill-de-573-41209071921-1-convergence.tar.zst", "refused-uk-573-41209071921-1.tsv"])
        # a Forgejo-named fill or a pointer Forgejo no longer has is still stale
        self.assertEqual(p.stale, ["fill-de-573-4200-1-convergence.tar.zst", "prod-latest-de.txt"])
        self.assertEqual(p.final_count(), 3)

    def test_the_overflow_past_the_limit_goes_to_part_releases(self):
        r = rel(*[(f"f{i:02}", 1) for i in range(7)])
        plans = cr.plan_release(r, {"old": 1}, with_caches=False, prune=False, limit=3)
        self.assertEqual([p.tag for p in plans], ["convergence-fixtures", "convergence-fixtures-part-2",
                                                  "convergence-fixtures-part-3"])
        self.assertEqual([len(p.upload) for p in plans], [2, 3, 2])
        self.assertTrue(all(p.final_count() <= 3 for p in plans))

    def test_an_asset_over_2_gib_is_reported_not_copied(self):
        [p] = cr.plan_release(rel(("huge", cr.GITHUB_ASSET_MAX_BYTES + 1)), {}, with_caches=False, prune=False)
        self.assertEqual(p.upload, [])
        self.assertEqual([a.name for a in p.too_large], ["huge"])


if __name__ == "__main__":
    unittest.main()
