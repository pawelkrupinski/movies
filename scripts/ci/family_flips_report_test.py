#!/usr/bin/env python3
"""family_flips_report.py: shards join into a country's file, flips rank by the deciding (never refusal) rules their walk fired, the flips an
earlier night's file lacks are the new ones, and a country with a shard missing publishes nothing and keeps its place.

    python3 scripts/ci/family_flips_report_test.py
"""
import os
import sys
import tempfile
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import family_flips_report as report  # noqa: E402


def row(venue, title, rules="", cc="de"):
    note = "final add/remove, model add/remove [lattice 2^4] fam" + (f" rules: {rules}" if rules else "")
    return "\t".join(["family", cc, "", "", venue, title, "none -> tmdb:1 -> none", "", "", note]) + "\n"


def write(path, *rows):
    os.makedirs(os.path.dirname(path), exist_ok=True)
    with open(path, "w", encoding="utf-8") as f:
        f.write("\t".join(report.BACKLOG_COLUMNS) + "\n" + "".join(rows))


class ReportTest(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.mkdtemp()
        self.shards, self.previous, self.out = (os.path.join(self.tmp, d) for d in ("shards", "previous", "out"))
        os.makedirs(self.previous)

    def shard(self, cc, i, n, *rows):
        write(os.path.join(self.shards, f"identity-family-{cc}-{i}of{n}", f"family-flips-{cc}.tsv"), *rows)

    def lines(self, name):
        with open(os.path.join(self.out, name), encoding="utf-8") as f:
            return [line.rstrip("\n").split("\t") for line in f][1:]

    def test_shards_join_and_rank_by_rule_with_a_listing_under_every_rule_it_fired(self):
        self.shard("de", 1, 2, row("A", "x", "join:same-title accept:y"), row("B", "y", "join:same-title"))
        self.shard("de", 2, 2, row("C", "z", ""))
        report.report(self.shards, {"de": 2}, self.previous, self.out)
        self.assertEqual([r[4] for r in self.lines("family-flips-de.tsv")], ["A", "B", "C"])
        ranked = [(r[0], r[1], r[2], r[4]) for r in self.lines("family-flips-ranked.tsv")]
        self.assertEqual(ranked, [("1", "join:same-title", "2", "A"), ("1", "join:same-title", "2", "B"),
                                  ("2", "accept:y", "1", "A"), ("3", "(no rule recorded)", "1", "C")])
        shape, tier = self.lines("family-flips-ranked.tsv")[0][6:8]
        self.assertEqual((shape, tier), ("final add/remove, model add/remove", "[lattice 2^4]"))

    def test_a_refusal_ranks_no_listing_because_every_none_take_records_every_alone_rules_refusal(self):
        # Acceptance.refusals records one refused:<rule> per alone rule that did not take a node, so every flip through
        # `none` carried the same dozen; ranked, they topped the burn-down (dated-title, 37 of 46 flips, 2026-11-08)
        # while deciding nothing. They stay in the country's file; the ranking reads only the deciding rules.
        self.shard("de", 1, 1, row("A", "x", "refused:dated-title:why veto:v join:same-title"),
                   row("B", "y", "refused:dated-title:why join:same-title"), row("C", "z", "refused:dated-title:why refused:sole-work:w"))
        report.report(self.shards, {"de": 1}, self.previous, self.out)
        ranked = [(r[0], r[1], r[2], r[4]) for r in self.lines("family-flips-ranked.tsv")]
        self.assertEqual(ranked, [("1", "join:same-title", "2", "A"), ("1", "join:same-title", "2", "B"),
                                  ("2", "veto:v", "1", "A"), ("3", "(refusals only)", "1", "C")])
        self.assertIn("refused:dated-title:why", self.lines("family-flips-de.tsv")[0][9])

    def test_the_first_night_is_a_baseline_and_the_next_names_what_is_new(self):
        self.shard("de", 1, 1, row("A", "x", "r"))
        summary = report.report(self.shards, {"de": 1}, self.previous, self.out)
        self.assertIn("baseline", summary)
        self.assertEqual(self.lines("family-flips-new.tsv"), [])
        os.replace(os.path.join(self.out, "family-flips-de.tsv"), os.path.join(self.previous, "family-flips-de.tsv"))
        self.shard("de", 1, 1, row("A", "x", "r"), row("B", "y", "r"))
        report.report(self.shards, {"de": 1}, self.previous, self.out)
        self.assertEqual([r[4] for r in self.lines("family-flips-new.tsv")], ["B"])

    def test_a_country_with_a_shard_missing_publishes_nothing_and_keeps_last_nights_ranking(self):
        write(os.path.join(self.previous, "family-flips-uk.tsv"), row("K", "k", "old:rule", "uk"))
        self.shard("uk", 1, 2, row("N", "n", "new:rule", "uk"))
        self.shard("de", 1, 1, row("A", "x", "r"))
        summary = report.report(self.shards, {"uk": 2, "de": 1}, self.previous, self.out)
        self.assertFalse(os.path.exists(os.path.join(self.out, "family-flips-uk.tsv")))
        self.assertIn("shard(s) 2 of 2 missing", summary)
        self.assertEqual(sorted(r[1] for r in self.lines("family-flips-ranked.tsv")), ["old:rule", "r"])

    def test_a_file_that_is_not_a_flips_table_is_refused(self):
        os.makedirs(os.path.join(self.shards, "identity-family-de-1of1"))
        with open(os.path.join(self.shards, "identity-family-de-1of1", "family-flips-de.tsv"), "w") as f:
            f.write("a\tb\n")
        self.assertEqual(report.main(["--shards", self.shards, "--expect", "de=1", "--previous", self.previous, "--out", self.out]), 2)


if __name__ == "__main__":
    unittest.main()
