#!/usr/bin/env python3
"""identity_takes_diff.py: a moved take fails unless accepted; churn and an identical resolve pass."""

import json
import os
import sys
import tempfile
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import identity_takes_diff as d  # noqa: E402


def row(venue, title, film, key=None, title_of=None, basis="OwnMatch", why="title + year"):
    return {"key": key or f"{venue}|{title}", "venue": venue, "rawTitle": title, "film": film,
            "filmTitle": title_of, "basis": basis, "explanation": [why], "blocker": None if film else "pooled:no-film"}


class TakesDiffTest(unittest.TestCase):
    def setUp(self):
        self.dir = tempfile.mkdtemp()

    def write(self, name, lines):
        path = os.path.join(self.dir, name)
        with open(path, "w", encoding="utf-8") as f:
            f.write("".join(l if isinstance(l, str) else json.dumps(l) + "\n" for l in lines))
        return path

    def run_diff(self, base, head, labels=None, accepted=None):
        summary = os.path.join(self.dir, "summary.md")
        args = ["pl", self.write("base.jsonl", base), self.write("head.jsonl", head), "--summary", summary]
        if labels is not None:
            args += ["--labels", self.write("labels.tsv", labels)]
        if accepted is not None:
            args += ["--accepted", self.write("accepted.tsv", accepted)]
        code = d.main(args)
        with open(summary, encoding="utf-8") as f:
            return code, f.read()

    BASE = [row("Helios", "Lalka", 501, title_of="Lalka (2026)"), row("Rialto", "Obcy", 502), row("Muza", "Diuna", None)]

    def test_an_identical_resolve_passes(self):
        code, md = self.run_diff(self.BASE, self.BASE)
        self.assertEqual(0, code)
        self.assertIn("lost 0, changed 0, gained 0", md)

    def test_a_lost_and_a_changed_take_fail_naming_venue_title_films_and_both_reasons(self):
        head = [row("Helios", "Lalka", None, why="refused: year"), row("Rialto", "Obcy", 602, title_of="Alien: Romulus (2024)"),
                row("Muza", "Diuna", None)]
        code, md = self.run_diff(self.BASE, head)
        self.assertEqual(1, code)
        self.assertIn("| ❌ | lost | Helios | Lalka | tmdb:501 Lalka (2026) | none | OwnMatch: title + year | OwnMatch: refused: year", md)
        self.assertIn("| ❌ | changed | Rialto | Obcy | tmdb:502 | tmdb:602 Alien: Romulus (2024) |", md)

    def test_a_gained_take_passes(self):
        head = self.BASE[:2] + [row("Muza", "Diuna", 438631)]
        code, md = self.run_diff(self.BASE, head)
        self.assertEqual(0, code)
        self.assertIn("gained 1", md)

    def test_a_listing_on_one_side_only_is_counted_not_failed(self):
        code, md = self.run_diff(self.BASE, self.BASE[:2])
        self.assertEqual(0, code)
        self.assertIn("1 listing(s) on one side only", md)

    def test_a_respelled_key_pairs_by_venue_and_title(self):
        head = [dict(r, key=r["key"] + "|2026") for r in self.BASE]
        code, _ = self.run_diff(self.BASE, head)
        self.assertEqual(0, code)

    def test_labels_accept_a_move_to_a_right_film_or_off_a_wrong_one_and_reject_a_wrong_new_one(self):
        head = [row("Helios", "Lalka", None), row("Rialto", "Obcy", 602), row("Muza", "Diuna", 7)]
        labels = ["country\tvenue\trawTitle\tfilm\tverdict\tnote\n",
                  "pl\tHelios\tLalka\ttmdb:501\twrong\t\n",
                  "pl\tRialto\tObcy\ttmdb:602\tright\t\n",
                  "pl\tMuza\tDiuna\ttmdb:7\twrong\t\n",
                  "de\tMuza\tDiuna\ttmdb:7\tright\tanother country\n"]
        code, md = self.run_diff(self.BASE, head, labels=labels)
        self.assertEqual(1, code)
        self.assertIn("| ✓ | lost | Helios | Lalka |", md)
        self.assertIn("| ✓ | changed | Rialto | Obcy |", md)
        self.assertIn("| ❌ | gained | Muza | Diuna |", md)

    def test_accepted_tsv_accepts_a_move_at_every_venue_with_a_wildcard(self):
        base = [row("Helios", "Lalka", 501), row("Rialto", "Lalka", 501)]
        head = [row("Helios", "Lalka", 9), row("Rialto", "Lalka", 9)]
        code, _ = self.run_diff(base, head, accepted=["# why\n", "pl\t*\tLalka\ttmdb:501\ttmdb:9\tthe 2026 remake\n"])
        self.assertEqual(0, code)
        code, _ = self.run_diff(base, head, accepted=["uk\t*\tLalka\t*\t*\tanother country\n"])
        self.assertEqual(1, code)

    def test_two_same_titled_listings_at_a_venue_are_never_paired_by_guess(self):
        # Belle (2013) and Belle (2021) at one venue, both re-keyed by the head: pairing them by title would pick one at
        # random and could read as a swap. They are reported as ambiguous instead.
        base = [row("Helios", "Belle", 157827, key="Helios|Belle|2013"), row("Helios", "Belle", 776305, key="Helios|Belle|2021")]
        head = [row("Helios", "Belle", 776305, key="Helios|Belle|2013|x"), row("Helios", "Belle", 157827, key="Helios|Belle|2021|x")]
        code, md = self.run_diff(base, head)
        self.assertEqual(0, code)
        self.assertIn("4 listing(s) on one side only", md)  # two a side
        self.assertIn("lost 0, changed 0", md)

    def test_unreadable_inputs_exit_2_never_1(self):
        base = self.write("b.jsonl", self.BASE)
        self.assertEqual(2, d.main(["pl", base, base, "--labels", os.path.join(self.dir, "missing.tsv")]))
        broken = self.write("broken.jsonl", [{"key": "k", "venue": "Helios", "film": 1}])
        self.assertEqual(2, d.main(["pl", base, broken]))

    def test_an_empty_side_is_an_error_not_a_pass(self):
        self.assertEqual(2, d.main(["pl", self.write("b.jsonl", self.BASE), self.write("h.jsonl", [])]))


if __name__ == "__main__":
    unittest.main()
