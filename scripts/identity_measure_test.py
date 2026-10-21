#!/usr/bin/env python3
"""identity_measure.py: each move of a takes-check artifact lands in the bucket its judgements give it, and the verdict
is LANDABLE only with no right take lost or turned wrong, no new wrong take, and every country measured."""

import io
import json
import os
import sys
import tempfile
import unittest
from contextlib import redirect_stdout

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import identity_measure as m  # noqa: E402

HEADER = "country\tvenue\trawTitle\tfilm\tverdict\tstate\tscope\tfilmTitle\treason\tjudged\tevidence\n"
RATCHET_HEADER = "status\tcountry\tvenue\trawTitle\tfilm\ttitle\tbasis\tids\twas\texplanation\n"


def judged(cc, venue, title, film, verdict, state="open", scope="", reason=""):
    return "\t".join([cc, venue, title, film, verdict, state, scope, "", reason or ("why" if state != "open" else ""),
                      "2026-10-14", ""]) + "\n"


def decision(venue, title, film, title_of=None):
    """One line of decisions-<cc>.jsonl, as integration.IdentityResolveDump writes it."""
    return {"key": f"{venue}|{title}", "venue": venue, "rawTitle": title, "film": film, "filmTitle": title_of,
            "basis": "OwnMatch" if film else "none", "explanation": ["title + year"], "blocker": None if film else "no film",
            "suspect": []}


def ratchet(cc, venue, title, film, status="RIGHT"):
    """One line of ratchet-takes.tsv, as UnmatchedClustersRatchetSpec writes it."""
    return "\t".join([status, cc, venue, title, film, "", "Agreed", film, "", "agreed"]) + "\n"


JUDGEMENTS = [
    judged("pl", "*", "Lalka", "tmdb:1", "right"),
    judged("pl", "*", "Obcy", "tmdb:2", "wrong"),
    judged("pl", "*", "Obcy", "tmdb:3", "wrong", "known-wrong", "served"),
    judged("pl", "*", "Diuna", "tmdb:4", "right"),
    judged("pl", "*", "Diuna", "tmdb:5", "wrong"),
    judged("pl", "*", "Kler", "tmdb:6", "wrong"),
    judged("pl", "*", "Seksmisja", "tmdb:7", "right"),
    judged("pl", "*", "Rejs", "tmdb:8", "right", "accepted-right-loss"),
    judged("pl", "*", "Psy", "tmdb:12", "wrong"),
]

BASE = [decision("Helios", "Lalka", 1), decision("Rialto", "Obcy", 2), decision("Muza", "Diuna", 4),
        decision("Kino Pod Baranami", "Kler", 6), decision("Muza", "Seksmisja", None), decision("Muza", "Rejs", 8),
        decision("Muza", "Miś", None), decision("Muza", "Vabank", 9), decision("Muza", "Psy", None),
        decision("Muza", "Stable", 11)]
HEAD = [decision("Helios", "Lalka", None),          # right-lost
        decision("Rialto", "Obcy", 3),              # wrong->wrong, onto a known-wrong film
        decision("Muza", "Diuna", 5),               # right->wrong
        decision("Kino Pod Baranami", "Kler", None),  # cure
        decision("Muza", "Seksmisja", 7),           # gained-right
        decision("Muza", "Rejs", None),             # accepted
        decision("Muza", "Miś", 10),                # gained-unjudged
        decision("Muza", "Vabank", None),           # unjudged loss
        decision("Muza", "Psy", 12),                # gained a wrong film: new-wrong
        decision("Muza", "Stable", 11)]             # no move


class MeasureTest(unittest.TestCase):
    def setUp(self):
        self.dir = tempfile.mkdtemp()
        self.judgements = self.write("identity-judgements.tsv", HEADER + "".join(JUDGEMENTS))

    def write(self, rel, text):
        path = os.path.join(self.dir, rel)
        os.makedirs(os.path.dirname(path), exist_ok=True)
        with open(path, "w", encoding="utf-8") as f:
            f.write(text)
        return path

    def artifact(self, cc, base, head, base_final=None, head_final=None, verdict="identity takes: lost 3"):
        root = f"dl/identity-takes-{cc}"
        self.write(f"{root}/verdict.txt", verdict + "\n")
        self.write(f"{root}/base.txt", "abc123\n")
        for side, rows in (("base", base), ("head", head)):
            if rows is not None:
                self.write(f"{root}/{side}/decisions-{cc}.jsonl", "".join(json.dumps(r) + "\n" for r in rows))
        for side, lines in (("base", base_final), ("head", head_final)):
            if lines is not None:
                self.write(f"{root}/{side}/ratchet-takes.tsv", RATCHET_HEADER + "".join(lines))

    def run_measure(self, *extra):
        out = io.StringIO()
        with redirect_stdout(out):
            code = m.main([os.path.join(self.dir, "dl"), "--judgements", self.judgements, *extra])
        return code, out.getvalue()

    def final_none(self):
        return [ratchet("pl", "Muza", "Stable", "tmdb:11")]

    def test_every_move_lands_in_its_judgements_bucket(self):
        self.artifact("pl", BASE, HEAD, self.final_none(), self.final_none())
        code, out = self.run_measure("--countries", "pl", "--json")
        self.assertEqual(1, code)
        model = json.loads(out)["countries"]["pl"]["stages"]["model"]
        named = {b: [e["rawTitle"] for e in es] for b, es in model.items() if es}
        self.assertEqual({"right-lost": ["Lalka"], "wrong->wrong": ["Obcy"], "right->wrong": ["Diuna"], "cure": ["Kler"],
                          "gained-right": ["Seksmisja"], "accepted": ["Rejs"], "gained-unjudged": ["Miś"],
                          "unjudged": ["Vabank"], "new-wrong": ["Psy"]}, named)
        self.assertEqual("known-wrong", model["wrong->wrong"][0]["target"])
        self.assertEqual("tmdb:2", model["wrong->wrong"][0]["before"])

    def test_the_final_takes_are_bucketed_too_and_a_listing_absent_from_one_side_took_none(self):
        self.artifact("pl", BASE[:1], BASE[:1], [ratchet("pl", "Helios", "Lalka", "tmdb:1")], [ratchet("pl", "Muza", "Kler", "tmdb:6")])
        code, out = self.run_measure("--countries", "pl", "--json")
        final = json.loads(out)["countries"]["pl"]["stages"]["final"]
        self.assertEqual(["Lalka"], [e["rawTitle"] for e in final["right-lost"]])
        self.assertEqual(["Kler"], [e["rawTitle"] for e in final["new-wrong"]])
        self.assertEqual(1, code)

    def test_only_cures_gains_judged_right_and_unjudged_moves_are_landable_and_every_unjudged_move_is_listed(self):
        base = [decision("Kino Pod Baranami", "Kler", 6), decision("Muza", "Vabank", 9), decision("Muza", "Miś", None)]
        head = [decision("Kino Pod Baranami", "Kler", None), decision("Muza", "Vabank", None), decision("Muza", "Miś", 10)]
        self.artifact("pl", base, head, self.final_none(), self.final_none())
        code, out = self.run_measure("--countries", "pl")
        self.assertEqual(0, code, out)
        self.assertIn("LANDABLE", out)
        self.assertIn("EVERY UNJUDGED MOVE (2)", out)
        self.assertIn("pl model lost    Muza | Vabank: tmdb:9 -> none", out)
        self.assertIn("pl model gained  Muza | Miś: none -> tmdb:10", out)

    def test_not_landable_names_each_blocking_bucket_and_shows_examples_with_both_films(self):
        self.artifact("pl", BASE, HEAD, self.final_none(), self.final_none())
        code, out = self.run_measure("--countries", "pl", "--examples", "1")
        self.assertEqual(1, code)
        self.assertIn("NOT LANDABLE:", out)
        for reason in ("pl model: 1 right-lost", "pl model: 1 right->wrong", "pl model: 1 new-wrong"):
            self.assertIn(reason, out)
        self.assertIn("pl model wrong->wrong (1):\n  Rialto | Obcy: tmdb:2 -> tmdb:3 [target known-wrong]", out)

    def test_a_row_whose_identity_code_did_not_change_moved_nothing(self):
        self.artifact("uk", None, None, verdict="identity takes (uk): no identity code changed since abc -- nothing can have moved")
        code, out = self.run_measure("--countries", "uk", "--json")
        self.assertEqual(0, code)
        self.assertEqual({}, {b: e for b, e in json.loads(out)["countries"]["uk"]["stages"]["model"].items() if e})

    def test_a_country_with_no_artifact_or_no_resolve_is_no_measurement(self):
        self.artifact("pl", BASE[:1], BASE[:1], self.final_none(), self.final_none())
        self.artifact("de", None, None, verdict="::warning::identity takes (de): the head resolved nothing")
        rows = self.write("rows.json", json.dumps({"pl": "success", "de": "failure", "es": "failure"}))
        code, out = self.run_measure("--countries", "pl,de,es", "--rows", rows)
        self.assertEqual(1, code)
        self.assertIn("de: no model takes in the artifact", out)
        self.assertIn("es: no identity-takes artifact", out)
        self.assertRegex(out, r"pl +model .* success")

    def test_the_takes_row_column_quotes_the_takes_line_over_the_corpus_shape_one(self):
        self.artifact("pl", BASE[:1], BASE[:1], self.final_none(), self.final_none(),
                      verdict="corpus shape (pl): 1 new finding\nidentity takes (pl): 9 listings -- lost 0")
        code, out = self.run_measure("--countries", "pl")
        self.assertRegex(out, r"pl +model .*: identity takes \(pl\): 9 listings")

    def test_moved_out_names_the_countries_whose_takes_moved_the_pre_diff_a_cross_country_change_is_read_from(self):
        self.artifact("pl", BASE, HEAD, self.final_none(), self.final_none())
        self.artifact("de", BASE[:1], BASE[:1], self.final_none(), self.final_none())
        self.artifact("uk", None, None, verdict="identity takes (uk): no identity code changed since abc -- nothing can have moved")
        moved = os.path.join(self.dir, "moved")
        self.run_measure("--countries", "pl,de,uk", "--moved-out", moved)
        with open(moved, encoding="utf-8") as f:
            self.assertEqual("pl", f.read().strip())


if __name__ == "__main__":
    unittest.main()
