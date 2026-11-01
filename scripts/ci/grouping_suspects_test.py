#!/usr/bin/env python3
"""grouping_suspects.py over a hand-made resolve dump: the ranking, the judged counts, the table, the metrics line.
Run: python3 scripts/ci/grouping_suspects_test.py"""
import json
import os
import sys
import tempfile
import unittest
import unittest.mock

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import grouping_suspects as g  # noqa: E402


def line(key, cluster, venue, raw, film, grouping=None):
    out = {"key": key, "venue": venue, "rawTitle": raw, "cluster": cluster, "film": film}
    if grouping:
        out["grouping"] = grouping
    return json.dumps(out)


PRIMA = {"score": 1.0, "flagged": True, "film": "tmdb:963568", "title": "National Theatre Live: Prima Facie", "size": 2,
         "signals": ["alone"], "involved": ["k1"],
         "findings": [{"kind": "prefers", "signal": "alone", "weight": 1.0, "why": "Chichester 'Prima Facie' alone takes tmdb:1127282"}]}
WEAK = {"score": 0.6, "flagged": False, "film": "tmdb:7", "title": "Lalka", "size": 2, "signals": ["facts"], "involved": ["k3", "k4"],
        "findings": [{"kind": "year", "signal": "facts", "weight": 0.6, "why": "A 'Lalka' states 2026, B 'Lalka' 2023"}]}
ETERNITY = {"score": 2.0, "flagged": True, "film": "tmdb:1259102", "title": "Eternity", "size": 2, "signals": ["facts", "alone"],
            "involved": ["k5", "k6"], "findings": [{"kind": "director", "signal": "facts", "weight": 1.0, "why": "Freyne | Meures"},
                                                    {"kind": "denies", "signal": "alone", "weight": 1.0, "why": "denied"}]}
DUMP = [line("k1", 0, "Chichester", "Prima Facie", 963568, PRIMA), line("k2", 0, "Power Station", "NT Live: Prima Facie", 963568),
        line("k3", 1, "A", "Lalka", 7, WEAK), line("k4", 1, "B", "Lalka", 7),
        line("k5", 2, "Caligari", "Eternity", 1259102, ETERNITY), line("k6", 2, "Zazie", "Eternity", 1259102),
        line("k7", 3, "Muza", "Psy", 9)]
JUDGEMENTS = "# comment\ncountry\tvenue\trawTitle\tfilm\tverdict\nuk\t*\tNT Live: Prima Facie\ttmdb:963568\tright\n" \
             "uk\tChichester\tPrima Facie\ttmdb:963568\twrong\nde\tZazie\tEternity\ttmdb:1259102\twrong\n"


class GroupingSuspectsTest(unittest.TestCase):
    def setUp(self):
        self.dir = tempfile.mkdtemp()
        self.dump = os.path.join(self.dir, "decisions-uk.jsonl")
        with open(self.dump, "w") as handle:
            handle.write("\n".join(DUMP) + "\n")
        self.judgements = os.path.join(self.dir, "judgements.tsv")
        with open(self.judgements, "w") as handle:
            handle.write(JUDGEMENTS)

    def test_ranks_flagged_first_by_score(self):
        ranked = g.suspects(g.read_dump(self.dump), {})
        self.assertEqual([s["film"] for s in ranked], ["tmdb:1259102", "tmdb:963568", "tmdb:7"])
        self.assertEqual([s["rank"] for s in ranked], [1, 2, 3])
        self.assertTrue(ranked[1]["members"][0]["involved"])

    def test_counts_the_judged_members(self):
        ranked = g.suspects(g.read_dump(self.dump), g.read_judgements(self.judgements, "uk"))
        prima = next(s for s in ranked if s["film"] == "tmdb:963568")
        self.assertEqual((prima["judged_right"], prima["judged_wrong"]), (1, 1))
        figures = g.totals("uk", ranked)
        self.assertEqual((figures["flagged"], figures["facts"], figures["alone"], figures["judged_wrong"]), (2, 1, 2, 1))

    def test_writes_the_report_the_summary_and_one_metrics_line(self):
        out, summary, metrics = os.path.join(self.dir, "out"), os.path.join(self.dir, "summary.md"), os.path.join(self.dir, "m.jsonl")
        with unittest.mock.patch.dict(os.environ, {"CI_METRICS_FILE": metrics}):
            self.assertEqual(g.main(["uk", self.dump, "--out", out, "--judgements", self.judgements, "--summary", summary, "--top", "1"]), 0)
        report = json.load(open(os.path.join(out, "grouping-suspects-uk.json")))
        self.assertEqual(report["flagged"], 2)
        self.assertEqual(len(report["suspects"]), 3)
        text = open(summary).read()
        self.assertIn("| 1 | 2.00 | tmdb:1259102 Eternity |", text)
        self.assertNotIn("Prima Facie", text)  # --top 1
        events = [json.loads(l) for l in open(metrics)]
        self.assertEqual([(e["kind"], e["country"], e["flagged"]) for e in events], [(g.KIND, "uk", 2)])

    def test_an_unreadable_dump_is_a_usage_error(self):
        self.assertEqual(g.main(["uk", os.path.join(self.dir, "missing.jsonl"), "--out", self.dir]), 2)


if __name__ == "__main__":
    unittest.main()
