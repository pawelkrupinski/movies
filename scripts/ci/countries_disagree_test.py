#!/usr/bin/env python3
"""countries_disagree.py: a title one country takes as a film no other country takes, in the same week, listed for
triage -- unjudged as `judge`, a side judged wrong as `check`; both sides judged right, all-event titles, takes the
billing backs and other weeks left out. And the Identity findings report carrying the section."""

import json
import os
import sys
import tempfile
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import countries_disagree as cd  # noqa: E402
import identity_findings_report as report  # noqa: E402

DAY = 86400000
WEEK = 1_700_000_000_000
LATER = WEEK + 30 * DAY

# (cc, venue, raw title, film, rules, year billed, first showtime)
LISTINGS = [
    # unjudged on both sides: the UK takes the 1984 short, Germany the 2026 film -> judge
    ("uk", "Prince Charles Cinema", "The Runner", 213938, [], None, WEEK),
    ("de", "Astor Berlin", "Runner", 1377237, [], None, WEEK),
    ("de", "Kino Arsenal", "Runner", 1377237, [], None, WEEK),
    # both judged right: two releases of one title -> left out
    ("uk", "BFI Southbank", "Girlfriends", 1388338, [], None, WEEK),
    ("de", "Filmmuseum München", "Girlfriends", 111469, [], None, WEEK),
    # Germany's take judged wrong, the UK's right, the US's unjudged on the same film -> check
    ("de", "Kino Lumiere", "Fallen Angels by Noël Coward", 11220, [], None, WEEK),
    ("uk", "Curzon Soho", "Fallen Angels by Noël Coward", 1702350, [], None, WEEK),
    ("uk", "Picturehouse Central", "Fallen Angels by Noël Coward", 1702350, [], None, WEEK),
    ("us", "Film Forum", "Fallen Angels by Noël Coward", 1702350, [], None, WEEK),
    # two houses' relays of one ballet -> left out
    ("uk", "Odeon Leicester Square", "Swan Lake", 900, ["title:xtra-canonical-royal-ballet-opera"], None, WEEK),
    ("de", "CineStar Berlin", "Swan Lake", 901, ["accept:stage-production"], None, WEEK),
    # the billing states its year, agreeing with the record it takes -> no one's to triage
    ("uk", "Prince Charles Cinema", "Halloween (2007)", 2082, [], None, WEEK),
    ("de", "Astor Berlin", "Halloween", 948, [], None, WEEK),
    ("uk", "BFI Southbank", "Halloween", 948, [], None, WEEK),
    # a month apart -> not the same week
    ("uk", "BFI Southbank", "Fame", 3537, [], None, WEEK),
    ("de", "Astor Berlin", "Fame", 28665, [], None, LATER),
]
FILMS = {213938: ("The Runner", 1984), 1377237: ("Runner", 2026), 1388338: ("Girlfriends", 2025),
         111469: ("Girlfriends", 1978), 11220: ("Fallen Angels", 1995), 1702350: ("Fallen Angels", 2026),
         900: ("Swan Lake", 2026), 901: ("Swan Lake", 2025), 2082: ("Halloween", 2007), 948: ("Halloween", 1978),
         3537: ("Fame", 1980), 28665: ("Fame", 2009)}
JUDGEMENTS = [
    ("uk", "*", "Girlfriends", "tmdb:1388338", "right"),
    ("de", "*", "Girlfriends", "tmdb:111469", "right"),
    ("de", "*", "Fallen Angels by Noël Coward", "tmdb:11220", "wrong"),
    ("uk", "*", "Fallen Angels by Noël Coward", "tmdb:1702350", "right"),
]


def write_jsonl(path, rows):
    with open(path, "w", encoding="utf-8") as f:
        for row in rows:
            f.write(json.dumps(row) + "\n")


def fixture(directory):
    """(export directory, judgements file) of LISTINGS, FILMS and JUDGEMENTS."""
    data = os.path.join(directory, "export")
    os.makedirs(data)
    for cc in ("pl", "uk", "de", "es", "us"):
        mine = [l for l in LISTINGS if l[0] == cc]
        write_jsonl(os.path.join(data, f"traces-{cc}.jsonl"),
                    [{"v": v, "r": r, "f": f, "b": "OwnMatch", "ru": rules} for _, v, r, f, rules, _, _ in mine])
        write_jsonl(os.path.join(data, f"listings-{cc}.jsonl"),
                    [{"v": v, "t": r, "o": None, "y": y, "d": [], "s0": s, "s1": s + DAY, "n": 2}
                     for _, v, r, _, _, y, s in mine])
        write_jsonl(os.path.join(data, f"films-{cc}.jsonl"),
                    [{"id": str(f), "t": FILMS[f][0], "y": FILMS[f][1], "d": []} for f in {l[3] for l in mine}])
    judgements = os.path.join(directory, "identity-judgements.tsv")
    with open(judgements, "w", encoding="utf-8") as f:
        f.write("country\tvenue\trawTitle\tfilm\tverdict\tstate\tscope\tfilmTitle\treason\tjudged\tevidence\n")
        for cc, venue, raw, film, verdict in JUDGEMENTS:
            f.write(f"{cc}\t{venue}\t{raw}\t{film}\t{verdict}\topen\t\t\t\t\t\n")
    return data, judgements


class CountriesDisagreeTest(unittest.TestCase):
    def setUp(self):
        self.data, self.judgements = fixture(tempfile.mkdtemp())
        self.found = cd.read_found(self.data, self.judgements)

    def test_lists_the_unjudged_disagreement_and_flags_the_wrong_side_only(self):
        self.assertEqual([(d["key"], d["action"]) for d in self.found],
                         [("fallen angels by noel coward", "check"), ("runner", "judge")])

    def test_the_lone_take_comes_first_with_its_judgement_and_the_others_after(self):
        fallen = self.found[0]
        self.assertEqual([(t["cc"], t["film"], t["verdict"]) for t in fallen["lone"]], [("de", 11220, "wrong")])
        line = cd.line_of(fallen)
        self.assertIn("**check** «fallen angels by noel coward» (4 listings)", line)
        self.assertIn("de «Fallen Angels by Noël Coward» → tmdb:11220 Fallen Angels (1995) OwnMatch ×1 wrong", line)
        self.assertIn("| vs uk/us → tmdb:1702350 Fallen Angels (2026) ×3 right/unjudged", line)

    def test_section_holds_only_the_titles_the_country_bills(self):
        section = "\n".join(cd.section(self.found, "uk"))
        self.assertIn("## Countries disagree (2)", section)
        self.assertEqual(cd.section(self.found, "pl"), [])

    def test_the_findings_report_carries_the_section(self):
        body, _ = report.render("de", [], {}, None, 3, "1", "https://x/1", cd.section(self.found, "de"))
        self.assertIn("## Countries disagree (2)", body)
        self.assertIn("**judge** «runner»", body)


if __name__ == "__main__":
    unittest.main()
