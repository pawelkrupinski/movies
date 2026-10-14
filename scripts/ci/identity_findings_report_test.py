#!/usr/bin/env python3
"""identity_findings_report.py: one issue per country, updated in place, closed when empty, red past the age limit."""

import json
import os
import sys
import tempfile
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import identity_findings_report as r  # noqa: E402

# What recording 38625654132 (2026-10-14) brought, which turned PL, UK and DE red with no commit touching any of it.
DIABLY = "suspect\tKino Kosmos\tKF Ambasada: Diabły\ttmdb:31767\tDiabły (1971)\tfragment: 'Diabły' is 1 of the billing's words\n"
NOC = ("suspect\tKino Muranów\t\"Noc żywych trupów\" + wstęp Jacka Rokosza\ttmdb:10331\tNoc żywych trupów (1968)\t"
       "fragment: 'Noc żywych trupów' is 3 of the billing's words\n")
ARRIETTY = "finding\t{venue}\tArrietty (4K Restoration) Experience It In IMAX\tsearches 'Arrietty (4K Restoration) Experience It' (4k → FormatTags)\n"
PREVIEW = "finding\tFilmRaum\tLa bola negra , Preview am Mi. 18.11. um 20 Uhr\tsearches it (preview)\n"


class FakeGitHub:
    """`gh` over an in-memory set of issues, recording every call."""

    def __init__(self):
        self.issues, self.calls = {}, []

    def __call__(self, args, stdin=None):
        self.calls.append(args)
        if args[:2] == ["issue", "list"]:
            return json.dumps([dict(i, number=n) for n, i in self.issues.items()])
        if args[:2] == ["issue", "create"]:
            n = len(self.issues) + 1
            self.issues[n] = {"title": args[args.index("--title") + 1], "body": args[args.index("--body") + 1], "state": "OPEN"}
            return ""
        number = int(args[2]) if len(args) > 2 and args[2].isdigit() else None
        if args[:2] == ["issue", "edit"]:
            self.issues[number]["body"] = args[args.index("--body") + 1]
        elif args[:2] == ["issue", "close"]:
            self.issues[number]["state"] = "CLOSED"
        elif args[:2] == ["issue", "reopen"]:
            self.issues[number]["state"] = "OPEN"
        return ""

    def body(self, cc):
        return next(i["body"] for i in self.issues.values() if i["title"] == r.title_of(cc))

    def state(self, cc):
        return next(i["state"] for i in self.issues.values() if i["title"] == r.title_of(cc))


class FindingsReportTest(unittest.TestCase):
    def setUp(self):
        self.dir = tempfile.mkdtemp()
        self.gh = FakeGitHub()
        self.labels = self.file("labels.tsv", ["country\tvenue\trawTitle\tfilm\tverdict\tnote\n"])
        self.baseline = self.file("baseline.tsv", ["country\trawTitle\tfilm\ttitle\treasons\n"])

    def file(self, name, lines):
        path = os.path.join(self.dir, name)
        os.makedirs(os.path.dirname(path), exist_ok=True)
        with open(path, "w", encoding="utf-8") as f:
            f.write("".join(lines))
        return path

    def recording(self, name, files):
        for (cc, file), lines in files.items():
            self.file(os.path.join(name, cc, file), lines)
        return os.path.join(self.dir, name)

    def run_report(self, diagnostics, today, countries="pl,uk,de"):
        return r.main(["--run", "38625654132", "--countries", countries, "--labels", self.labels, "--baseline", self.baseline,
                       "--today", today, "--diagnostics", diagnostics, "--repo", "o/r", "--assignee", "owner"], run_gh=self.gh)

    RED = {("pl", "pl-suspect-unjudged.txt"): [DIABLY, NOC],
           ("pl", "corpus-shape-recorded-pl-SearchQueryMarkers.txt"): [],
           ("uk", "uk-suspect-unjudged.txt"): [],
           ("uk", "corpus-shape-recorded-uk-SearchQueryMarkers.txt"): [ARRIETTY.format(venue=v) for v in ("Cineworld Barnsley", "Cineworld Belfast")],
           ("de", "de-suspect-unjudged.txt"): [],
           ("de", "corpus-shape-recorded-de-SearchQueryMarkers.txt"): [PREVIEW]}

    def test_a_recordings_findings_open_one_issue_a_country_with_ready_triage_commands(self):
        self.assertEqual(0, self.run_report(self.recording("a", self.RED), "2026-10-14"))
        pl, uk, de = self.gh.body("pl"), self.gh.body("uk"), self.gh.body("de")
        self.assertIn("**2 finding(s)**, **0 untriaged for more than 3 days**", pl)
        self.assertIn("### KF Ambasada: Diabły → tmdb:31767 Diabły (1971)", pl)
        self.assertIn("scripts/identity-slice pl --venue 'Kino Kosmos' --title 'KF Ambasada: Diabły' --recording 38625654132", pl)
        self.assertIn("scripts/identity-slice pl --explain --venue 'Kino Kosmos' --title 'KF Ambasada: Diabły'", pl)
        self.assertIn("scripts/label-lookup pl --title 'KF Ambasada: Diabły' --venue 'Kino Kosmos'", pl)
        # one entry for the chain's listing at both venues
        self.assertIn("## Corpus-shape findings (1)", uk)
        self.assertIn("Cineworld Barnsley, Cineworld Belfast · SearchQueryMarkers · first seen 2026-10-14 (0 d)", uk)
        self.assertIn("scripts/title-trace 'Arrietty (4K Restoration) Experience It In IMAX' --country uk --venue 'Cineworld Barnsley'", uk)
        self.assertIn("scripts/venue-replay uk 'Cineworld Barnsley' --recording 38625654132", uk)
        self.assertIn("La bola negra , Preview am Mi. 18.11. um 20 Uhr", de)
        self.assertEqual(["OPEN"] * 3, [self.gh.state(cc) for cc in ("pl", "uk", "de")])
        self.assertTrue(all("--assignee" in c for c in self.gh.calls if c[:2] == ["issue", "create"]))

    def test_the_issue_is_updated_in_place_keeping_first_seen_and_a_finding_past_three_days_turns_the_report_red(self):
        red = self.recording("a", self.RED)
        self.run_report(red, "2026-10-14")
        self.assertEqual(0, self.run_report(red, "2026-10-17"))  # 3 days: within the limit
        self.assertEqual(3, len(self.gh.issues))  # updated in place, never a second issue
        self.assertEqual(1, self.run_report(red, "2026-10-18"))  # 4 days: overdue
        self.assertIn("### ⏰ KF Ambasada: Diabły", self.gh.body("pl"))
        self.assertIn("first seen 2026-10-14 (4 d)", self.gh.body("pl"))

    def test_a_suspect_judged_since_the_recording_leaves_at_once_and_an_empty_report_closes_the_issue(self):
        red = self.recording("a", self.RED)
        self.run_report(red, "2026-10-14", countries="pl")
        self.labels = self.file("labels.tsv", ["country\tvenue\trawTitle\tfilm\tverdict\tnote\n",
                                               "pl\t*\tKF Ambasada: Diabły\ttmdb:31767\twrong\tKen Russell's is not it\n"])
        self.baseline = self.file("baseline.tsv", ["country\trawTitle\tfilm\ttitle\treasons\n",
                                                   "pl\t\"Noc żywych trupów\" + wstęp Jacka Rokosza\ttmdb:10331\tx\tfragment\n"])
        self.assertEqual(0, self.run_report(red, "2026-10-20", countries="pl"))
        self.assertIn("no finding to triage", self.gh.body("pl"))
        self.assertEqual("CLOSED", self.gh.state("pl"))
        # a later recording bringing one back reopens it, first seen anew
        self.labels = self.file("labels.tsv", ["country\tvenue\trawTitle\tfilm\tverdict\tnote\n"])
        self.assertEqual(0, self.run_report(red, "2026-10-21", countries="pl"))
        self.assertEqual("OPEN", self.gh.state("pl"))
        self.assertIn("first seen 2026-10-21 (0 d)", self.gh.body("pl"))

    def test_a_country_whose_row_left_no_findings_keeps_its_issue_and_none_at_all_is_an_error(self):
        red = self.recording("a", self.RED)
        self.run_report(red, "2026-10-14", countries="pl")
        empty = self.recording("b", {("uk", "other.txt"): []})
        self.assertEqual(2, self.run_report(empty, "2026-10-15", countries="pl"))
        self.assertEqual("OPEN", self.gh.state("pl"))
        self.assertIn("**2 finding(s)**", self.gh.body("pl"))

    def test_a_body_too_long_for_github_lists_the_rest_one_line_each(self):
        many = [f"finding\tKino {n}\tFilm {n} PREMIERA\tsearches 'Film {n} PREMIERA' ({'x' * 300})\n" for n in range(400)]
        self.assertEqual(0, self.run_report(self.recording("a", {("pl", "corpus-shape-recorded-pl-SearchQueryMarkers.txt"): many}),
                                            "2026-10-14", countries="pl"))
        body = self.gh.body("pl")
        self.assertLessEqual(len(body), r.BODY_LIMIT)
        self.assertIn("- **Film 0 PREMIERA**", body)
        self.assertIn("more: the run's `convergence-diagnostics-pl-convergence` artifact", body)
        self.assertEqual(400, len(r.first_seen_of(body)))


if __name__ == "__main__":
    unittest.main()
