#!/usr/bin/env python3
"""Unit tests for recording_diff.py over fabricated scrape corpora and enrichment trees.

    python3 scripts/recording_diff_test.py
"""
import gzip
import importlib.util
import json
import os
import pathlib
import tempfile
import unittest

spec = importlib.util.spec_from_file_location("recording_diff", pathlib.Path(__file__).with_name("recording_diff.py"))
diff = importlib.util.module_from_spec(spec)
spec.loader.exec_module(diff)


def film(title, raw=None, **fields):
    movie = {"title": title, "countries": [], "genres": []}
    for key in ("runtimeMinutes", "releaseYear", "originalTitle"):
        if key in fields:
            movie[key] = fields.pop(key)
    if raw:
        movie["rawTitle"] = raw
    entry = {"movie": movie, "cast": fields.pop("cast", []), "director": fields.pop("director", []),
             "showtimes": [], "externalIds": fields.pop("externalIds", {})}
    entry.update(fields)
    return entry


def venue(name, *films):
    return {"_id": name, "city": "X", "scrapedAt": "2026-10-14T00:00:00Z", "listingComplete": True, "films": list(films)}


class RespellingTest(unittest.TestCase):
    def test_whitespace_case_punctuation_accents_are_named(self):
        self.assertEqual(diff.respelling("Lieblingsmenschen - Die  Freundschaft", "Lieblingsmenschen - Die Freundschaft"),
                         ["whitespace"])
        self.assertEqual(diff.respelling("LIEBLINGSMENSCHEN", "Lieblingsmenschen"), ["case"])
        self.assertEqual(diff.respelling("Agnes & Amir!", "Agnes & Amir"), ["punctuation"])
        self.assertEqual(diff.respelling("Agnes & Amir", "Agnes und Amir"), None)
        self.assertEqual(diff.respelling("Mädchen", "Madchen"), ["accents"])
        self.assertEqual(diff.respelling("Sneak Over", "SNEAK-OVER"), ["case", "punctuation"])

    def test_truncation_is_a_prefix_of_the_longer_title(self):
        self.assertEqual(diff.respelling("Lieblingsmenschen - Die außergewöhnliche Freundschaft von A",
                                         "Lieblingsmenschen - Die außergewöhnliche Freundschaft von Agnes und Amir"),
                         ["truncation"])
        self.assertEqual(diff.respelling("Lieblingsmenschen - Die außergewöhnliche  Freundschaft von A…",
                                         "Lieblingsmenschen - Die außergewöhnliche Freundschaft von Agnes und Amir"),
                         ["whitespace", "punctuation", "truncation"])

    def test_a_different_title_or_a_short_prefix_is_no_respelling(self):
        self.assertIsNone(diff.respelling("Alien", "Aliens"))   # too short to call a truncation
        self.assertIsNone(diff.respelling("Lieblingsmenschen", "Lieblingsmenschen - Die außergewöhnliche Freundschaft"))
        self.assertIsNone(diff.respelling("Hamnet", "Hamlet"))


class CorpusDiffTest(unittest.TestCase):
    def test_listings_added_lost_and_respelled_per_venue(self):
        a = [venue("Astor", film("Lieblingsmenschen - Die außergewöhnliche Freundschaft"), film("Old Film"), film("Kept"))]
        b = [venue("Astor", film("Lieblingsmenschen - Die außergewöhnliche  Freundschaft"), film("New Film"), film("Kept"))]
        result = diff.compare_corpus(a, b)
        astor = result["venues"]["Astor"]
        self.assertEqual(astor["added"], ["New Film"])
        self.assertEqual(astor["lost"], ["Old Film"])
        self.assertEqual(astor["respelled"], [("Lieblingsmenschen - Die außergewöhnliche Freundschaft",
                                               "Lieblingsmenschen - Die außergewöhnliche  Freundschaft", ["whitespace"])])

    def test_the_raw_title_is_the_listing_when_the_scraper_kept_one(self):
        a = [venue("Kino", film("Sneak", raw="Sneak_nov + Filmquiz"))]
        b = [venue("Kino", film("Sneak", raw="Sneak Over"))]
        astor = diff.compare_corpus(a, b)["venues"]["Kino"]
        self.assertEqual((astor["added"], astor["lost"]), (["Sneak Over"], ["Sneak_nov + Filmquiz"]))

    def test_a_listing_retitled_with_its_showtimes_kept_is_the_same_listing(self):
        def at(*times):
            return [{"dateTime": t, "format": []} for t in times]
        a = [venue("Filmwerk", film("Sneak_nov + Filmquiz", showtimes=at("2026-11-02T20:30:00")),
                   film("Other", showtimes=at("2026-11-03T20:30:00")))]
        b = [venue("Filmwerk", film("Sneak Over", showtimes=at("2026-11-02T20:30:00")),
                   film("Another", showtimes=at("2026-11-03T20:30:00", "2026-11-04T20:30:00")))]
        kino = diff.compare_corpus(a, b)["venues"]["Filmwerk"]
        self.assertEqual(kino["retitled"], [("Sneak_nov + Filmquiz", "Sneak Over")])
        self.assertEqual((kino["added"], kino["lost"]), (["Another"], ["Other"]))
        out = diff.report("de", a, b, None, None, title_filter="sneak")
        self.assertIn("«Sneak_nov + Filmquiz» → «Sneak Over»: RE-TITLED (same showtimes)", out)
        self.assertIn("RE-TITLED", diff.report("de", a, b, None, None))

    def test_venues_added_and_lost(self):
        result = diff.compare_corpus([venue("Gone", film("A"))], [venue("New", film("B"))])
        self.assertEqual((result["venues_added"], result["venues_lost"]), (["New"], ["Gone"]))

    def test_field_changes_on_listings_present_in_both(self):
        a = [venue("Cineworld", *[film(f"F{i}", runtimeMinutes=100, synopsis="long text", director=["D"],
                                       externalIds={"boxoffice": str(i)}) for i in range(4)])]
        b = [venue("Cineworld", *[film(f"F{i}", runtimeMinutes=100 + (i == 0), externalIds={"boxoffice": str(i)})
                                  for i in range(4)])]
        fields = diff.compare_corpus(a, b)["venues"]["Cineworld"]["fields"]
        self.assertEqual(fields["synopsis"], {"lost": 4, "gained": 0, "changed": 0})
        self.assertEqual(fields["director"], {"lost": 4, "gained": 0, "changed": 0})
        self.assertEqual(fields["runtime"], {"lost": 0, "gained": 0, "changed": 1})
        self.assertNotIn("feed:boxoffice", fields)   # unchanged fields are not reported

    def test_detail_pages_vanish_when_a_venue_s_detail_fields_go(self):
        a = [venue("Cineworld", *[film(f"F{i}", runtimeMinutes=100, synopsis="s") for i in range(4)]),
             venue("Steady", *[film(f"F{i}", runtimeMinutes=100, synopsis="s") for i in range(4)])]
        b = [venue("Cineworld", *[film(f"F{i}") for i in range(4)]),
             venue("Steady", *[film(f"F{i}", runtimeMinutes=100, synopsis="s") for i in range(4)])]
        result = diff.compare_corpus(a, b)
        self.assertEqual(result["detail_vanished"], [("Cineworld", 1.0, 0.0, 4)])
        self.assertEqual(result["detail_appeared"], [])
        reversed_ = diff.compare_corpus(b, a)
        self.assertEqual(reversed_["detail_appeared"], [("Cineworld", 0.0, 1.0, 4)])

    def test_new_spellings_of_known_markers(self):
        a = [venue("Filmwerk", film("Sneak_nov + Filmquiz")), venue("Kino", film("Pokaz specjalny: Film"))]
        b = [venue("Filmwerk", film("Sneak_dez + Filmquiz"), film("Sneak Over")),
             venue("Kino", film("Pokaz specjalny: Other"), film("Pokaz przyjazny ADHD: Film")),
             venue("Odeon", film("Double Bill: A + B"), film("Film (12A)"))]
        markers = diff.compare_corpus(a, b)["markers"]
        new = {(m, s) for m, s, _ in markers["new"]}
        self.assertIn(("sneak", "sneak over"), new)
        self.assertIn(("sneak", "sneak_dez"), new)
        self.assertIn(("pokaz", "pokaz przyjazny"), new)
        self.assertIn(("double bill", "double bill:"), new)
        self.assertIn(("certificate", "(12a)"), new)
        self.assertNotIn(("pokaz", "pokaz specjalny:"), new)
        self.assertEqual({(m, s) for m, s, _ in markers["gone"]}, {("sneak", "sneak_nov")})
        example = dict(((m, s), ex) for m, s, ex in markers["new"])[("pokaz", "pokaz przyjazny")]
        self.assertEqual(example, [("Kino", "Pokaz przyjazny ADHD: Film")])


class OriginsTest(unittest.TestCase):
    def test_an_origin_whose_refusal_share_moved(self):
        a = {"origins": {"www.cineworld.co.uk": {"tree": 50, "answered": 50, "notFound": 0, "refused": 0, "refusals": {}},
                         "api.themoviedb.org": {"tree": 900, "answered": 100, "notFound": 0, "refused": 1, "refusals": {}}}}
        b = {"origins": {"www.cineworld.co.uk": {"tree": 50, "answered": 2, "notFound": 0, "refused": 300,
                                                 "refusals": {"403": 300}},
                         "api.themoviedb.org": {"tree": 900, "answered": 100, "notFound": 0, "refused": 2, "refusals": {}}}}
        moved = diff.compare_origins(a, b)
        self.assertEqual([m["origin"] for m in moved], ["www.cineworld.co.uk"])
        self.assertEqual(moved[0]["after"], "300/302 (99%)")
        self.assertEqual(moved[0]["verdict"], "refusing")

    def test_an_origin_gone_from_the_tree(self):
        a = {"origins": {"x.org": {"tree": 40, "answered": 40, "notFound": 0, "refused": 0, "refusals": {}}}}
        b = {"origins": {}}
        self.assertEqual([m["verdict"] for m in diff.compare_origins(a, b)], ["gone"])


class ReportTest(unittest.TestCase):
    def test_report_names_the_data_changes_and_filters_by_venue(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = pathlib.Path(tmp)
            for run, films in (("A", [film("Lieblingsmenschen - Die  Freundschaft"), film("Old")]),
                               ("B", [film("Lieblingsmenschen - Die Freundschaft"), film("Sneak Over")])):
                (root / run / "corpus").mkdir(parents=True)
                with gzip.open(root / run / "corpus" / "cinema-scrapes-de.json.gz", "wt") as out:
                    json.dump([venue("Astor HafenCity", *films), venue("Other", film("Z"))], out)
            out = diff.report("de", diff.load_corpus(root / "A" / "corpus", "de"),
                              diff.load_corpus(root / "B" / "corpus", "de"), None, None, venue_filter="hafencity")
            self.assertIn("RE-SPELLED (whitespace)", out)
            self.assertIn("Lieblingsmenschen - Die  Freundschaft", out)
            self.assertIn("+ Sneak Over", out)
            self.assertIn("- Old", out)
            self.assertNotIn("Other", out)
            self.assertIn("sneak over", out)

    def test_a_listing_billed_alike_in_both_says_the_data_did_not_move(self):
        title = "Lieblingsmenschen - Die außergewöhnliche  Freundschaft von A"
        a = [venue("Astor Film Lounge", film(title, runtimeMinutes=100)), venue("Other", film("Lieblingsmenschen"))]
        b = [venue("Astor Film Lounge", film(title, runtimeMinutes=100)), venue("Other", film("LIEBLINGSMENSCHEN"))]
        out = diff.report("de", a, b, None, None, venue_filter="astor", title_filter="lieblingsmenschen")
        self.assertIn(f"«{title}»: in both, spelled alike, fields alike", out)
        self.assertNotIn("Other", out)
        out = diff.report("de", a, b, None, None, title_filter="lieblingsmenschen")
        self.assertIn("«Lieblingsmenschen» → «LIEBLINGSMENSCHEN»: RE-SPELLED (case)", out)

    def test_a_listing_s_field_change_is_shown_value_by_value(self):
        a = [venue("Kino", film("Sneak_nov", runtimeMinutes=100, externalIds={"cineamo": "1m"}))]
        b = [venue("Kino", film("Sneak_nov", runtimeMinutes=8, externalIds={"cineamo": "2m"}))]
        out = diff.report("de", a, b, None, None, title_filter="sneak")
        self.assertIn("runtime 100 → 8", out)
        self.assertIn("feed:cineamo 1m → 2m", out)


if __name__ == "__main__":
    unittest.main()
