#!/usr/bin/env python3
"""Unit tests for convergence-findings.py, over lines copied from real failed legs.

    python3 scripts/test_convergence_findings.py
"""
import importlib.util
import pathlib
import unittest

spec = importlib.util.spec_from_file_location(
    "findings", pathlib.Path(__file__).with_name("convergence-findings.py"))
findings = importlib.util.module_from_spec(spec)
spec.loader.exec_module(findings)


def names(line):
    return list(findings.titles(line))


class ConvergenceFindingsTest(unittest.TestCase):
    def test_mixed_film_split_names_both_sides(self):
        line = ("17:08:21 INFO  services.movies.MixedFilmSplitter - Mixed-film split: 'It' (2017) — Bright Star "
                "Cinemas screens a different film [director: Tommy Lee Wallace; original: —] → re-diverted to "
                "staging as 'It (1990)' (?) with 1 showtime(s).")
        self.assertEqual(names(line), ["It", "It (1990)"])

    def test_record_key_sets_keep_titles_with_commas_and_brackets(self):
        line = ("record keys differ: onlypass1=HashSet((The Hunger Games: Mockingjay - Part 2,Some(2015))) "
                "onlypass0=HashSet((Pat Garrett, the Kid (1973),None))")
        self.assertEqual(names(line), ["The Hunger Games: Mockingjay - Part 2", "Pat Garrett, the Kid (1973)"])

    def test_rediverted_entries_give_their_sanitized_keys(self):
        line = ("tick 1: 2 known film(s) RE-DIVERTED to staging: (Freiluftkino Hasenheide,bloodisinners), "
                "(Sputnik Südstern,bmovielustisoundinwestberlin1979)")
        self.assertEqual(names(line), ["bloodisinners", "bmovielustisoundinwestberlin1979"])

    def test_order_dependence_names_the_screening_key_and_the_rendered_film(self):
        # PolandConvergenceSpec, recorder run 36016829894 — the order-dependence failure
        # names its film only through these two shapes.
        self.assertEqual(names("      pass2=Map(Cinema1\u241favengerskoniecgryrerelease -> 14)"),
                         ["avengerskoniecgryrerelease"])
        line = ("      pass2=\u2026905ce3ea),Some(Sala 6),List(NAP))))))),ResolvedMovie(avengerskoniecgryrerelease|2026,"
                "Avengers: Koniec Gry (re-release),Some(Avengers: Doomsday),Some(https://med\u2026")
        self.assertEqual(names(line), ["Avengers: Koniec Gry (re-release)"])

    def test_served_corpus_findings_name_the_listing_and_its_film(self):
        line = ("[info]     'Avengers. Koniec gry - dubbing' at Helios Aleja Bielany \u2192 'Avengers: Koniec gry' (2019) "
                "[avengerskoniecgry|2019]: 10 of 16 showtime(s) unserved, first 2026-09-25T11:00")
        self.assertEqual(names(line), ["Avengers. Koniec gry - dubbing", "Avengers: Koniec gry"])
        self.assertEqual(names("[info]     'Bojkot' (2021) [bojkot|2021]"), ["Bojkot"])
        self.assertEqual(names("  'RBO Cinema Season 2026-27: Tosca' (\u2014) [rbocinemaseason202627tosca|] attempt=x"),
                         ["RBO Cinema Season 2026-27: Tosca"])

    def test_ansi_colour_does_not_hide_a_finding(self):
        self.assertEqual(names("\x1b[31mrecord 'Lalka' (Some(2026)):\x1b[0m"), ["Lalka"])

    def test_application_noise_names_nothing(self):
        self.assertEqual(names("16:13:52 INFO  services.staging.MongoStagingFolder - Folded staging group 'it'"), [])


if __name__ == "__main__":
    unittest.main()
