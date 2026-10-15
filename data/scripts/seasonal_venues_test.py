#!/usr/bin/env python3
"""Unit tests for seasonal_venues: which names make a venue seasonal, and which do not.

Run: python3 data/scripts/seasonal_venues_test.py
"""
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent))
import seasonal_venues as sv  # noqa: E402


def test_each_countrys_words_for_a_summer_screen():
    for name in ["Freiluftkino Kreuzberg", "Open-Air-Kino Brentanobad", "Openair Kino", "Sommerkino Güstrow",
                 "Sommernachtskino Tübingen", "Autokino Usedom", "Zittauer Filmnächte",
                 "Open Air Strandkorbkino am Bootshafen", "Freilichtbühne Sosa"]:
        assert sv.is_seasonal("de", name), name
    for name in ["Autocine Getxo", "Autocinema Tenerife", "Cine de Verano Abarán", "Terraza Neptuno",
                 "Cine Liszt Terraza de verano"]:
        assert sv.is_seasonal("es", name), name
    for name in ["Admiral Twin Drive-in Tulsa", "411 Twin Drive In Centre", "McHenry Outdoor Theater"]:
        assert sv.is_seasonal("us", name), name


def test_an_ordinary_cinema_is_not_seasonal():
    assert not sv.is_seasonal("de", "Kino Central Berlin")
    assert not sv.is_seasonal("de", "Kinopark Aalen")       # "park" is not a park screen
    assert not sv.is_seasonal("es", "Artesiete Las Terrazas")  # a mall named after terraces
    assert not sv.is_seasonal("us", "Starlight Terra Vista 6")
    assert not sv.is_seasonal("us", "Skyline 8 Cinemas Dillon")


def test_a_matching_name_that_screens_all_year_is_excluded():
    assert sv.PATTERNS["de"].search("Drive In Autokino Essen")
    assert not sv.is_seasonal("de", "Drive In Autokino Essen")
    assert not sv.is_seasonal("es", "Autocine Star")
    assert not sv.is_seasonal("us", "West Wind Glendale Drive-In")
    assert not sv.is_seasonal("us", "Wellfleet Cinemas & Drive-In")


def test_every_exclusion_names_its_evidence_and_still_matches_a_pattern():
    for country, names in sv.NOT_SEASONAL.items():
        for name, evidence in names.items():
            assert evidence, name
            # An exclusion no pattern matches is dead: a rename upstream left it behind.
            assert sv.PATTERNS[country].search(name), name


def test_a_summer_screen_whose_name_does_not_say_so_is_declared_seasonal():
    # Worpswede's Music Hall is a concert venue; its films are open-air nights in August.
    assert not sv.PATTERNS["de"].search("Music Hall Worpswede")
    assert sv.is_seasonal("de", "Music Hall Worpswede")
    assert sv.seasonal_names("de", ["Music Hall Worpswede", "Kino Central Berlin"]) == ["Music Hall Worpswede"]


def test_every_declared_summer_screen_names_its_evidence_and_no_pattern_already_covers_it():
    for country, names in sv.SEASONAL.items():
        for name, evidence in names.items():
            assert evidence, name
            # A declaration a pattern already matches is redundant: the pattern decides it.
            assert not sv.PATTERNS[country].search(name), name
            assert name not in sv.NOT_SEASONAL[country], name


def test_renders_the_sorted_seasonal_names_as_a_scala_set():
    names = sv.seasonal_names("de", ["Sommerkino Klöden", "Kino Central Berlin", "Freiluftkino Rehberge"])
    assert names == ["Freiluftkino Rehberge", "Sommerkino Klöden"]
    out = "\n".join(sv.render_scala(names, lambda s: '"' + s + '"'))
    assert '  val seasonal: Set[String] = Set(\n    "Freiluftkino Rehberge",\n    "Sommerkino Klöden",\n  )' in out


def main():
    tests = [v for k, v in sorted(globals().items()) if k.startswith("test_")]
    for t in tests:
        t()
        print("PASS", t.__name__)
    print(f"\n{len(tests)} tests passed")


if __name__ == "__main__":
    main()
