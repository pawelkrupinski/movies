#!/usr/bin/env python3
"""The decisions harvest_sources.py and sweep_filmstarts_ids.py make about a venue.

Run: python3 data/germany/scripts/test_harvest_sources.py
"""
import json
import pathlib
import sys
import tempfile

HERE = pathlib.Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import harvest_sources as hs  # noqa: E402
import roster_venues  # noqa: E402
import sweep_filmstarts_ids as sweep  # noqa: E402
from venue_filters import excluded_reason  # noqa: E402

DATA = HERE.parent


def fs(tid, name, town, postcode="", street=""):
    return {"theaterId": tid, "name": name, "town": town, "rosterCity": town, "postcode": postcode, "street": street}


def found(name, town, postcode, street, lat=51.0, lon=7.0, key="kinoheld:1"):
    return {"key": key, "name": name, "town": town, "postcode": postcode, "street": street, "lat": lat, "lon": lon}


def test_a_bochum_venue_filmstarts_never_listed_is_new():
    capitol = found("Capitol Kino Bochum", "Bochum", "44787", "Kortumstraße 51")
    assert hs.FilmstartsIndex([fs("A0263", "Capitol", "Dortmund", "44137", "Hansastr. 1")]).decide(capitol) == (None, None)


def test_the_same_address_is_the_same_venue_whatever_either_site_calls_it():
    k = found("Cinema Arthouse Kino", "Frankfurt am Main", "60313", "Roßmarkt 7")
    assert hs.FilmstartsIndex([fs("A0216", "Cinema", "Frankfurt", "60313", "Rossmarkt 7")]).decide(k) == ("A0216", "same address")
    assert hs.street_key("Kortumstr. 51") == hs.street_key("Kortumstraße 51") == hs.street_key("Kortumstrasse 51")


def test_a_town_s_only_branch_of_a_chain_is_that_chain_s_venue_there():
    k = found("UCI Kinowelt Ruhr Park", "Bochum", "44791", "Am Einkaufszentrum")
    assert hs.FilmstartsIndex([fs("A9001", "UCI KINOWELT Ruhr Park", "Bochum")]).decide(k)[0] == "A9001"


def test_a_venue_in_another_town_is_never_a_match_however_alike_the_names():
    k = found("Schauburg", "Bremen", "28203", "Vor dem Steintor 114")
    assert hs.FilmstartsIndex([fs("A0700", "Schauburg", "Karlsruhe", "76137", "Marienstraße 16")]).decide(k) == (None, None)


def test_a_middling_name_score_goes_to_a_human():
    k = found("Kino im Kulturhaus", "Musterstadt", "12345", "Hauptstraße 1")
    verdict, rows = hs.FilmstartsIndex([fs("A0001", "Kulturhaus Lichtspiele", "Musterstadt", "12345", "Bahnhofstraße 2")]).decide(k)
    assert verdict == "?" and rows[0][1] == "A0001"


def test_a_kinoheld_and_a_cineamo_venue_at_one_site_are_one_venue():
    cineamo = {**found("Babylon", "Berlin", "10178", "Rosa-Luxemburg-Straße 30", 52.5260, 13.4111, "cineamo:997"),
               "handle": {"id": "997"}}
    kinoheld = {**found("Babylon Berlin", "Berlin", "10178", "Rosa-Luxemburg-Str. 30", 52.5262, 13.4113, "kinoheld:61"),
                "handle": {"id": "61", "path": "/kino/berlin/babylon-berlin"}}
    elsewhere = {**found("Babylon Kreuzberg", "Berlin", "10999", "Dresdener Str. 126", 52.4996, 13.4180, "kinoheld:60"),
                 "handle": {"id": "60", "path": "/kino/berlin/babylon-kreuzberg"}}
    venues = hs.union([cineamo], [kinoheld, elsewhere])
    assert [sorted(v["handles"]) for v in venues] == [["cineamo", "kinoheld"], ["kinoheld"]]


def test_the_most_complete_source_is_the_primary_and_the_rest_its_fallbacks_in_order():
    # Babylon Mitte, 2026-10-11: Filmstarts 23 shows in the fortnight, Cineamo 52.
    shows = {"filmstarts": [23, 23], "cineamo": [52, 114], "kinoprogramm": [20, 20]}
    assert hs.choose(shows) == ("cineamo", ["filmstarts", "kinoprogramm"], "cineamo")
    # A tie on the window goes to the longer horizon, then to SOURCES order.
    assert hs.choose({"filmstarts": [40, 40], "kinoheld": [40, 90]})[0] == "kinoheld"
    assert hs.choose({"filmstarts": [40, 40], "cineamo": [40, 40]})[0] == "filmstarts"


def test_a_default_order_serves_its_first_source_that_lists_anything():
    order = ("cineamo", "filmstarts", "kinoheld", "kinoprogramm")
    assert hs.choose({"filmstarts": [30, 30], "cineamo": [28, 90]}, order) == ("cineamo", ["filmstarts"], "filmstarts")
    assert hs.choose({"filmstarts": [30, 30], "cineamo": [0, 0]}, order) == ("filmstarts", ["cineamo"], "filmstarts")


def test_shows_count_by_the_german_day_they_start_on():
    assert hs.berlin_day("2026-10-11T22:30:00Z") == "2026-10-12"
    assert hs.counts(["2026-10-10", "2026-10-11", "2026-10-24", "2026-10-25", "2027-01-01"], "2026-10-11") == [2, 4]


def test_open_air_screens_and_sex_cinemas_never_join():
    assert excluded_reason("Fiege-KinoOpenAir") == "open-air/drive-in"
    assert excluded_reason("Sommerkino im Park") == "open-air/drive-in"
    assert excluded_reason("Anything", is_open_air=True) == "open-air/drive-in"
    assert excluded_reason("Erotik-Kino Blue Movie") == "sex cinema"
    assert excluded_reason("Capitol Kino Bochum") is None
    assert excluded_reason("Casablanca Filmtheater Bochum") is None


def test_a_filmstarts_venue_page_gives_its_address_and_showtime_days():
    page = (HERE / "fixtures" / "filmstarts_A0003_trimmed.html").read_text()
    venue = sweep.parse_venue(page)
    assert venue["name"] == "Abaton"
    assert (venue["street"], venue["postcode"], venue["town"]) == ("Allende-Platz 3 - Grindelhof", "20146", "Hamburg")
    assert venue["showDays"][0] == "2026-10-11"
    assert sweep.parse_venue("<html>404</html>") is None


def test_the_roster_reads_from_venues_json_once_it_exists():
    assert any(v["theaterId"] == "A0003" for v in roster_venues.load(str(DATA)))
    with tempfile.TemporaryDirectory() as d:
        pathlib.Path(d, "venues.json").write_text(json.dumps(
            [{"theaterId": "A0003", "name": "Abaton", "displayName": "Abaton", "city": "Hamburg"}]))
        assert roster_venues.load(d) == [{"theaterId": "A0003", "name": "Abaton", "displayName": "Abaton", "city": "Hamburg"}]


def test_both_scripts_cache_outside_any_session_scratchpad():
    from polite_fetch import cache_dir
    assert cache_dir("kh-harvest", {}, pathlib.Path("/home/u")) == "/home/u/.cache/kinowo/kh-harvest"
    assert cache_dir("kh-harvest", {"X": "/x"}, pathlib.Path("/home/u"), "X") == "/x"


if __name__ == "__main__":
    for name, test in list(globals().items()):
        if name.startswith("test_"):
            test()
            print(f"ok  {name}")
