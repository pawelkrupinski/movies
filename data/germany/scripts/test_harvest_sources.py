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
    k = found("Lichtspiele im Kino-Hotel", "Harsefeld", "21698", "Marktstraße 19")
    assert hs.FilmstartsIndex([fs("A0602", "Harsefelder Lichtspiele", "Harsefeld", "21698", "Marktstr. 19")]).decide(k) == \
        ("A0602", "same address")
    assert hs.street_key("Kortumstr. 51") == hs.street_key("Kortumstraße 51") == hs.street_key("Kortumstrasse 51")


def test_one_address_with_two_venues_is_decided_by_name():
    # The Filmhaus Nürnberg's two cinemas share Königstr. 93; an open-air screen is filed
    # under the address of the cinema that runs it.
    index = hs.FilmstartsIndex([fs("A0854", "Filmfabrik KommKino", "Nürnberg", "90402", "Königstrasse 93"),
                                fs("A1418", "Filmhauskino im Künstlerhaus", "Nürnberg", "90402", "Königstrasse 93")])
    assert index.decide(found("Filmhauskino", "Nürnberg", "90402", "Königstraße 93"))[0] == "A1418"
    index = hs.FilmstartsIndex([fs("A1959", "OPEN AIR auf dem Campus FH", "Aschaffenburg", "63739", "Ohmbachgasse 1"),
                                fs("A1342", "Casino Aschaffenburg", "Aschaffenburg", "63739", "Ohmbachsgasse 1")])
    assert index.decide(found("Casino", "Aschaffenburg", "63739", "Ohmbachgasse 1"))[0] == "A1342"


def test_a_town_spelled_with_its_district_and_another_postcode_is_still_the_town():
    index = hs.FilmstartsIndex([fs("A2891", "Movie Magic", "Eberswalde-Finow", "16225", "Heegermühler Strasse 25")])
    assert index.decide(found("Movie Magic", "Eberswalde", "16230", "Heegermühler Straße 25"))[0] == "A2891"


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
    # One name, geocoded 600 m apart by the two sites: still one venue.
    regina_c = {**found("Regina-Filmtheater", "Pegnitz", "91257", "Hauptstr. 1", 49.7560, 11.5420, "cineamo:1232"), "handle": {"id": "1232"}}
    regina_k = {**found("Regina- Filmtheater", "Pegnitz", "91257", "Hauptstr. 1", 49.7510, 11.5440, "kinoheld:2172"),
                "handle": {"id": "2172", "path": "/kino/pegnitz/regina"}}
    assert [sorted(v["handles"]) for v in hs.union([regina_c], [regina_k])] == [["cineamo", "kinoheld"]]


def test_the_most_complete_source_is_the_primary_and_the_rest_its_fallbacks_in_order():
    # Babylon Mitte, 2026-10-11: Filmstarts 23 shows in the fortnight, Cineamo 52.
    shows = {"filmstarts": [23, 23], "cineamo": [52, 114], "kinoprogramm": [20, 20]}
    assert hs.choose(shows) == ("cineamo", ["filmstarts", "kinoprogramm"], "cineamo")
    # As many shows in the window and far more beyond it: the longer horizon wins.
    assert hs.choose({"filmstarts": [40, 40], "kinoheld": [40, 90]})[0] == "kinoheld"
    # A near-tie stays with the earlier source; the raw winner is still recorded.
    assert hs.choose({"filmstarts": [20, 22], "kinoprogramm": [21, 21]}) == ("filmstarts", ["kinoprogramm"], "kinoprogramm")
    assert hs.choose({"filmstarts": [40, 40], "cineamo": [40, 40]})[0] == "filmstarts"
    assert hs.choose({"filmstarts": [0, 1], "cineamo": [46, 49], "kinoheld": [48, 51]})[0] == "cineamo"


def test_a_default_order_serves_its_first_source_that_lists_anything():
    order = ("cineamo", "filmstarts", "kinoheld", "kinoprogramm")
    assert hs.choose({"filmstarts": [30, 30], "cineamo": [28, 90]}, order) == ("cineamo", ["filmstarts"], "filmstarts")
    assert hs.choose({"filmstarts": [30, 30], "cineamo": [0, 0]}, order) == ("filmstarts", ["cineamo"], "filmstarts")


def test_a_venue_s_own_site_leads_with_the_measured_order_behind_it():
    # Steinhaus, Bautzen: no aggregator lists its film Wednesdays; its Wix page does.
    shows = {"filmstarts": [0, 0], "kinoprogramm": [0, 0]}
    r = hs.row("Steinhaus", "Bautzen", "A2231", {}, "/kino/bautzen/steinhaus-73647", shows, None)
    assert r["ownSite"] == hs.OWN_SITES["A2231"]
    assert (r["primary"], r["fallbacks"]) == ("ownSite", ["filmstarts", "kinoprogramm"])
    # A venue with no own site keeps the measured choice.
    plain = hs.row("Abaton", "Hamburg", "A0003", {}, None, {"filmstarts": [5, 5]}, None)
    assert "ownSite" not in plain and plain["primary"] == "filmstarts"


def test_every_own_site_is_a_rostered_venue_s_and_on_its_row():
    rows = {v["key"]: v for v in json.loads((DATA / "venue-sources.json").read_text())["venues"]}
    for tid, site in hs.OWN_SITES.items():
        assert site["site"] in hs.OWN_SITE_KINDS, tid
        assert rows[tid]["rostered"] and rows[tid]["ownSite"] == site and rows[tid]["primary"] == "ownSite", tid


def test_shows_count_by_the_german_day_they_start_on():
    assert hs.berlin_day("2026-10-11T22:30:00Z") == "2026-10-12"
    # Today's shows are left out -- some sources still list the ones already past.
    assert hs.counts(["2026-10-10", "2026-10-11", "2026-10-12", "2026-10-25", "2026-10-26", "2027-01-01"], "2026-10-11") == [2, 4]


def test_the_measured_split_keeps_the_per_venue_choice():
    # The rule (2026-10-11): Cineamo would become every venue's default primary only if it
    # were the most complete source at 90% or more of the venues with a show, and the
    # venues only Filmstarts lists were few or seasonal. Measured: neither held.
    summary = json.loads((DATA / "venue-sources.json").read_text())["summary"]
    assert summary["cineamoWinShare"] < 0.9
    assert len(summary["filmstartsOnly"]) > 20 and sum(v["seasonal"] for v in summary["filmstartsOnly"]) < len(summary["filmstartsOnly"]) / 2
    assert hs.DEFAULT_ORDER is None


def test_open_air_screens_and_sex_cinemas_never_join():
    assert excluded_reason("Fiege-KinoOpenAir") == "open-air/drive-in"
    assert excluded_reason("Sommerkino im Park") == "open-air/drive-in"
    assert excluded_reason("Anything", is_open_air=True) == "open-air/drive-in"
    assert excluded_reason("Erotik-Kino Blue Movie") == "sex cinema"
    assert excluded_reason("Exground Filmfest") == "not a cinema"
    assert excluded_reason("Zeiss-Großplanetarium Berlin") == "not a cinema"
    assert excluded_reason("Capitol Kino Bochum") is None
    assert excluded_reason("Casablanca Filmtheater Bochum") is None


def test_a_filmstarts_venue_page_gives_its_address_and_showtime_days():
    page = (HERE / "fixtures" / "filmstarts_A0003_trimmed.html").read_text()
    venue = sweep.parse_venue(page)
    assert venue["name"] == "Abaton"
    assert (venue["street"], venue["postcode"], venue["town"]) == ("Allende-Platz 3 - Grindelhof", "20146", "Hamburg")
    assert venue["showDays"][0] == "2026-10-11"
    assert sweep.parse_venue("<html>404</html>") is None


def test_the_roster_is_venues_json_less_the_retired_ones():
    with tempfile.TemporaryDirectory() as d:
        pathlib.Path(d, "venues.json").write_text(json.dumps([
            {"theaterId": "A0003", "name": "Abaton", "displayName": "Abaton", "city": "Hamburg"},
            {"theaterId": "A0743", "name": "Kino Kiste", "displayName": "Kino Kiste", "city": "Bad Wörishofen"}]))
        pathlib.Path(d, "retired.json").write_text(json.dumps({"A0743": {"reason": "closed"}}))
        assert roster_venues.load(d) == [{"theaterId": "A0003", "name": "Abaton", "displayName": "Abaton", "city": "Hamburg"}]
    assert "berlin" in roster_venues.page_slugs(str(DATA))


def test_both_scripts_cache_outside_any_session_scratchpad():
    from polite_fetch import cache_dir
    assert cache_dir("kh-harvest", {}, pathlib.Path("/home/u")) == "/home/u/.cache/kinowo/kh-harvest"
    assert cache_dir("kh-harvest", {"X": "/x"}, pathlib.Path("/home/u"), "X") == "/x"


if __name__ == "__main__":
    for name, test in list(globals().items()):
        if name.startswith("test_"):
            test()
            print(f"ok  {name}")
