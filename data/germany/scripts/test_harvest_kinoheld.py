#!/usr/bin/env python3
"""The decisions harvest_kinoheld.py and sweep_filmstarts_ids.py make about a venue.

Run: python3 data/germany/scripts/test_harvest_kinoheld.py
"""
import os
import pathlib
import sys
from collections import defaultdict

HERE = pathlib.Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import harvest_kinoheld as kh  # noqa: E402
import sweep_filmstarts_ids as sweep  # noqa: E402
from venue_filters import excluded_reason  # noqa: E402


def fs(tid, name, town, postcode="", street="", source="roster"):
    return {"theaterId": tid, "name": name, "town": town, "rosterCity": town,
            "postcode": postcode, "street": street, "source": source}


def kinoheld(name, town, postcode, street):
    return {"kinoheldId": "1", "name": name, "town": town, "postcode": postcode, "street": street}


def decide(k, venues):
    by_postcode, by_town, chains = defaultdict(list), defaultdict(list), defaultdict(int)
    for v in venues:
        by_postcode[v["postcode"]].append(v)
        by_town[kh.town_key(v["town"])].append(v)
        chain = kh.kp.chain_of(v["name"])
        if chain:
            chains[(chain, kh.town_key(v["town"]))] += 1
    return kh.decide(k, kh.candidates(k, by_postcode, by_town), chains)


def test_a_bochum_venue_filmstarts_never_listed_is_kinoheld_only():
    capitol = kinoheld("Capitol Kino Bochum", "Bochum", "44787", "Kortumstraße 51")
    assert decide(capitol, [fs("A0263", "Capitol", "Dortmund", "44137", "Hansastr. 1")]) == (None, None)


def test_the_same_address_is_the_same_venue_whatever_either_site_calls_it():
    # Street abbreviations differ between the two sites.
    k = kinoheld("Cinema Arthouse Kino", "Frankfurt am Main", "60313", "Roßmarkt 7")
    assert kh.decide(k, [fs("A0216", "Cinema", "Frankfurt", "60313", "Rossmarkt 7")], defaultdict(int)) == \
        ("A0216", "same address")
    assert kh.street_key("Kortumstr. 51") == kh.street_key("Kortumstraße 51") == kh.street_key("Kortumstrasse 51")


def test_a_town_s_only_branch_of_a_chain_is_that_chain_s_venue_there():
    k = kinoheld("UCI Kinowelt Ruhr Park", "Bochum", "44791", "Am Einkaufszentrum")
    venues = [fs("A9001", "UCI KINOWELT Ruhr Park", "Bochum")]
    assert decide(k, venues)[0] == "A9001"


def test_a_venue_in_another_town_is_never_a_match_however_alike_the_names():
    k = kinoheld("Schauburg", "Bremen", "28203", "Vor dem Steintor 114")
    assert decide(k, [fs("A0700", "Schauburg", "Karlsruhe", "76137", "Marienstraße 16")]) == (None, None)


def test_a_middling_name_score_goes_to_a_human():
    k = kinoheld("Kino im Kulturhaus", "Musterstadt", "12345", "Hauptstraße 1")
    verdict, rows = decide(k, [fs("A0001", "Kulturhaus Lichtspiele", "Musterstadt", "12345", "Bahnhofstraße 2")])
    assert verdict == "?" and rows[0][1] == "A0001"


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


def test_both_scripts_cache_outside_any_session_scratchpad():
    from polite_fetch import cache_dir
    assert cache_dir("kh-harvest", {}, pathlib.Path("/home/u")) == "/home/u/.cache/kinowo/kh-harvest"
    assert cache_dir("kh-harvest", {"X": "/x"}, pathlib.Path("/home/u"), "X") == "/x"


if __name__ == "__main__":
    for name, test in list(globals().items()):
        if name.startswith("test_"):
            test()
            print(f"ok  {name}")
