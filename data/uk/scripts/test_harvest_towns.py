#!/usr/bin/env python3
"""Unit test for harvest_towns.town_of — the rule that reads a town out of a UK
postal address.

The rule is not a guess, and this is where that is kept honest. Cineworld's
recorded venue fixture carries BOTH the free-text address Flicks-style parsing
has to cope with and the chain's own `addressInfo.city`, for 87 UK venues — so
the parser is scored against a source that already knows the answer. It has to
agree on every one of them.

That corpus is what found the rule. Reading the last comma-separated part (the
obvious first guess) scores 86/87, and the one it misses is the shape that
matters: "…, Speke Road, L24 8QB, Speke, Merseyside" ends in a COUNTY. The
postcode, not the position, is what marks the town.

Run: python3 data/uk/scripts/test_harvest_towns.py
"""
import glob
import importlib.util
import json
import os

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.abspath(os.path.join(HERE, "..", "..", ".."))

spec = importlib.util.spec_from_file_location("harvest_towns", os.path.join(HERE, "harvest_towns.py"))
ht = importlib.util.module_from_spec(spec)
spec.loader.exec_module(ht)


def test_town_carries_the_postcode():
    assert ht.town_of("Riverside, Wherry Road, Norwich NR1 1XA") == "Norwich"
    assert ht.town_of("High Street, Banchory AB31 5SR") == "Banchory"


def test_postcode_stands_before_the_town():
    assert ht.town_of("Queens Links Leisure Park, Links Road, AB24 5EN, Aberdeen") == "Aberdeen"


def test_a_trailing_county_is_not_the_town():
    # The shape the naive "last part" rule gets wrong.
    assert ht.town_of("New Mersey Shopping Park, Speke Road, L24 8QB, Speke, Merseyside") == "Speke"


def test_a_county_carrying_the_postcode_is_not_the_town():
    # The postcode marks the town in almost every address, but not when the
    # COUNTY is what carries it. 8 of the 842 harvested venues read this way.
    assert ht.town_of("Bridge Road, Haslemere, Surrey GU27 2AS") == "Haslemere"
    assert ht.town_of("Derby Square, Epsom, Surrey KT19 8AG") == "Epsom"


def test_an_island_group_is_not_the_town_either():
    # Every island venue in the roster is written this way.
    assert ht.town_of("Kenneth Street, Stornoway, Isle of Lewis HS1 2DS") == "Stornoway"
    assert ht.town_of("Central Promenade, Douglas, Isle of Man, IM2 4NA") == "Douglas"
    assert ht.town_of("Pickaquoy Centre, Muddisdale Road, Kirkwall, Okney Islands KW15 1LR") == "Kirkwall"


def test_the_word_county_is_dropped_from_the_town():
    assert ht.town_of("1 Millennium Place, Durham County DH1 1WA") == "Durham"
    assert ht.town_of("Victoria Road, County Hartlepool TS24 8BH") == "Hartlepool"


def test_postcode_last_falls_back_to_the_part_before_it():
    assert ht.town_of("7 Leicester Square, London, WC2H 7NA") == "London"


def test_a_loosely_written_postcode_is_still_a_postcode():
    # Both are in the roster. Missing them is worse than matching loosely: the
    # whole tail then reads as the town, which is how "Coventry CV1 38AZ" became
    # one.
    assert ht.town_of("Sky Dome Leisure Complex, Croft Road, Coventry CV1 38AZ") == "Coventry"
    assert ht.town_of("27 Maney Cor, Birmingham, Sutton Coldfield B72 1Q") == "Sutton Coldfield"


def test_a_county_with_no_comma_in_front_of_it_is_still_dropped():
    assert ht.town_of("18-24 Park Avenue, Whitley Bay Tyne & Wear NE26 1DG") == "Whitley Bay"


def test_a_town_riding_on_the_end_of_a_street_line():
    assert ht.town_of("5A High Street Tisbury, SP3 6LD") == "Tisbury"
    # Only for a part that opens with a house number, or the rule would take
    # ordinary town names apart.
    assert ht.town_of("Halter Street, Bury St Edmunds IP33 1NE") == "Bury St Edmunds"
    assert ht.town_of("Hill Road, Barton upon Humber DN18 5DL") == "Barton upon Humber"


def test_no_postcode_at_all_takes_the_last_part():
    assert ht.town_of("The Old Library, Acton Centre, Ealing") == "Ealing"
    assert ht.town_of("") == ""


def test_sync_keeps_fetches_and_forgets_the_right_venues():
    held = {
        "kept":    {"flicksSlug": "kept",    "displayName": "Kept Cinema",    "town": "Ayr"},
        "gone":    {"flicksSlug": "gone",    "displayName": "Gone Cinema",    "town": "Ely"},
        "renamed": {"flicksSlug": "renamed", "displayName": "Old Name",       "town": "Hull"},
    }
    wired = {"kept": "KeptCinema", "renamed": "RenamedCinema", "brandnew": "BrandNewCinema"}
    names = {"KeptCinema": "Kept Cinema", "RenamedCinema": "New Name", "BrandNewCinema": "Brand New"}

    keep, retired, renamed = ht.sync_plan(held, wired, names)

    assert sorted(keep) == ["kept", "renamed"]
    assert retired == ["gone"]                       # no longer wired: forgotten
    assert renamed == ["New Name"]
    # Re-keyed in place, so the row keeps matching after a rename in Cinema.scala.
    assert keep["renamed"]["displayName"] == "New Name"
    assert keep["renamed"]["town"] == "Hull"         # and keeps the town it had
    # What is left for the sweep to actually fetch: only the new venue.
    assert sorted(set(wired) - set(keep)) == ["brandnew"]


def test_sync_of_an_unchanged_roster_fetches_nothing():
    held = {"a": {"flicksSlug": "a", "displayName": "A Cinema", "town": "Ayr"}}
    keep, retired, renamed = ht.sync_plan(held, {"a": "ACinema"}, {"ACinema": "A Cinema"})
    assert (sorted(set({"a": "ACinema"}) - set(keep)), retired, renamed) == ([], [], [])


def test_agrees_with_cineworlds_own_city_on_every_recorded_venue():
    """The real corpus: 87 UK venues, scored against the chain's own field."""
    matches = glob.glob(
        os.path.join(ROOT, "test/resources/fixtures/cineworld/**/cinemas/**/*.json"), recursive=True)
    assert matches, "no recorded Cineworld venue fixture to score against"

    def venues(node):
        if isinstance(node, dict):
            if isinstance(node.get("address"), str) and "addressInfo" in node:
                yield node
            for value in node.values():
                yield from venues(value)
        elif isinstance(node, list):
            for value in node:
                yield from venues(value)

    scored, wrong, sharper = 0, [], []
    for venue in venues(json.load(open(matches[0]))):
        city = (venue.get("addressInfo") or {}).get("city") or ""
        if not city:
            continue
        scored += 1
        got = ht.town_of(venue["address"])
        # Cineworld writes a couple of its cities as "<town>, <county>"; the
        # town is the half we want, and the half we assert on.
        if got.lower() == city.split(",")[0].strip().lower():
            continue
        # Cineworld itself sometimes files a COUNTY where a town belongs —
        # "Boldon Leisure Park, Bolden Colliery, NE35 9PB, Tyne and Wear" is
        # filed under "Tyne and Wear". Answering the town there is the parser
        # being RIGHT, so it counts as a pass; it is capped so the allowance
        # cannot quietly become the rule.
        (sharper if city.lower() in ht.COUNTIES else wrong).append((venue["address"], city, got))
    assert scored >= 80, f"expected the full venue list, scored only {scored}"
    assert not wrong, f"{len(wrong)}/{scored} disagree, e.g. {wrong[:3]}"
    assert len(sharper) <= 3, f"{len(sharper)} venues fall through the county allowance: {sharper}"


if __name__ == "__main__":
    tests = [(k, v) for k, v in sorted(globals().items()) if k.startswith("test_")]
    for name, fn in tests:
        fn()
        print(f"PASS {name}")
    print(f"\n{len(tests)} tests passed")
