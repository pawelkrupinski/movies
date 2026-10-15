#!/usr/bin/env python3
"""Tests for Germany's build_pages.py on a synthetic map (no gazetteer), and for
the committed pages.json. The clustering rule itself is Poland's and Spain's too
and is tested in data/pl/scripts/build_pages_test.py; these cover what Germany
adds — majors by population, its tighter radii, town aliases, pinned
coordinates, German slugs, and where a retired region's URL goes.

    python3 data/germany/scripts/build_pages_test.py
"""
import json
import math
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent))
import build_pages as b  # noqa: E402


def point(km_north, km_east):
    lat = 50.0 + km_north / 111.2
    return lat, 8.0 + km_east / (111.2 * math.cos(math.radians(lat)))


def town(km_north, km_east, pop, admin1="05"):
    lat, lon = point(km_north, km_east)
    return {"lat": lat, "lon": lon, "population": pop, "admin1": admin1}


def run(coords, venues, previous, aliases=None, pins=None):
    """venues: (Filmstarts city, displayName) in roster order."""
    town_of, towns = b.load_towns(coords, aliases or {}, pins or {})
    venue_town = {v: town_of(city) for city, v in venues}
    pages, retired = b.build([v for _, v in venues], venue_town, towns, previous, {})
    return {p["slug"]: p for p in pages}, retired


def check(name, cond):
    print(("PASS " if cond else "FAIL ") + name)
    if not cond:
        sys.exit(1)


def refuses(f):
    try:
        f()
    except SystemExit:
        return True
    return False


def region(cinemas, km_north, km_east, qualified=None, anchor=None):
    lat, lon = point(km_north, km_east)
    return {"cinemas": cinemas, "kind": "region", "lat": lat, "lon": lon, "qualifiedSlug": qualified,
            "anchor": anchor}


def synthetic():
    coords = {
        "Frankfurt am Main": town(0, 0, 650000),
        "Frankfurt": town(0, 0, 650000),           # Filmstarts' other spelling of the same city
        "Offenbach am Main": town(-3, 6, 130000),  # a second major, 7 km away
        "Bad Vilbel": town(9, 2, 33000),           # 3 venues → its own page, next to a major
        "Kelkheim": town(2, -15, 28000),           # 1 venue, by Frankfurt: majors absorb nothing
        "Eschborn": town(1, -8, 21000),
        "Lüneburg": town(300, 50, 75000, "06"),    # a 2-venue town, its slug spelt the German way
        "Adendorf": town(303, 50, 11000, "06"),
        "Dahlenburg": town(300, 50 + 31, 3000, "06"),  # 31 km from Lüneburg: Germany absorbs within 30, not 35
        "Uelzen": town(400, 0, 33000, "06"),
        "Bad Bevensen": town(400, 22, 9000, "06"),  # 22 km: outside Germany's 20 km, inside Poland's 25
        "Löhne": town(-100, 0, 39000, "07"),
        "Lohne": town(-100, 0, 39000, "07"),       # GeoNames' Löhne — the venue is Lohne (Oldenburg)
    }
    venues = [("Frankfurt am Main", "F1"), ("Frankfurt", "F2"), ("Offenbach am Main", "O1"),
              ("Bad Vilbel", "V1"), ("Bad Vilbel", "V2"), ("Bad Vilbel", "V3"), ("Kelkheim", "K1"),
              ("Eschborn", "E1"), ("Lüneburg", "L1"), ("Lüneburg", "L2"), ("Adendorf", "A1"), ("Dahlenburg", "D1"),
              ("Uelzen", "U1"), ("Bad Bevensen", "BB1"),
              ("Löhne", "LO1"), ("Lohne", "LO2")]
    previous = {"frankfurt-am-main": region(["F1", "F2", "O1", "V1", "V2", "V3", "K1", "E1"], 0, 0),
                # Named after Eschborn, but most of its venues are Bad Vilbel's now.
                "eschborn": region(["E1", "V1", "V2"], 1, -8, anchor="Eschborn"),
                "lueneburg": region(["L1", "L2", "A1", "D1"], 300, 50),
                "uelzen": region(["U1", "BB1"], 400, 0),
                "minden": region(["LO1", "LO2"], -100, 0, "minden-nordrhein-westfalen")}
    aliases = {"Frankfurt": "Frankfurt am Main"}
    pins = {"Lohne": town(50, 0, 27000, "06")}
    pages, retired = run(coords, venues, previous, aliases, pins)

    check("two spellings of one city are one town",
          pages["frankfurt-am-main"]["towns"] == ["Frankfurt am Main"] and pages["frankfurt-am-main"]["cinemas"] == ["F1", "F2"])
    check("a municipality of 100,000+ is a major city with only its own venues",
          pages["frankfurt-am-main"]["kind"] == "major" and pages["offenbach-am-main"]["kind"] == "major")
    check("a 3-venue town next to a major is a page of its own", pages["bad-vilbel"]["kind"] == "town")
    check("towns by a major cluster among themselves", pages["kelkheim"]["towns"] == ["Kelkheim", "Eschborn"])
    check("a slug transliterates umlauts the German way", "lueneburg" in pages and "luneburg" not in pages)
    check("1-venue towns 22 km apart do not cluster (20 km, not Poland's 25)",
          pages["uelzen"]["towns"] == ["Uelzen"] and pages["bad-bevensen"]["towns"] == ["Bad Bevensen"])
    check("a lone town 31 km from a cluster stays alone (30 km, not Poland's 35)",
          pages["lueneburg"]["towns"] == ["Lüneburg", "Adendorf"] and "dahlenburg" in pages)
    check("a pinned coordinate places the town where it really is", pages["lohne"]["bundesland"] == "Niedersachsen")
    check("a page carries its anchor's Bundesland and the slug qualified with it",
          (pages["lueneburg"]["bundesland"], pages["lueneburg"]["qualifiedSlug"])
          == ("Niedersachsen", "lueneburg-niedersachsen"))
    check("a cluster is named after its anchor", pages["kelkheim"]["kind"] == "cluster" and pages["kelkheim"]["name"] == "Kelkheim")
    check("a region whose slug is still a page is not retired",
          "frankfurt-am-main" not in retired and "lueneburg" not in retired)
    check("a retired region goes to the page holding most of its venues", retired["minden"]["page"] == "loehne")
    check("a retired page keeps the slug it would be qualified to", retired["minden"]["qualifiedSlug"] == "minden-nordrhein-westfalen")
    check("a region named after a town goes to the page holding that town, not the one holding most of its venues",
          retired["eschborn"]["page"] == "kelkheim")
    check("every venue is on exactly one page",
          sorted(c for p in pages.values() for c in p["cinemas"]) == sorted(v for _, v in venues))

    closed = dict(previous, helgoland=region(["H1"], 302, 50))   # its one cinema since retired
    _, retired = run(coords, venues, closed, aliases, pins)
    check("a page none of whose venues is left goes to the page nearest it", retired["helgoland"]["page"] == "lueneburg")

    far = {"Lohne": town(-100, 0, 39000, "07"), "Lohne (Oldenburg)": town(50, 0, 27000, "06")}
    check("a town placed > 60 km from the page its venues were on is refused",
          refuses(lambda: run(far, [("Lohne (Oldenburg)", "LO2")], {"minden": region(["LO2"], -100, 0)})))
    check("unless its coordinate is pinned on purpose",
          not refuses(lambda: run(far, [("Lohne (Oldenburg)", "LO2")], {"minden": region(["LO2"], -100, 0)},
                                  pins={"Lohne (Oldenburg)": far["Lohne (Oldenburg)"]})))
    check("an alias joining two places far apart is refused",
          refuses(lambda: b.load_towns(far, {"Lohne": "Lohne (Oldenburg)"}, {})))
    check("an alias to a town the gazetteer lacks is refused",
          refuses(lambda: b.load_towns(far, {"Lohne": "Nowhere"}, {})))
    check("a venue in a town the gazetteer lacks is refused",
          refuses(lambda: run(far, [("Nowhere", "N1")], {})))


def committed():
    data = json.loads((b.DATA / "pages.json").read_text())
    pages = {p["slug"]: p for p in data["pages"]}
    venues = [c for p in data["pages"] for c in p["cinemas"]]
    roster = b.roster_venues()
    town_of, towns = b.load_towns(b.load_coords())
    count = {}
    for v in roster:
        count[town_of(v["city"])] = count.get(town_of(v["city"]), 0) + 1
    check("pages.json: every rostered venue on exactly one page",
          len(venues) == len(set(venues)) and set(venues) == {v["displayName"] for v in roster})
    check("pages.json: every page or retired slug is unique across both",
          not set(pages) & set(data["retired"]))
    check("pages.json: every retired slug lands on a page", all(r["page"] in pages for r in data["retired"].values()))
    check("pages.json: Berlin keeps only its own venues", pages["berlin"]["towns"] == ["Berlin"])
    check("pages.json: only 1- and 2-venue towns cluster",
          all(count[t] <= 2 for p in pages.values() if p["kind"] == "cluster" for t in p["towns"]))
    check("pages.json: a major or town page is one town", all(len(p["towns"]) == 1 for p in pages.values()
                                                               if p["kind"] in ("major", "town")))
    check("pages.json: no cinema is more than 50 km from the town its page is named after",
          all(b.km((p["lat"], p["lon"]), (towns[t]["lat"], towns[t]["lon"])) <= 50
              for p in pages.values() for t in p["towns"]))


def main():
    synthetic()
    committed()
    print("all passed")


if __name__ == "__main__":
    main()
