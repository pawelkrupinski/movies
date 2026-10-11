#!/usr/bin/env python3
"""Tests for the US build_pages.py on a synthetic map, and for the committed
pages.json. The clustering rule itself is Poland's, Spain's and Germany's too and
is tested in data/pl/scripts/test_build_pages.py; these cover what the US adds —
majors by population, its wider radii, clusters across a state line, a slug that
keeps meaning the town it was named after, and where a retired page's URL goes.

    python3 data/us/scripts/test_build_pages.py
"""
import json
import math
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent))
import build_pages as b  # noqa: E402


def point(km_north, km_east):
    lat = 40.0 + km_north / 111.2
    return lat, -95.0 + km_east / (111.2 * math.cos(math.radians(lat)))


def town(name, state, km_north, km_east, pop):
    lat, lon = point(km_north, km_east)
    return f"{name}, {state}", {"name": name, "state": state, "lat": lat, "lon": lon, "pop": pop}


def run(towns, venues, previous=None, prior_retired=None):
    """towns: [town(...)]; venues: (town key, venue slug) in roster order."""
    towns = dict(towns)
    at = [(v, (towns[t]["lat"], towns[t]["lon"])) for t, v in venues]
    pages, retired = b.build(at, {v: t for t, v in venues}, towns, previous or {}, prior_retired or {})
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


def metro(cinemas, km_north, km_east, anchor=None, qualified=None, slug=None):
    lat, lon = point(km_north, km_east)
    return {"cinemas": cinemas, "kind": "metro", "lat": lat, "lon": lon, "anchor": anchor,
            "qualifiedSlug": qualified or slug}


TOWNS = [
    town("Kansas City", "Missouri", 0, 0, 508_000),
    town("Kansas City", "Kansas", 0, -5, 156_000),       # a second major of the same name, 5 km away
    town("Overland Park", "Kansas", -15, -10, 197_000),  # a major beside both: each keeps only its own
    town("Liberty", "Missouri", 22, 18, 30_000),         # 2 venues, Missouri side
    town("Leavenworth", "Kansas", 30, 5, 37_000),        # 1 venue, 19 km from Liberty, across the line
    town("Olathe", "Kansas", -25, -25, 99_000),          # 99,999 short of major: a town page of 3
    town("Bend", "Oregon", 500, 0, 99_000),
    town("Redmond", "Oregon", 500, 25, 35_000),          # 1 venue, 25 km: outside 20, inside 30 — clusters
    town("Sisters", "Oregon", 500, -31, 3_000),          # 31 km: outside 30, joins within the 40 km absorb
    town("Madras", "Oregon", 541, 0, 7_000),             # 41 km from Bend: past the absorb, alone
    town("Springfield", "Illinois", -300, 300, 114_000),
    town("Springfield", "Missouri", -300, 0, 169_000),   # bigger, but /springfield/ was Illinois's
]
VENUES = [("Kansas City, Missouri", "kc1"), ("Kansas City, Missouri", "kc2"), ("Kansas City, Kansas", "kck1"),
          ("Overland Park, Kansas", "op1"), ("Liberty, Missouri", "li1"), ("Liberty, Missouri", "li2"),
          ("Leavenworth, Kansas", "le1"), ("Olathe, Kansas", "ol1"), ("Olathe, Kansas", "ol2"),
          ("Olathe, Kansas", "ol3"), ("Bend, Oregon", "be1"), ("Redmond, Oregon", "re1"), ("Sisters, Oregon", "si1"),
          ("Madras, Oregon", "ma1"), ("Springfield, Illinois", "sp1"), ("Springfield, Missouri", "sm1")]


def synthetic():
    previous = {
        # The two halves of one travel-shed the per-state metros made.
        "kansas-city": metro(["kc1", "kc2", "li1", "li2"], 0, 0, "Kansas City, Missouri"),
        "kansas-city-kansas": metro(["kck1", "op1", "ol1", "ol2", "ol3", "le1"], 0, -5, "Kansas City, Kansas"),
        "springfield": metro(["sp1"], -300, 300, "Springfield, Illinois"),
        # A regional metro named after no town: its venues decide.
        "central-oregon": metro(["be1", "re1", "si1", "ma1"], 500, 0),
    }
    pages, retired = run(TOWNS, VENUES, previous)

    check("a place of 100,000+ is a major city with only its own venues",
          pages["kansas-city"]["kind"] == "major" and pages["kansas-city"]["cinemas"] == ["kc1", "kc2"])
    check("two majors sharing a name: the second is qualified with its state",
          pages["kansas-city-kansas"]["cinemas"] == ["kck1"] and pages["kansas-city-kansas"]["state"] == "Kansas")
    check("a major never takes in a neighbour", pages["overland-park"]["cinemas"] == ["op1"])
    check("a town just under 100,000 with 3 venues is a town page", pages["olathe"]["kind"] == "town")
    check("a cluster crosses a state line, filed under its anchor's state",
          pages["liberty"]["towns"] == ["Liberty, Missouri", "Leavenworth, Kansas"]
          and pages["liberty"]["state"] == "Missouri")
    check("1-venue towns 25 km apart cluster (30 km, not Germany's 20)",
          pages["bend"]["towns"][:2] == ["Bend, Oregon", "Redmond, Oregon"])
    check("a lone town 31 km away joins within the 40 km absorb", "Sisters, Oregon" in pages["bend"]["towns"])
    check("a lone town 41 km away stays a page of its own", pages["madras"]["towns"] == ["Madras, Oregon"])
    check("a slug keeps meaning the town it was named after, however big its namesake",
          pages["springfield"]["anchor"] == "Springfield, Illinois" and "springfield-missouri" in pages)
    check("a page carries the slug it would be qualified to",
          pages["bend"]["qualifiedSlug"] == "bend-oregon" and pages["kansas-city"]["qualifiedSlug"] == "kansas-city-missouri")
    check("a page still named after its town is not retired", "kansas-city" not in retired)
    check("a retired page named after no town goes to the page holding most of its venues",
          retired["central-oregon"]["page"] == "bend")
    check("every venue is on exactly one page",
          sorted(c for p in pages.values() for c in p["cinemas"]) == sorted(v for _, v in VENUES))

    # Kansas City, Kansas is a major now and keeps its slug; a metro whose
    # namesake town is on a cluster named after another goes to that cluster.
    named = dict(previous, leavenworth=metro(["le1", "ol1"], 30, 5, "Leavenworth, Kansas"))
    _, retired = run(TOWNS, VENUES, named)
    check("a retired page goes to the page holding its namesake town, not the one holding most of its venues",
          retired["leavenworth"]["page"] == "liberty" and retired["leavenworth"]["anchor"] == "Leavenworth, Kansas")

    # Built again from its own output, the build changes nothing: the retired
    # /leavenworth/ stays reserved for Leavenworth, and keeps following it.
    pages2, retired2 = run(TOWNS, VENUES, pages, retired)
    check("a rebuild from the build's own output is a no-op", (pages2, retired2) == (pages, retired))
    # Liberty's cinemas close and two open in Lansing, 5 km from Leavenworth.
    lansing = TOWNS + [town("Lansing", "Kansas", 25, 5, 11_000)]
    reopened = ([v for v in VENUES if v[0] != "Liberty, Missouri"]
                + [("Lansing, Kansas", "la1"), ("Lansing, Kansas", "la2")])
    pages3, retired3 = run(lansing, reopened, pages, retired)
    check("a retired slug follows its town to the page holding it now",
          pages3["lansing"]["towns"] == ["Lansing, Kansas", "Leavenworth, Kansas"]
          and retired3["leavenworth"]["page"] == "lansing")
    alone = [v for v in VENUES if v[0] != "Liberty, Missouri"]
    pages4, retired4 = run(TOWNS, alone, pages, retired)
    check("a retired slug's town that is a page again takes its slug back",
          pages4["leavenworth"]["towns"] == ["Leavenworth, Kansas"] and "leavenworth" not in retired4)

    closed = dict(previous, atchison=metro(["at1"], 60, 0))   # its one cinema since retired
    _, retired = run(TOWNS, VENUES, closed)
    check("a page none of whose venues is left goes to the page nearest it",
          retired["atchison"]["page"] == "liberty")

    chain = {"old-metro": {"page": "central-oregon", "qualifiedSlug": "old-metro"}}
    _, retired = run(TOWNS, VENUES, previous, chain)
    check("a slug retired earlier onto a page that retired since lands on that page's heir",
          retired["old-metro"]["page"] == "bend")

    far = [town("Perry", "Iowa", 0, 0, 7_000)]
    check("a town placed > 60 km from every venue it holds is refused",
          refuses(lambda: b.build([("pe1", point(0, 80))], {"pe1": "Perry, Iowa"}, dict(far), {}, {})))
    check("a venue in a town towns.json lacks is refused",
          refuses(lambda: b.build([("n1", point(0, 0))], {"n1": "Nowhere, Iowa"}, {}, {}, {})))


def coordinates():
    def venue(slug, state, lat, lon):
        return {"slug": slug, "state": state, "lat": str(lat), "lon": str(lon)}
    iowa = [venue("flix-brewhouse-des-moines", "Iowa", 41.58684, -93.61510),
            venue("cinemark-ankeny", "Iowa", 41.72938, -93.60577),
            venue("grand-theatre-perry", "Iowa", 41.84101, -94.10478)]
    check("a clean state passes the coordinate check", not refuses(lambda: b.check_coordinates(iowa)))
    flipped = iowa[:2] + [venue("grand-theatre-perry", "Iowa", 41.84101, 94.10478)]
    check("a flipped longitude sign is refused", refuses(lambda: b.check_coordinates(flipped)))


def committed():
    data = json.loads((b.DATA / "pages.json").read_text())
    pages = {p["slug"]: p for p in data["pages"]}
    venue_town, towns = b.load_towns()
    roster = b.roster_venues()
    at = {v["slug"]: (float(v["lat"]), float(v["lon"])) for v in roster}
    count = {}
    for v in roster:
        count[venue_town[v["slug"]]] = count.get(venue_town[v["slug"]], 0) + 1
    venues = [c for p in data["pages"] for c in p["cinemas"]]
    check("pages.json: every rostered venue on exactly one page",
          len(venues) == len(set(venues)) and set(venues) == set(at))
    check("pages.json: no slug is both a page and retired", not set(pages) & set(data["retired"]))
    check("pages.json: every retired slug lands on a page", all(r["page"] in pages for r in data["retired"].values()))
    check("pages.json: only 1- and 2-venue towns cluster",
          all(count[t] <= 2 for p in pages.values() if p["kind"] == "cluster" for t in p["towns"]))
    check("pages.json: a major or town page is one town",
          all(len(p["towns"]) == 1 for p in pages.values() if p["kind"] in ("major", "town")))
    reach = sorted(b.km((p["lat"], p["lon"]), at[c]) for p in pages.values() for c in p["cinemas"])
    check(f"pages.json: no cinema is more than 80 km from the town its page is named after (furthest {reach[-1]:.0f})",
          reach[-1] <= 80)
    check(f"pages.json: 95% of cinemas are within 30 km of it ({reach[int(.95 * len(reach))]:.0f})",
          reach[int(.95 * len(reach))] <= 30)
    check("pages.json: New York keeps only its own boroughs' venues, Brooklyn's and Queens' among them",
          pages["new-york"]["towns"] == ["New York City, New York"]
          and {"alamo-drafthouse-brooklyn", "amc-bay-terrace-6"} <= set(pages["new-york"]["cinemas"]))
    # The Twin Cities' Wisconsin side was a metro of its own (/minneapolis-st-paul-wisconsin/);
    # Hudson's cinema is on the Woodbury page with Minnesota's now.
    woodbury = pages["woodbury"]
    check("pages.json: a metro split at a state line is one page",
          {"Hudson, Wisconsin", "Woodbury, Minnesota"} <= set(woodbury["towns"]) and woodbury["state"] == "Minnesota")
    check("pages.json: the old split halves redirect to the page holding their namesake town",
          data["retired"]["minneapolis-st-paul-wisconsin"]["page"] == "minneapolis"
          and data["retired"]["cincinnati-kentucky"]["page"] == "cincinnati")
    check("pages.json: /springfield/ is still Springfield, Illinois",
          pages["springfield"]["anchor"] == "Springfield, Illinois")


def main():
    synthetic()
    coordinates()
    committed()
    print("all passed")


if __name__ == "__main__":
    main()
