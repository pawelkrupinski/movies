#!/usr/bin/env python3
"""Tests for the UK build_pages.py on a synthetic map, and for the committed
pages.json. The clustering rule itself is Poland's, Spain's, Germany's and the
US's too and is tested in data/pl/scripts/test_build_pages.py; these cover what
the UK adds — the 40 km cap on how far a page reaches, no cluster across water,
two towns of one name, slugs another country holds, and where a retired county
page's URL goes.

    python3 data/uk/scripts/test_build_pages.py
"""
import collections
import json
import math
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent))
import build_pages as b  # noqa: E402


def point(km_north, km_east):
    lat = 53.0 + km_north / 111.2
    return lat, -2.0 + km_east / (111.2 * math.cos(math.radians(lat)))


def town(name, county, km_north, km_east, pop, landmass="Great Britain"):
    lat, lon = point(km_north, km_east)
    return f"{name}, {county}", {"name": name, "nation": "England", "county": county, "landmass": landmass,
                                 "lat": lat, "lon": lon, "pop": pop}


def run(towns, venues, previous=None, prior_retired=None, foreign=frozenset(), rules=b.RULES):
    """towns: [town(...)]; venues: (town key, venue, offset) in roster order, the
    venue `offset` (km north, km east) from its town's centre."""
    towns = dict(towns)
    at = []
    for t, v, (n, e) in venues:
        lat, lon = towns[t]["lat"], towns[t]["lon"]
        at.append((v, (lat + n / 111.2, lon + e / (111.2 * math.cos(math.radians(lat))))))
    pages, retired = b.build(at, {v: t for t, v, _ in venues}, towns, previous or {}, prior_retired or {},
                             frozenset(foreign), rules)
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


def region(cinemas, km_north, km_east, anchor=None):
    lat, lon = point(km_north, km_east)
    return {"cinemas": cinemas, "lat": lat, "lon": lon, "anchor": anchor, "qualifiedSlug": None}


HERE = (0, 0)
TOWNS = [
    town("Leeds", "West Yorkshire", 0, 0, 536_000),
    town("Bradford", "West Yorkshire", 0, -14, 366_000),   # a major 14 km away keeps its own page
    town("Otley", "West Yorkshire", 15, -10, 14_000),      # 2 venues
    town("Ilkley", "West Yorkshire", 20, -20, 5_000),      # 1 venue, 11.7 km from Otley: past 10 km
    town("Skipton", "North Yorkshire", 30, -35, 14_000),   # 1 venue
    town("Settle", "North Yorkshire", 45, -60, 2_500),     # 1 venue, 29 km from Skipton
    # Its town centre 34 km from Skipton's, inside the 35 km absorb — but its one
    # venue sits 8 km further out, 42 km from Skipton: past the 40 km cap.
    town("Grassington", "North Yorkshire", 64, -35, 1_200),
    town("Newport", "Isle of Wight", -400, 0, 25_000, landmass="Isle of Wight"),
    town("Ryde", "Isle of Wight", -395, 10, 24_000, landmass="Isle of Wight"),
    town("Southsea", "Hampshire", -388, 12, 20_000),        # 5 km from Ryde, across the water
    town("Newport", "Gwent", -200, -100, 145_000),          # the bigger Newport
    town("Cambridge", "Cambridgeshire", -150, 150, 145_000),
]
VENUES = [
    ("Leeds, West Yorkshire", "le1", HERE), ("Bradford, West Yorkshire", "br1", HERE),
    ("Otley, West Yorkshire", "ot1", HERE), ("Otley, West Yorkshire", "ot2", HERE),
    ("Ilkley, West Yorkshire", "il1", HERE), ("Skipton, North Yorkshire", "sk1", HERE),
    ("Settle, North Yorkshire", "se1", HERE), ("Grassington, North Yorkshire", "gr1", (8, 0)),
    ("Newport, Isle of Wight", "nw1", HERE), ("Ryde, Isle of Wight", "ry1", HERE),
    ("Southsea, Hampshire", "so1", HERE), ("Newport, Gwent", "ng1", HERE),
    ("Cambridge, Cambridgeshire", "ca1", HERE),
]


def synthetic():
    previous = {
        "west-yorkshire": region(["le1", "br1", "ot1", "ot2", "il1"], 0, -5),
        "north-yorkshire": region(["sk1", "se1", "gr1"], 40, -40),
        "isle-of-wight": region(["nw1", "ry1"], -398, 5),
        "leeds": region(["le1"], 0, 0, "Leeds, West Yorkshire"),
    }
    pages, retired = run(TOWNS, VENUES, previous, foreign={"cambridge"})

    check("a place of 100,000+ is a major city with only its own venues",
          pages["leeds"]["kind"] == "major" and pages["leeds"]["cinemas"] == ["le1"]
          and pages["bradford"]["cinemas"] == ["br1"])
    check("a 2-venue town takes only the towns within 10 km (Poland's radius, not the US's 20)",
          pages["otley"]["towns"] == ["Otley, West Yorkshire"])
    check("1-venue towns cluster within 25 km", {"Ilkley, West Yorkshire", "Skipton, North Yorkshire"}
          <= set(next(p for p in pages.values() if "il1" in p["cinemas"])["towns"]))
    skipton = next(p for p in pages.values() if "sk1" in p["cinemas"])
    check("a venue more than 40 km from a page's anchor is not on that page, though its town is within the "
          "35 km absorb", "gr1" not in skipton["cinemas"])
    check("…and, with nowhere within reach, it stands as a page of its own",
          pages["grassington"]["cinemas"] == ["gr1"] and pages["grassington"]["kind"] == "town")
    uncapped, _ = run(TOWNS, VENUES, previous, foreign={"cambridge"},
                      rules=b.town_pages.Rules(max_reach_km=None))
    check("without the cap the same venue would be on the cluster (the case the cap exists for)",
          any("gr1" in p["cinemas"] and len(p["towns"]) > 1 for p in uncapped.values()))
    check("no cluster crosses water: Ryde clusters with Newport, not Southsea 5 km across the Solent",
          next(p for p in pages.values() if "ry1" in p["cinemas"])["cinemas"] == ["nw1", "ry1"]
          and next(p for p in pages.values() if "so1" in p["cinemas"])["cinemas"] == ["so1"])
    check("of two towns of one name, the more populous has the bare slug, the other its county's",
          pages["newport"]["anchor"] == "Newport, Gwent" and "newport-isle-of-wight" in pages)
    check("a slug another country holds is qualified with the county",
          "cambridge" not in pages and pages["cambridge-cambridgeshire"]["cinemas"] == ["ca1"])
    check("a page carries its nation and its county",
          pages["skipton"]["county"] == "North Yorkshire" and pages["skipton"]["nation"] == "England")
    check("a page still named after its town is not retired", "leeds" not in retired)
    check("a county page goes to the page holding most of its venues",
          retired["west-yorkshire"]["page"] == "otley" and retired["isle-of-wight"]["page"] == "newport-isle-of-wight")
    check("every venue is on exactly one page",
          sorted(c for p in pages.values() for c in p["cinemas"]) == sorted(v for _, v, _ in VENUES))

    pages2, retired2 = run(TOWNS, VENUES, pages, retired, foreign={"cambridge"})
    check("a rebuild from the build's own output is a no-op", (pages2, retired2) == (pages, retired))
    pages3, _ = run(TOWNS, VENUES, pages, retired, foreign={"cambridge", "newport", "leeds"})
    check("a slug the UK held before stays the UK's when another country later wants it",
          "leeds" in pages3 and "newport" in pages3)


def committed():
    data = json.loads((b.DATA / "pages.json").read_text())
    pages = {p["slug"]: p for p in data["pages"]}
    venue_town, towns = b.load_towns()
    at = {v["cinemaObject"]: (float(v["lat"]), float(v["lon"])) for v in b.roster_venues()}
    count = collections.Counter(venue_town.values())
    venues = [c for p in data["pages"] for c in p["cinemas"]]
    check("pages.json: every venue on exactly one page", len(venues) == len(set(venues)) and set(venues) == set(at))
    check("pages.json: no slug is both a page and retired", not set(pages) & set(data["retired"]))
    check("pages.json: every retired slug lands on a page", all(r["page"] in pages for r in data["retired"].values()))
    check("pages.json: only 1- and 2-venue towns cluster",
          all(count[t] <= 2 for p in pages.values() if p["kind"] == "cluster" for t in p["towns"]))
    reach = sorted(b.km((p["lat"], p["lon"]), at[c]) for p in pages.values() for c in p["cinemas"])
    check(f"pages.json: no venue is more than 40 km from the town its page is named after (furthest {reach[-1]:.0f})",
          reach[-1] <= 40)
    check("pages.json: no page spans two landmasses",
          all(len({towns[t]["landmass"] for t in p["towns"]}) == 1 for p in pages.values()))
    check("pages.json: Greater London is one page, Croydon's and Romford's cinemas on it",
          pages["london"]["towns"] == ["London, Greater London"]
          and {"DavidLeanCinemaCroydon", "VueCinemasRomford"} <= set(pages["london"]["cinemas"]))
    check("pages.json: the two Bangors and the two Newports are four pages",
          len({p["slug"] for p in pages.values() if p["name"] in ("Bangor", "Newport")}) == 4)
    missing = sorted(set(HAND_FILED) - set(pages) - set(data["retired"]))
    check(f"pages.json: every one of the 76 hand-filed regions and the 3 merged pages still answers {missing}",
          not missing)
    check("pages.json: a retired region goes to the page holding its namesake town",
          data["retired"]["antrim"]["page"] == next(p["slug"] for p in pages.values()
                                                     if "Antrim, Antrim and Newtownabbey" in p["towns"]))


#: The UK's slugs before it was rebuilt from towns (2026-10): the 76 Flicks
#: regions it was filed under, and the three merged into a neighbour.
HAND_FILED = """
aberdeenshire antrim armagh ayrshire-and-arran bedfordshire belfast berkshire birmingham bristol buckinghamshire
cambridgeshire cardiff central-scotland cheshire clwyd cornwall county-durham cumbria derbyshire devon dorset down
dudley dumfries-and-galloway dunbartonshire-argyll-bute dyfed east-sussex east-yorkshire edinburgh-and-lothians essex
fermanagh fife glamorgan glasgow gloucestershire guernsey gwent gwynedd hampshire herefordshire hertfordshire
highlands-and-islands isle-of-man isle-of-wight jersey kent lanarkshire lancashire leicestershire lincolnshire
liverpool london londonderry manchester north-yorkshire northamptonshire northumberland norwich nottinghamshire
oxfordshire powys renfrewshire roxburgh-ettrick-and-lauderdale sandwell shropshire somerset south-yorkshire
staffordshire suffolk surrey tayside tyne-and-wear tyrone warwickshire west-sussex west-yorkshire wiltshire
worcestershire yorkshire
""".split()


def main():
    synthetic()
    committed()
    print("all passed")


if __name__ == "__main__":
    main()
