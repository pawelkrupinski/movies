#!/usr/bin/env python3
"""Tests for the UK generate_roster.py — roster.json into the uk-roster and uk-sources
tables, pages.json into the uk-pages tables and the apps' fallback lists — on
synthetic data, and the committed output against it.

    python3 data/uk/scripts/generate_roster_test.py
"""
import json
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent))
import generate_roster as g  # noqa: E402


def check(name, cond):
    print(("PASS " if cond else "FAIL ") + name)
    if not cond:
        sys.exit(1)


def page(slug, name, kind, county, cinemas, towns, nation="England"):
    return {"slug": slug, "name": name, "kind": kind, "nation": nation, "county": county, "lat": 51.5, "lon": -0.1,
            "towns": towns, "cinemas": cinemas}


PAGES = [
    page("london", "London", "major", "Greater London", ["BfiLondonSouthbank"], ["London, Greater London"]),
    page("leeds", "Leeds", "major", "West Yorkshire", ["HydeParkPictureHouse"], ["Leeds, West Yorkshire"]),
    page("ilkley", "Ilkley", "cluster", "West Yorkshire", ["IlkleyCinema", "PlazaSkipton"],
         ["Ilkley, West Yorkshire", "Skipton, North Yorkshire"]),
    page("lerwick", "Lerwick", "town", "Shetland", ["MareelLerwick"], ["Lerwick, Shetland"], nation="Scotland"),
]
TOWN_NAMES = {"London, Greater London": "London", "Leeds, West Yorkshire": "Leeds",
              "Ilkley, West Yorkshire": "Ilkley", "Skipton, North Yorkshire": "Skipton",
              "Lerwick, Shetland": "Lerwick"}
RETIRED = {
    "west-yorkshire": {"page": "leeds", "name": "West Yorkshire"},
    "highlands-and-islands": {"page": "lerwick", "name": "Highlands and Islands"},
    "yorkshire": {"page": "leeds", "name": "Yorkshire"},
    "leeds-old": {"page": "leeds", "name": "Leeds"},
}


def synthetic():
    tables = g.pages_tables(PAGES, RETIRED, TOWN_NAMES)
    pages = tables["pages.tsv"]
    check("London is the first page, then pages.json's order", [r[0] for r in pages] == ["london", "leeds", "ilkley", "lerwick"])
    check("a cluster is a multi-town row naming its towns by their display names and its venues by key",
          pages[2] == ["ilkley", "Ilkley", "51.5", "-0.1", "England", "West Yorkshire", "true", "Ilkley|Skipton",
                       "IlkleyCinema|PlazaSkipton"])
    check("a coordinate is written so it reads back exactly", g.page_row(dict(PAGES[0], lat=0.1 + 0.2), TOWN_NAMES)[2]
          == repr(0.1 + 0.2) and float(repr(0.1 + 0.2)) == 0.1 + 0.2)
    check("the nations are in the pickers' order", tables["nations.tsv"] == [[n] for n in g.NATIONS])
    check("a retired slug maps to its page", ["west-yorkshire", "leeds"] in tables["retired.tsv"])
    check("a retired name aliases its page — not the page's own name, nor its county's heading",
          tables["former-names.tsv"] == [["leeds", "Yorkshire"], ["lerwick", "Highlands and Islands"]])
    rows = {r["slug"]: r for r in g.mobile_rows(PAGES)}
    check("the apps' rows keep pages.json's order, London first", list(rows) == [p["slug"] for p in PAGES])
    check("a cluster's app label is '<town> area'", rows["ilkley"]["label"] == "Ilkley area")
    check("a county of several pages is the subregion; a county of one is none",
          rows["leeds"]["subregion"] == "West Yorkshire" and rows["lerwick"]["subregion"] is None)
    text = "before\n" + g.MOBILE_START + "old\n" + g.MOBILE_END + "after\n"
    spliced = g.splice(text, list(rows.values()), g.mobile_row_kt)
    check("the apps' block is replaced between its markers and nowhere else",
          spliced.startswith("before\n" + g.MOBILE_START) and spliced.endswith(g.MOBILE_END + "after\n")
          and "old\n" not in spliced
          and '        City("lerwick", "Lerwick", 51.5000, -0.1000, "uk", "Scotland"),' in spliced)
    try:
        g.pages_tables([dict(PAGES[0], nation="Atlantis")], {}, TOWN_NAMES)
        refused = False
    except SystemExit:
        refused = True
    check("a nation with no place in the pickers' order is refused", refused)


def v(obj, display, slug, primary=None, **kw):
    row = {"object": obj, "displayName": display, "pillName": display, "flicks": slug, **kw}
    return dict(row, primary=primary) if primary else row


ROSTER = [
    {"group": "leeds", "venues": [
        v("HydeParkPictureHouse", "Hyde Park Picture House", "hyde-park-picture-house",
          ["spektrix", "leedsheritagetheatres", "Hyde Park Picture House"]),
        v("CineworldLeeds", "Cineworld Leeds", "cineworld-leeds", ["cineworld", "x01-leeds"]),
        v("IlkleyCinema", "Ilkley Cinema", "ilkley-cinema"),
    ]},
    {"group": "london", "venues": [
        v("BfiLondonSouthbank", "BFI London Southbank", "bfi-southbank", ["bfiSouthbank"], seasonal=True),
        v("TheOldCourtWindsor", "The Old Court Windsor", None, ["theOldCourt"], note="on no Flicks page"),
    ]},
]


def roster():
    venues = g.venue_rows(ROSTER)
    check("a venue is keyed on its object, keeping its names, and opens all year",
          ["CineworldLeeds", "Cineworld Leeds", "Cineworld Leeds", ""] in venues)
    check("a seasonal venue opens with the UK's season",
          ["BfiLondonSouthbank", "BFI London Southbank", "BFI London Southbank", "seasonal"] in venues)
    sources = g.source_rows(ROSTER)
    check("a Flicks venue reads its slug as its primary", ["IlkleyCinema", "flicks", "ilkley-cinema", ""] in sources)
    check("an own-site venue keeps its Flicks slug as the fallback",
          ["CineworldLeeds", "cineworld", "x01-leeds", "cineworld-leeds"] in sources
          and ["HydeParkPictureHouse", "spektrix", "leedsheritagetheatres|Hyde Park Picture House",
               "hyde-park-picture-house"] in sources)
    check("a venue on no Flicks page has no fallback", ["TheOldCourtWindsor", "theOldCourt", "", ""] in sources)
    check("the venues keep the roster's order, group by group",
          [r[0] for r in sources] == ["HydeParkPictureHouse", "CineworldLeeds", "IlkleyCinema", "BfiLondonSouthbank",
                                      "TheOldCourtWindsor"])
    pages = [page("leeds", "Leeds", "major", "West Yorkshire",
                  ["HydeParkPictureHouse", "CineworldLeeds", "IlkleyCinema", "BfiLondonSouthbank", "TheOldCourtWindsor"],
                  ["Leeds, West Yorkshire"])]
    check("a sound roster on its pages has no problems", g.roster_problems(ROSTER, pages) == [])

    def refused(change, needle, pages=pages):
        bad = json.loads(json.dumps(ROSTER))
        change(bad)
        found = g.roster_problems(bad, pages)
        return any(needle in f for f in found)

    check("two venues under one display name are refused — they would read each other's slots",
          refused(lambda r: r[0]["venues"][2].update(displayName="Cineworld Leeds"), "displayName"))
    check("a Flicks venue with no slug is refused",
          refused(lambda r: r[0]["venues"][2].update(flicks=None), "no Flicks slug"))
    check("an unknown source is refused", refused(lambda r: r[0]["venues"][1].update(primary=["regal", "1"]), "unknown"))
    check("a source missing its handle is refused", refused(lambda r: r[0]["venues"][1].update(primary=["odeon"]), "takes"))
    check("Derby QUAD's client on another venue is refused",
          refused(lambda r: r[0]["venues"][2].update(primary=["quadDerby"]), "reads QuadDerby"))
    check("a venue on no page is refused", refused(lambda r: None, "on no page", pages=[dict(pages[0], cinemas=[])]))
    check("roster.json is written one venue per line, and reads back the same",
          g.uk_roster.dumps(ROSTER).count("\n") == 2 + 2 * 2 + 5 + 2
          and json.loads(g.uk_roster.dumps(ROSTER))["groups"] == ROSTER)


def table(path):
    return [line.split("\t") for line in path.read_text(encoding="utf-8").splitlines()]


def committed():
    data = json.loads(g.PAGES.read_text(encoding="utf-8"))
    towns = {k: t["name"] for k, t in json.loads(g.TOWNS.read_text(encoding="utf-8"))["towns"].items()}
    for name, rows in g.pages_tables(data["pages"], data["retired"], towns).items():
        check(f"uk-pages/{name} is what the generator makes of pages.json", table(g.PAGES_OUT / name) == rows)
    groups = g.uk_roster.load()
    check("roster.json is sound, and on the pages", g.roster_problems(groups, data["pages"]) == [])
    check("roster.json is in its canonical layout", g.uk_roster.ROSTER.read_text(encoding="utf-8") == g.uk_roster.dumps(groups))
    check("uk-roster/venues.tsv is what the generator makes of roster.json", table(g.VENUES_OUT) == g.venue_rows(groups))
    check("uk-sources/sources.tsv is what the generator makes of roster.json", table(g.SOURCES_OUT) == g.source_rows(groups))


def main():
    synthetic()
    roster()
    committed()
    print("all passed")


if __name__ == "__main__":
    main()
