#!/usr/bin/env python3
"""Tests for the UK generate_roster.py — roster.json into UkRosterData.scala and
UkSourcesData.scala, pages.json into UkPages.scala and the apps' fallback lists —
on synthetic data, and the committed output against it.

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
    src = g.scala_source(PAGES, RETIRED, TOWN_NAMES)
    check("London is its own row, not one of the generated pages",
          '  val london: Row = Row("london"' in src and src.count('Row("london"') == 1)
    check("a cluster is a multi-town row naming its towns by their display names",
          'Row("ilkley", "Ilkley", 51.5, -0.1, "England", "West Yorkshire", multiTown = true, '
          'Seq("Ilkley", "Skipton"), Seq(IlkleyCinema, PlazaSkipton))' in src)
    check("a retired slug maps to its page", '"west-yorkshire" -> "leeds",' in src)
    check("a retired name aliases its page — not the page's own name, nor its county's heading",
          '"leeds" -> Seq("Yorkshire"),' in src and '"lerwick" -> Seq("Highlands and Islands"),' in src)
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
        g.scala_source([dict(PAGES[0], nation="Atlantis")], {}, TOWN_NAMES)
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
    venues = g.venues_source(ROSTER)
    check("a venue is a top-level val named as its object, keeping its names and its Scala name",
          'val CineworldLeeds: UkCinema = new UkCinema("Cineworld Leeds", "Cineworld Leeds", "CineworldLeeds")'
          in venues)
    check("a seasonal venue opens with the UK's season",
          'new UkCinema("BFI London Southbank", "BFI London Southbank", "BfiLondonSouthbank", '
          'Opening.Seasonal(SeasonWindow.UnitedKingdom))' in venues)
    sources = g.sources_source(ROSTER)
    check("a Flicks venue reads its slug as its primary", '    f(IlkleyCinema, "ilkley-cinema"),' in sources)
    check("an own-site venue keeps its Flicks slug as the fallback",
          '    o(CineworldLeeds, UkFeed.Cineworld("x01-leeds"), "cineworld-leeds"),' in sources
          and '    o(HydeParkPictureHouse, UkFeed.Spektrix("leedsheritagetheatres", Some("Hyde Park Picture House")), '
              '"hyde-park-picture-house"),' in sources)
    check("a venue on no Flicks page has no fallback", "    own(TheOldCourtWindsor, UkFeed.TheOldCourt)," in sources)
    check("the venues keep the roster's order, group by group",
          sources.index("HydeParkPictureHouse") < sources.index("IlkleyCinema") < sources.index("BfiLondonSouthbank")
          and "group0 ++ group1" in sources)
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


def committed():
    data = json.loads(g.PAGES.read_text(encoding="utf-8"))
    towns = {k: t["name"] for k, t in json.loads(g.TOWNS.read_text(encoding="utf-8"))["towns"].items()}
    check("UkPages.scala is what the generator makes of pages.json",
          g.OUT.read_text(encoding="utf-8") == g.scala_source(data["pages"], data["retired"], towns))
    groups = g.uk_roster.load()
    check("roster.json is sound, and on the pages", g.roster_problems(groups, data["pages"]) == [])
    check("roster.json is in its canonical layout", g.uk_roster.ROSTER.read_text(encoding="utf-8") == g.uk_roster.dumps(groups))
    check("UkRosterData.scala is what the generator makes of roster.json",
          g.VENUES_OUT.read_text(encoding="utf-8") == g.venues_source(groups))
    check("UkSourcesData.scala is what the generator makes of roster.json",
          g.SOURCES_OUT.read_text(encoding="utf-8") == g.sources_source(groups))


def main():
    synthetic()
    roster()
    committed()
    print("all passed")


if __name__ == "__main__":
    main()
