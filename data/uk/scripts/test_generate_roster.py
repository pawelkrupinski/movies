#!/usr/bin/env python3
"""Tests for the UK generate_roster.py — pages.json into UkPages.scala and the
apps' fallback lists — on synthetic pages, and the committed output against it.

    python3 data/uk/scripts/test_generate_roster.py
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


def committed():
    data = json.loads(g.PAGES.read_text(encoding="utf-8"))
    towns = {k: t["name"] for k, t in json.loads(g.TOWNS.read_text(encoding="utf-8"))["towns"].items()}
    check("UkPages.scala is what the generator makes of pages.json",
          g.OUT.read_text(encoding="utf-8") == g.scala_source(data["pages"], data["retired"], towns))


def main():
    synthetic()
    committed()
    print("all passed")


if __name__ == "__main__":
    main()
