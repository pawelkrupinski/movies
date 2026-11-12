#!/usr/bin/env python3
"""Generate the tables under geodata/src/main/resources/generated/es-roster/ from pages.json.

The JSON -> Scala step of the Spanish roster pipeline, modelled on
data/us/scripts/generate_roster.py. Reads the PAGES (`pages.json`, built by
`build_pages.py`: which town or cluster page each venue is on, and where each page
that stopped existing now redirects), the harvested venues (`provinces.json`) and
the Ocine chain's own ticketing servers (`ocine.json`), and writes the tab-separated
tables `models.SpanishRosterData` reads (through `models.GeneratedTable`) and
`models.SpanishRoster` materialises into City/Cinema objects: `pages.tsv`, `cinemas.tsv`
(keyed by page slug, in page order), `retired.tsv` and `seasonal.tsv`.

`ocine.json` does two things to the harvest: it names the ticketing server of
each Ocine venue SensaCine lists (`listed`, by theaterId), and it ADDS the
chain's venues SensaCine does not list at all (`unlisted`), which have no
theaterId and are scraped off their own server only.

Things it refuses to do, because each fails SILENTLY downstream:

  * emit a duplicate `displayName` — that string is the wire key every stored
    showtime is filed under, and `Source.byDisplayName` is a plain `toMap`, so
    two venues sharing one silently become one venue;
  * emit a venue on no page, or on two, or a page listing a venue the roster does
    not have — a venue on no page is never scraped, and nothing else says so;
  * merge an `ocine.json` row that no longer lines up with the harvest — a
    listed theaterId the harvest dropped, an unlisted venue in a province it
    does not know, or one ticketing server named for two venues — since a stale
    row quietly loses a venue's own-server scrape, or the venue itself.

Usage:  python3 data/spain/scripts/generate_roster.py
"""
import collections
import json
import pathlib
import sys

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parents[2] / "scripts"))
import retired_venues  # noqa: E402
import seasonal_venues  # noqa: E402
from generated_tables import list_cell, option_cell, write_table  # noqa: E402
from town_pages import placement_problems  # noqa: E402

ROOT = pathlib.Path(__file__).resolve().parents[3]
DATA = ROOT / "data" / "spain"
OCINE = DATA / "ocine.json"
PAGES = DATA / "pages.json"
OUT = ROOT / "geodata" / "src" / "main" / "resources" / "generated" / "es-roster"


def merge_ocine(provinces: list, ocine: dict) -> list[str]:
    """Fold `ocine.json` into the harvested provinces, in place: each listed
    venue gains its `ocineServer`, each unlisted one joins its province with no
    theaterId, and a province that gained a venue is re-sorted by displayName,
    the order the harvest keeps. Returns the problems found — empty means the
    table and the harvest agree."""
    problems = []
    by_theater = {c["theaterId"]: c for p in provinces for c in p["cinemas"]}
    by_province = {p["name"]: p for p in provinces}
    servers = collections.Counter(
        list(ocine["listed"].values()) + [v["ticketingServer"] for v in ocine["unlisted"]])
    problems += [f"ticketing server {server!r} is named for {n} venues"
                 for server, n in sorted(servers.items()) if n > 1]
    for theater_id, server in sorted(ocine["listed"].items()):
        if theater_id in by_theater:
            by_theater[theater_id]["ocineServer"] = server
        else:
            problems.append(f"listed Ocine venue {theater_id} ({server}) is not in the harvest")
    grown = set()
    for venue in ocine["unlisted"]:
        province = by_province.get(venue["province"])
        if province is None:
            problems.append(f"unlisted Ocine venue {venue['name']!r} names unknown "
                            f"province {venue['province']!r}")
            continue
        province["cinemas"].append({"theaterId": None, "name": venue["name"], "town": venue["town"],
                                    "displayName": venue["name"], "ocineServer": venue["ticketingServer"]})
        grown.add(province["name"])
    for name in grown:
        by_province[name]["cinemas"].sort(key=lambda c: c["displayName"])
    return problems


def main() -> int:
    provinces = json.loads((DATA / "provinces.json").read_text())
    retired = retired_venues.load(DATA)
    for province in provinces:
        province["cinemas"] = [c for c in province["cinemas"] if c.get("theaterId") not in retired]
    problems = merge_ocine(provinces, json.loads(OCINE.read_text()))
    if problems:
        for problem in problems:
            print(f"ERROR: {problem}", file=sys.stderr)
        print("Fix data/spain/ocine.json.", file=sys.stderr)
        return 1

    venues: dict[str, dict] = {}
    for province in provinces:
        for cinema in province["cinemas"]:
            name = cinema["displayName"]
            if name in venues:
                print(f"ERROR: duplicate displayName {name!r}", file=sys.stderr)
                return 1
            venues[name] = cinema

    data = json.loads(PAGES.read_text())
    pages, retired_pages = data["pages"], data["retired"]
    problems = placement_problems(venues, pages)
    if problems:
        for problem in problems:
            print(f"ERROR: {problem}", file=sys.stderr)
        return 1

    page_rows, cinema_rows = [], []
    for page in pages:
        page_rows.append([
            page["slug"], page["qualifiedSlug"], page["name"], page["province"],
            repr(float(page["lat"])), repr(float(page["lon"])), page["zoneId"],
            "true" if page["kind"] == "cluster" else "false", list_cell(page["towns"])])
        for name in page["cinemas"]:
            cinema_rows.append([page["slug"], name, option_cell(venues[name]["theaterId"]),
                                option_cell(venues[name].get("ocineServer"))])
    write_table(OUT / "pages.tsv", page_rows)
    write_table(OUT / "cinemas.tsv", cinema_rows)
    write_table(OUT / "retired.tsv", [[slug, r["qualifiedSlug"], r["page"]]
                                      for slug, r in sorted(retired_pages.items())])
    write_table(OUT / "seasonal.tsv", [[name] for name in seasonal_venues.seasonal_names("es", venues)])
    print(f"Wrote {OUT.relative_to(ROOT)}: {len(pages)} pages / {len(venues)} cinemas / "
          f"{len(retired_pages)} retired slugs")
    return 0


if __name__ == "__main__":
    sys.exit(main())
