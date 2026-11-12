#!/usr/bin/env python3
"""Generate geodata/src/main/resources/generated/de-roster/ (read by `models.GermanRosterData`) from pages.json.

The JSON -> Scala step of the German roster pipeline, the counterpart of
data/spain/scripts/generate_roster.py. Reads the PAGES (`pages.json`, built by
`build_pages.py`: which town or cluster page each venue is on, and where each page
that stopped existing now redirects), the venues (`venues.json` less
`retired.json`) and each venue's kinoprogramm.com fallback (`kinoprogramm.json`),
and emits the tab-separated tables `models.GermanRoster` materialises into City/Cinema
objects: pages.tsv, cinemas.tsv (by page slug, in page order), retired.tsv, seasonal.tsv.

Things it refuses to do, because each fails SILENTLY downstream:

  * emit a duplicate `displayName` — that string is the wire key every stored
    showtime is filed under, and `Source.byDisplayName` is a plain `toMap`, so
    two venues sharing one quietly become one venue;
  * emit a venue on no page, or on two, or a page listing a venue the roster does
    not have — a venue on no page is never scraped, and nothing else says so.

Usage:  python3 data/germany/scripts/generate_roster.py
"""
import json
import pathlib
import re
import sys

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent))
import build_pages  # noqa: E402
import seasonal_venues  # noqa: E402
from town_pages import placement_problems  # noqa: E402

ROOT = build_pages.ROOT
DATA = build_pages.DATA
OUT = ROOT / "geodata" / "src" / "main" / "resources" / "generated" / "de-roster"
sys.path.insert(0, str(ROOT / "data" / "scripts"))
from generated_tables import list_cell, option_cell, write_table  # noqa: E402


def roster_problems(roster: list) -> list[str]:
    seen, problems = set(), []
    for venue in roster:
        if venue["displayName"] in seen:
            problems.append(f"duplicate displayName {venue['displayName']!r}")
        seen.add(venue["displayName"])
    return problems


def render(roster: list, pages: list, retired: dict, kinoprogramm: dict, town_of) -> dict:
    """The tables, file name -> rows: pages (one a page, in order), cinemas (one a venue, keyed by
    its page's slug, in page order), the retired slugs and the seasonal venues' names."""
    venues = {v["displayName"]: v for v in roster}
    page_rows = [
        [page["slug"], page["qualifiedSlug"], page["name"], page["bundesland"], repr(page["lat"]), repr(page["lon"]),
         "true" if page["kind"] == "cluster" else "false", list_cell(page["towns"])]
        for page in pages]
    cinema_rows = [
        [page["slug"], name, name, build_pages.venue_key(venues[name]),
         option_cell(kinoprogramm.get(venues[name].get("theaterId"))), town_of(venues[name]["city"])]
        for page in pages for name in page["cinemas"]]
    return {
        "pages.tsv": page_rows,
        "cinemas.tsv": cinema_rows,
        "retired.tsv": [[slug, r["qualifiedSlug"], r["page"]] for slug, r in sorted(retired.items())],
        "seasonal.tsv": [[name] for name in seasonal_venues.seasonal_names("de", venues)],
    }


def main() -> int:
    roster = build_pages.roster_venues()
    data = json.loads(build_pages.OUT.read_text())
    problems = roster_problems(roster) + placement_problems({v["displayName"] for v in roster}, data["pages"])
    if problems:
        for problem in problems:
            print(f"ERROR: {problem}", file=sys.stderr)
        return 1
    # Each venue's kinoprogramm.com page (harvest_kinoprogramm.py) — its showtime
    # FALLBACK behind Filmstarts. A venue the harvest could not match confidently
    # simply has none.
    kinoprogramm = json.loads((DATA / "kinoprogramm.json").read_text())
    town_of, _ = build_pages.load_towns(build_pages.load_coords())
    for name, rows in render(roster, data["pages"], data["retired"], kinoprogramm, town_of).items():
        write_table(OUT / name, rows)
    covered = sum(1 for v in roster if v.get("theaterId") in kinoprogramm)
    print(f"Wrote {OUT.relative_to(ROOT)}: {len(data['pages'])} pages / {len(roster):,} cinemas, "
          f"{covered:,} with a kinoprogramm.com fallback / {len(data['retired'])} retired slugs")
    return 0


if __name__ == "__main__":
    sys.exit(main())
