#!/usr/bin/env python3
"""Generate geodata/src/main/resources/generated/venue-towns.tsv (read by `models.VenueTowns`) — the town each venue
in a HAND-WRITTEN roster sits in.

Germany, Spain and the US generate their rosters, so each venue's town rides
along in the generated tuple. The UK's ~840 venues and Poland's ~300 are
hand-written `case object`s in Cinema.scala with a display name and nothing
else, and there is nowhere in them for a town to live. This is that place: one
table, keyed on the display name every other part of the codebase identifies a
venue by, read by `TownRanking.ofVenues` (behind `City.extraPlaces`) for both countries.

Two sources, because the two countries knew their towns in different ways — the
UK's are harvested from Flicks' addresses, Poland's read off the annotations
already beside its venues (see each country's scripts). Both arrive here as
`{displayName, town}` and stop differing at this point.

It refuses a venue whose display name it cannot pin, refuses two venues sharing
one, and refuses a table that has collapsed — a silently half-empty table is a
page that quietly stops naming the places it covers.

Usage:  python3 data/scripts/generate_venue_towns.py
"""
from __future__ import annotations

import json
import pathlib
import sys

ROOT = pathlib.Path(__file__).resolve().parents[2]
OUT = ROOT / "geodata" / "src" / "main" / "resources" / "generated" / "venue-towns.tsv"
sys.path.insert(0, str(ROOT / "data" / "scripts"))
from generated_tables import write_table  # noqa: E402

# (country, source file, the floor its sweep must clear).
SOURCES = [
    ("the UK", ROOT / "data" / "uk" / "venues.json", 700),
    ("Poland", ROOT / "data" / "pl" / "venues.json", 100),
]

def rows_from(country: str, path: pathlib.Path, floor: int) -> list[tuple[str, str]]:
    venues = json.loads(path.read_text())
    named = [v for v in venues if v["displayName"] and v["town"]]

    unnamed = [v.get("cinemaObject") for v in venues if not v["displayName"]]
    if unnamed:
        print(f"ERROR: {len(unnamed)} {country} venues have no displayName — the case object "
              f"could not be read out of Cinema.scala: {unnamed[:10]}", file=sys.stderr)
        sys.exit(1)

    if len(named) < floor:
        print(f"ERROR: only {len(named)} {country} venues carry a town (expected at least "
              f"{floor}). Rebuild that country's table before generating — a short sweep "
              f"means the source moved, not that the venues lost their towns.", file=sys.stderr)
        sys.exit(1)

    return [(v["displayName"], v["town"]) for v in named]


def main() -> int:
    rows: list[tuple[str, str]] = []
    counts = []
    for country, path, floor in SOURCES:
        country_rows = rows_from(country, path, floor)
        counts.append(f"{len(country_rows)} in {country}")
        rows += country_rows

    duplicates = sorted({name for name, _ in rows
                         if sum(1 for other, _ in rows if other == name) > 1})
    if duplicates:
        print(f"ERROR: two venues share a displayName: {duplicates[:10]}", file=sys.stderr)
        return 1

    rows.sort()
    write_table(OUT, rows)
    print(f"Wrote {OUT.relative_to(ROOT)}: {len(rows)} venues ({', '.join(counts)}) / "
          f"{len({t for _, t in rows})} towns")
    return 0


if __name__ == "__main__":
    sys.exit(main())
