#!/usr/bin/env python3
"""Generate the tables under geodata/src/main/resources/generated/es-own-sources/
(taquilla.tsv, artesiete.tsv; read by `services.cinemas.es.SpanishOwnSourcesData`) -- which Spanish roster venues are scraped off
a source other than SensaCine (and Ocine's own servers, which the roster itself
carries), keyed by SensaCine theaterId:

  * data/spain/taquilla.json  (harvest_taquilla.py) -> TaquillaClient
  * data/spain/artesiete.json (hand-kept)           -> ArtesieteClient

Refuses a theaterId the roster does not have, an Ocine venue, or one named by both
files -- a venue has exactly one primary.

Usage: python3 data/spain/scripts/generate_own_sources.py
"""

import json
import pathlib
import sys

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parents[2] / "scripts"))
from generated_tables import write_table  # noqa: E402

HERE = pathlib.Path(__file__).resolve().parent
DATA = HERE.parent
REPO = DATA.parent.parent
OUT = REPO / "geodata/src/main/resources/generated/es-own-sources"


def problems(roster_ids, ocine_ids, taquilla, artesiete):
    out = []
    for name, table in (("taquilla.json", taquilla), ("artesiete.json", artesiete)):
        for tid in table:
            if tid not in roster_ids:
                out.append(f"{name}: {tid} is not a roster venue")
            if tid in ocine_ids:
                out.append(f"{name}: {tid} is an Ocine venue, scraped off its own server")
    for tid in sorted(set(taquilla) & set(artesiete)):
        out.append(f"{tid} is in both taquilla.json and artesiete.json")
    return out


def write(taquilla, artesiete):
    """Rows sorted by theaterId; the venue's name rides along for the reader of the table."""
    write_table(OUT / "taquilla.tsv", [[tid, v["venueId"], v["path"], v["name"]]
                                       for tid, v in sorted(taquilla.items())])
    write_table(OUT / "artesiete.tsv", [[tid, v["page"], v["name"]]
                                        for tid, v in sorted(artesiete.items())])


def main():
    provinces = json.loads((DATA / "provinces.json").read_text())
    roster_ids = {c["theaterId"] for p in provinces for c in p["cinemas"] if c.get("theaterId")}
    ocine_ids = set(json.loads((DATA / "ocine.json").read_text())["listed"])
    taquilla = json.loads((DATA / "taquilla.json").read_text())["venues"]
    artesiete = json.loads((DATA / "artesiete.json").read_text())["venues"]
    found = problems(roster_ids, ocine_ids, taquilla, artesiete)
    if found:
        for p in found:
            print(f"ERROR: {p}", file=sys.stderr)
        return 1
    write(taquilla, artesiete)
    print(f"wrote {OUT.relative_to(REPO)}/: {len(taquilla)} Taquilla, {len(artesiete)} Artesiete")
    return 0


if __name__ == "__main__":
    sys.exit(main())
