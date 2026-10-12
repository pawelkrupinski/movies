#!/usr/bin/env python3
"""Generate SpanishOwnSourcesData.scala -- which Spanish roster venues are scraped off
a source other than SensaCine (and Ocine's own servers, which the roster itself
carries), keyed by SensaCine theaterId:

  * data/spain/taquilla.json  (harvest_taquilla.py) -> TaquillaClient

Refuses a theaterId the roster does not have, or an Ocine venue -- a venue has
exactly one primary.

Usage: python3 data/spain/scripts/generate_own_sources.py
"""

import json
import pathlib
import sys

HERE = pathlib.Path(__file__).resolve().parent
DATA = HERE.parent
REPO = DATA.parent.parent
OUT = REPO / "worker/src/main/scala/services/cinemas/es/SpanishOwnSourcesData.scala"


def problems(roster_ids, ocine_ids, taquilla):
    out = []
    for name, table in (("taquilla.json", taquilla),):
        for tid in table:
            if tid not in roster_ids:
                out.append(f"{name}: {tid} is not a roster venue")
            if tid in ocine_ids:
                out.append(f"{name}: {tid} is an Ocine venue, scraped off its own server")
    return out


def scala_string(s):
    return '"' + s.replace("\\", "\\\\").replace('"', '\\"') + '"'


def render(taquilla):
    lines = [
        "// GENERATED from data/spain/taquilla.json by",
        "// data/spain/scripts/generate_own_sources.py -- do NOT edit by hand.",
        f"// {len(taquilla)} venues off taquilla.com; see data/spain/README.md.",
        "package services.cinemas.es",
        "",
        "private[cinemas] object SpanishOwnSourcesData {",
        "  /** SensaCine theaterId -> the venue on taquilla.com. */",
        "  val taquilla: Map[String, TaquillaVenue] = Map(",
    ]
    lines += [f"    {scala_string(tid)} -> TaquillaVenue({scala_string(v['venueId'])}, {scala_string(v['path'])}),"
              f"   // {v['name']}"
              for tid, v in sorted(taquilla.items())]
    lines += ["  )", "}", ""]
    return "\n".join(lines)


def main():
    provinces = json.loads((DATA / "provinces.json").read_text())
    roster_ids = {c["theaterId"] for p in provinces for c in p["cinemas"] if c.get("theaterId")}
    ocine_ids = set(json.loads((DATA / "ocine.json").read_text())["listed"])
    taquilla = json.loads((DATA / "taquilla.json").read_text())["venues"]
    found = problems(roster_ids, ocine_ids, taquilla)
    if found:
        for p in found:
            print(f"ERROR: {p}", file=sys.stderr)
        return 1
    OUT.write_text(render(taquilla))
    print(f"wrote {OUT.relative_to(REPO)}: {len(taquilla)} Taquilla")
    return 0


if __name__ == "__main__":
    sys.exit(main())
