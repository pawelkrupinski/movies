#!/usr/bin/env python3
"""Generate common/src/main/scala/models/UkVenueTowns.scala from venues.json.

The JSON -> Scala step of the UK town harvest, the counterpart of
data/spain/scripts/generate_roster.py.

A side table rather than a field on the venue, because the UK roster is the one
that is not generated: its ~840 venues are hand-written `case object`s in
Cinema.scala, and this is the only country whose towns cannot ride along in a
generated tuple. Keyed on `displayName` for the reason everything else is — it
is the wire key a venue is identified by everywhere else in the codebase.

It refuses to emit a town for a venue whose display name it cannot pin, and
refuses to write a table that has drifted to nearly nothing, since a silently
half-empty table is a page that quietly stops naming its towns.

Usage:  python3 data/uk/scripts/generate_towns.py
"""
from __future__ import annotations

import json
import pathlib
import sys

ROOT = pathlib.Path(__file__).resolve().parents[3]
SRC = ROOT / "data" / "uk" / "venues.json"
OUT = ROOT / "common" / "src" / "main" / "scala" / "models" / "UkVenueTowns.scala"

# One `Seq(...)` per 150 venues, for the reason the other rosters chunk: no
# single generated method may approach the JVM's 64 KB method-size limit.
CHUNK = 150

# A sweep that comes back with a fraction of the roster means Flicks changed its
# page or throttled us out, not that the UK lost its towns.
MIN_VENUES = 700


def scala_string(value: str) -> str:
    return '"' + value.replace("\\", "\\\\").replace('"', '\\"') + '"'


def main() -> int:
    venues = json.loads(SRC.read_text())
    named = [v for v in venues if v["displayName"] and v["town"]]

    unnamed = [v["flicksSlug"] for v in venues if not v["displayName"]]
    if unnamed:
        print(f"ERROR: {len(unnamed)} venues have no displayName — the case object could not be "
              f"read out of Cinema.scala: {unnamed[:10]}", file=sys.stderr)
        return 1

    if len(named) < MIN_VENUES:
        print(f"ERROR: only {len(named)} venues carry a town (expected at least {MIN_VENUES}). "
              f"Re-run data/uk/scripts/harvest_towns.py — a short sweep means Flicks throttled "
              f"or moved the address block, not that the venues lost their towns.", file=sys.stderr)
        return 1

    duplicates = {v["displayName"] for v in named
                  if sum(1 for w in named if w["displayName"] == v["displayName"]) > 1}
    if duplicates:
        print(f"ERROR: two venues share a displayName: {sorted(duplicates)[:10]}", file=sys.stderr)
        return 1

    rows = sorted((v["displayName"], v["town"]) for v in named)
    lines = [
        "// GENERATED from data/uk/venues.json by data/uk/scripts/generate_towns.py",
        f"// — do NOT edit by hand. The town each of {len(rows)} UK venues sits in, harvested",
        "// from Flicks. Regenerate with `python3 data/uk/scripts/harvest_towns.py` followed by",
        "// `python3 data/uk/scripts/generate_towns.py`; see data/uk/README.md.",
        "package models",
        "",
        "/** The town each UK venue sits in — the roster fact every other country",
        " *  carries in its generated roster and the UK, whose venues are hand-written,",
        " *  has to keep beside it. Read by [[UkCity.extraPlaces]], so that",
        " *  `/aberdeenshire/` names Peterhead and Banchory rather than only itself. */",
        "private[models] object UkVenueTowns {",
        "",
    ]

    chunks = [rows[i:i + CHUNK] for i in range(0, len(rows), CHUNK)]
    for index, chunk in enumerate(chunks):
        body = ",\n".join(
            f"    ({scala_string(name)}, {scala_string(town)})" for name, town in chunk)
        lines.append(f"  private def chunk{index}: Seq[(String, String)] = Seq(\n{body}\n  )")
        lines.append("")

    lines.append("  /** Venue display name -> its town. Display name because that is the key a")
    lines.append("   *  venue is identified by everywhere else — `Source.byDisplayName`, the")
    lines.append("   *  stored slots, the scrape catalog. */")
    lines.append("  val byDisplayName: Map[String, String] = ("
                 + " ++ ".join(f"chunk{i}" for i in range(len(chunks))) + ").toMap")
    lines.append("")
    lines.append("  /** The towns a group of venues sits in, most venues first — the order")
    lines.append("   *  [[City.coveredPlaces]] promises. A venue the table does not know is")
    lines.append("   *  simply not counted, so a fresh cinema names no town until the next")
    lines.append("   *  sweep rather than blanking the ones around it. */")
    lines.append("  def of(cinemas: Seq[Cinema]): Seq[String] =")
    lines.append("    TownRanking.ranked(cinemas.flatMap(c => byDisplayName.get(c.displayName)))")
    lines.append("}")

    OUT.write_text("\n".join(lines) + "\n")
    print(f"Wrote {OUT.relative_to(ROOT)}: {len(rows)} venues / "
          f"{len({t for _, t in rows})} towns")
    return 0


if __name__ == "__main__":
    sys.exit(main())
