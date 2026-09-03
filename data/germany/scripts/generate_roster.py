#!/usr/bin/env python3
"""Generate common/src/main/scala/models/GermanRosterData.scala from regions.json.

The JSON -> Scala step of the German roster pipeline, the counterpart of
data/spain/scripts/generate_roster.py. Reads the clustered roster
(`regions.json`, written by cluster_regions.py) and emits the flat tuple data
`models.GermanRoster` materialises into City/Cinema objects.

Recovered: the file it writes was checked in as generated but its generator had
been lost, so this was reconstructed against the committed output and verified
byte-identical to it before the `cities` field was added.

It refuses to emit a duplicate `displayName`, which fails SILENTLY downstream:
that string is the wire key every stored showtime is filed under, and
`Source.byDisplayName` is a plain `toMap`, so two venues sharing one quietly
become one venue.

Usage:  python3 data/germany/scripts/generate_roster.py
"""
import json
import pathlib
import re
import sys

ROOT = pathlib.Path(__file__).resolve().parents[3]
DATA = ROOT / "data" / "germany"
OUT = ROOT / "common" / "src" / "main" / "scala" / "models" / "GermanRosterData.scala"

CHUNK = 40


def scala_string(value: str) -> str:
    return '"' + value.replace("\\", "\\\\").replace('"', '\\"') + '"'


def ident(slug: str) -> str:
    return "r_" + re.sub(r"[^a-z0-9]", "_", slug)


def main() -> int:
    regions = json.loads((DATA / "regions.json").read_text())
    cinemas = sum(len(r["cinemas"]) for r in regions)

    seen: dict[str, str] = {}
    for region in regions:
        for cinema in region["cinemas"]:
            name = cinema["displayName"]
            if name in seen:
                print(f"ERROR: duplicate displayName {name!r} in "
                      f"{seen[name]} and {region['name']}", file=sys.stderr)
                return 1
            seen[name] = region["name"]

    lines = [
        "// GENERATED from data/germany/regions.json by the DE roster generator — do NOT edit by hand.",
        f"// Full German cinema roster: {len(regions)} regions / {cinemas:,} cinemas (Filmstarts). Regenerate with",
        "// the generator in data/germany/scripts after re-harvesting; see data/germany/README.md.",
        "package models",
        "",
        "private[models] object GermanRosterData {",
        "  // (displayName, pillName, filmstarts theaterId)",
        "  type C = (String, String, String)",
        "  // (slug, name, lat, lon, cities, cinemas)",
        "  type R = (String, String, Double, Double, Seq[String], Seq[C])",
        "",
    ]

    for region in regions:
        venues = ",\n".join(
            "    ({}, {}, {})".format(
                scala_string(c["displayName"]), scala_string(c["displayName"]), scala_string(c["theaterId"]))
            for c in region["cinemas"])
        # `cities` is already ranked by cinema count (cluster_regions.py), which
        # is the order `City.coveredPlaces` promises its consumers.
        cities = ", ".join(scala_string(c) for c in region["cities"])
        lines.append(
            "  private def {}: R = ({}, {}, {}, {}, Seq({}), Seq(\n{}\n  ))".format(
                ident(region["slug"]),
                scala_string(region["slug"]),
                scala_string(region["name"]),
                region["lat"], region["lon"],
                cities,
                venues))

    lines.append("")
    names = [ident(r["slug"]) for r in regions]
    chunks = [names[i:i + CHUNK] for i in range(0, len(names), CHUNK)]
    for index, chunk in enumerate(chunks):
        lines.append(f"  private def chunk{index}: Seq[R] = Seq({', '.join(chunk)})")
    lines.append("  val regions: Seq[R] = " + " ++ ".join(f"chunk{i}" for i in range(len(chunks))))
    lines.append("}")

    OUT.write_text("\n".join(lines) + "\n")
    print(f"Wrote {OUT.relative_to(ROOT)}: {len(regions)} regions / {cinemas:,} cinemas")
    return 0


if __name__ == "__main__":
    sys.exit(main())
