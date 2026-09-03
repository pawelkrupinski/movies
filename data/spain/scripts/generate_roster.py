#!/usr/bin/env python3
"""Generate common/src/main/scala/models/SpanishRosterData.scala from provinces.json.

The JSON -> Scala step of the Spanish roster pipeline, modelled on
data/us/scripts/generate_roster.py. Reads the harvested roster
(`provinces.json`) plus the province -> autonomous-community reference table
(`communities.json`) and emits the flat tuple data `models.SpanishRoster`
materialises into City/Cinema objects.

Two things it refuses to do, because both fail SILENTLY downstream:

  * emit a province with no community — the community is what qualifies a
    province slug that another country already claims (`City.spanishSlugs`), so
    a missing one means an unqualifiable collision;
  * emit a duplicate `displayName` — that string is the wire key every stored
    showtime is filed under, and `Source.byDisplayName` is a plain `toMap`, so
    two venues sharing one silently become one venue.

Usage:  python3 data/spain/scripts/generate_roster.py
"""
import collections
import json
import pathlib
import re
import sys

ROOT = pathlib.Path(__file__).resolve().parents[3]
DATA = ROOT / "data" / "spain"
TOWN_NAMES = DATA / "town-names.json"
OUT = ROOT / "common" / "src" / "main" / "scala" / "models" / "SpanishRosterData.scala"

# Chunked exactly as the German roster is: one `Seq(...)` per 40 provinces, so no
# single generated method approaches the JVM's 64 KB method-size limit as the
# roster grows.
CHUNK = 40


def scala_string(value: str) -> str:
    return '"' + value.replace("\\", "\\\\").replace('"', '\\"') + '"'


def ident(slug: str) -> str:
    return "p_" + re.sub(r"[^a-z0-9]", "_", slug)


# SensaCine's own town headers title-case every word and mostly drop the
# accents, and those headers are where `town` comes from. The names go on the
# page — the `<h1>`, the meta description, the schema.org `containsPlace` — so
# they are written the way Spanish writes them.
#
# Two passes, because the two problems have different answers. The ACCENTS are
# not in anything SensaCine serves, so they come from GeoNames, via the
# `town-names.json` table `build_town_names.py` builds (100 of the 423 towns).
# The CASING is a rule — Spanish lowercases the particles in a toponym — and
# applies to the rest, including the 60 towns GeoNames does not know under the
# name we harvested.
PARTICLES = {"de", "del", "la", "las", "el", "los", "y", "i", "a", "o"}


def spanish_case(town: str) -> str:
    words = town.split(" ")
    return " ".join(
        w.lower() if i > 0 and w.lower() in PARTICLES else w
        for i, w in enumerate(words))


def town_name(raw: str, corrections: dict) -> str:
    """The town as Spanish writes it: GeoNames' spelling where it has one for
    exactly this town, and the casing rule everywhere else."""
    return corrections.get(raw) or spanish_case(raw)


def towns_of(province: dict, corrections: dict) -> list[str]:
    """The province's towns, the ones with most venues first (ties
    alphabetical) — the order `City.coveredPlaces` promises, because the
    consumers cap the list and the biggest towns are the ones worth naming."""
    counts = collections.Counter(
        town_name(c["town"], corrections) for c in province["cinemas"] if c.get("town"))
    return [t for t, _ in sorted(counts.items(), key=lambda kv: (-kv[1], kv[0]))]


def main() -> int:
    provinces = json.loads((DATA / "provinces.json").read_text())
    corrections = json.loads(TOWN_NAMES.read_text()) if TOWN_NAMES.exists() else {}
    communities = json.loads((DATA / "communities.json").read_text())
    communities.pop("_comment", None)

    missing = sorted(p["name"] for p in provinces if p["name"] not in communities)
    if missing:
        print(f"ERROR: no autonomous community for: {missing}", file=sys.stderr)
        print("Add them to data/spain/communities.json.", file=sys.stderr)
        return 1

    seen: dict[str, str] = {}
    for province in provinces:
        for cinema in province["cinemas"]:
            name = cinema["displayName"]
            if name in seen:
                print(f"ERROR: duplicate displayName {name!r} in "
                      f"{seen[name]} and {province['name']}", file=sys.stderr)
                return 1
            seen[name] = province["name"]

    lines = [
        "// GENERATED from data/spain/provinces.json by data/spain/scripts/generate_roster.py",
        "// — do NOT edit by hand. Full Spanish cinema roster: "
        f"{len(provinces)} provinces / {len(seen)} cinemas (SensaCine).",
        "// Regenerate with `python3 data/spain/scripts/generate_roster.py` after re-harvesting;",
        "// see data/spain/README.md.",
        "package models",
        "",
        "private[models] object SpanishRosterData {",
        "  // (displayName, pillName, sensacine theaterId)",
        "  type C = (String, String, String)",
        "  // (slug, name, autonomous community, lat, lon, zoneId, towns, cinemas)",
        "  type R = (String, String, String, Double, Double, String, Seq[String], Seq[C])",
        "",
    ]

    for province in provinces:
        venues = ",\n".join(
            "    ({}, {}, {})".format(
                scala_string(c["displayName"]), scala_string(c["displayName"]), scala_string(c["theaterId"]))
            for c in province["cinemas"])
        towns = ", ".join(scala_string(t) for t in towns_of(province, corrections))
        lines.append(
            "  private def {}: R = ({}, {}, {}, {}, {}, {}, Seq({}), Seq(\n{}\n  ))".format(
                ident(province["slug"]),
                scala_string(province["slug"]),
                scala_string(province["name"]),
                scala_string(communities[province["name"]]),
                province["lat"], province["lon"],
                scala_string(province["zoneId"]),
                towns,
                venues))

    lines.append("")
    names = [ident(p["slug"]) for p in provinces]
    chunks = [names[i:i + CHUNK] for i in range(0, len(names), CHUNK)]
    for index, chunk in enumerate(chunks):
        lines.append(f"  private def chunk{index}: Seq[R] = Seq({', '.join(chunk)})")
    lines.append("  val provinces: Seq[R] = "
                 + " ++ ".join(f"chunk{i}" for i in range(len(chunks))))
    lines.append("}")

    OUT.write_text("\n".join(lines) + "\n")
    print(f"Wrote {OUT.relative_to(ROOT)}: {len(provinces)} provinces / {len(seen)} cinemas")
    return 0


if __name__ == "__main__":
    sys.exit(main())
