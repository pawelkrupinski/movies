#!/usr/bin/env python3
"""Generate geodata/src/main/scala/models/GermanRosterData.scala from pages.json.

The JSON -> Scala step of the German roster pipeline, the counterpart of
data/spain/scripts/generate_roster.py. Reads the PAGES (`pages.json`, built by
`build_pages.py`: which town or cluster page each venue is on, and where each page
that stopped existing now redirects), the venues (`venues.json` less
`retired.json`) and each venue's kinoprogramm.com fallback (`kinoprogramm.json`),
and emits the flat tuple data `models.GermanRoster` materialises into City/Cinema
objects.

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
OUT = ROOT / "geodata" / "src" / "main" / "scala" / "models" / "GermanRosterData.scala"

# One `Seq(...)` per 40 pages, so no single generated method approaches the JVM's
# 64 KB method-size limit as the roster grows.
CHUNK = 40


def scala_string(value: str) -> str:
    return '"' + value.replace("\\", "\\\\").replace('"', '\\"') + '"'


def ident(slug: str) -> str:
    return "p_" + re.sub(r"[^a-z0-9]", "_", slug)


def roster_problems(roster: list) -> list[str]:
    seen, problems = set(), []
    for venue in roster:
        if venue["displayName"] in seen:
            problems.append(f"duplicate displayName {venue['displayName']!r}")
        seen.add(venue["displayName"])
    return problems


def render(roster: list, pages: list, retired: dict, kinoprogramm: dict, town_of) -> str:
    venues = {v["displayName"]: v for v in roster}
    laender = {p["bundesland"] for p in pages}
    lines = [
        "// GENERATED from data/germany/pages.json by data/germany/scripts/generate_roster.py",
        f"// — do NOT edit by hand. Full German cinema roster: {len(pages)} pages over {len(laender)} Bundesländer /",
        f"// {len(venues):,} cinemas (Filmstarts, and the ones only Cineamo or kinoheld list). Regenerate after adding venues or re-clustering;",
        "// see data/germany/README.md.",
        "package models",
        "",
        "private[models] object GermanRosterData {",
        "  // (displayName, pillName, venue key -- its Filmstarts theaterId, or the feed that found a venue",
        "  //  Filmstarts does not list (\"cineamo:997\") --, kinoprogramm.com fallback path, town)",
        "  type C = (String, String, String, Option[String], String)",
        "  // (slug, slug qualified with its Bundesland, name, Bundesland, lat, lon, multiTown, towns, cinemas)",
        "  type R = (String, String, String, String, Double, Double, Boolean, Seq[String], Seq[C])",
        "",
    ]
    for page in pages:
        rows = ",\n".join(
            "    ({}, {}, {}, {}, {})".format(
                scala_string(name), scala_string(name), scala_string(build_pages.venue_key(venues[name])),
                "Some({})".format(scala_string(kinoprogramm[venues[name]["theaterId"]]))
                if venues[name].get("theaterId") in kinoprogramm else "None",
                scala_string(town_of(venues[name]["city"])))
            for name in page["cinemas"])
        lines.append(
            "  private def {}: R = ({}, {}, {}, {}, {}, {}, {}, Seq({}), Seq(\n{}\n  ))".format(
                ident(page["slug"]), scala_string(page["slug"]), scala_string(page["qualifiedSlug"]),
                scala_string(page["name"]), scala_string(page["bundesland"]), page["lat"], page["lon"],
                "true" if page["kind"] == "cluster" else "false",
                ", ".join(scala_string(t) for t in page["towns"]), rows))

    lines.append("")
    names = [ident(p["slug"]) for p in pages]
    chunks = [names[i:i + CHUNK] for i in range(0, len(names), CHUNK)]
    for index, chunk in enumerate(chunks):
        lines.append(f"  private def chunk{index}: Seq[R] = Seq({', '.join(chunk)})")
    lines.append("  val pages: Seq[R] = " + " ++ ".join(f"chunk{i}" for i in range(len(chunks))))
    lines.append("")
    lines.append("  /** Pages that no longer exist — the regions Germany's pages were until 2026-10")
    lines.append("   *  among them — and the page now holding most of their venues: (slug, slug")
    lines.append("   *  qualified with its Bundesland, the page's slug). */")
    lines.append("  val retired: Seq[(String, String, String)] = Seq(")
    lines.extend(f"    ({scala_string(slug)}, {scala_string(r['qualifiedSlug'])}, {scala_string(r['page'])}),"
                 for slug, r in sorted(retired.items()))
    lines.append("  )")
    lines.append("")
    lines.extend(seasonal_venues.render_scala(seasonal_venues.seasonal_names("de", venues), scala_string))
    lines.append("}")
    return "\n".join(lines) + "\n"


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
    OUT.write_text(render(roster, data["pages"], data["retired"], kinoprogramm, town_of))
    covered = sum(1 for v in roster if v.get("theaterId") in kinoprogramm)
    print(f"Wrote {OUT.relative_to(ROOT)}: {len(data['pages'])} pages / {len(roster):,} cinemas, "
          f"{covered:,} with a kinoprogramm.com fallback / {len(data['retired'])} retired slugs")
    return 0


if __name__ == "__main__":
    sys.exit(main())
