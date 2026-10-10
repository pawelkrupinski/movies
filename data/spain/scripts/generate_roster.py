#!/usr/bin/env python3
"""Generate common/src/main/scala/models/SpanishRosterData.scala from pages.json.

The JSON -> Scala step of the Spanish roster pipeline, modelled on
data/us/scripts/generate_roster.py. Reads the PAGES (`pages.json`, built by
`build_pages.py`: which town or cluster page each venue is on, and where each page
that stopped existing now redirects), the harvested venues (`provinces.json`) and
the Ocine chain's own ticketing servers (`ocine.json`), and emits the flat tuple
data `models.SpanishRoster` materialises into City/Cinema objects.

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
import re
import sys

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parents[2] / "scripts"))
import retired_venues  # noqa: E402

ROOT = pathlib.Path(__file__).resolve().parents[3]
DATA = ROOT / "data" / "spain"
OCINE = DATA / "ocine.json"
PAGES = DATA / "pages.json"
OUT = ROOT / "common" / "src" / "main" / "scala" / "models" / "SpanishRosterData.scala"

# One `Seq(...)` per 40 pages, so no single generated method approaches the JVM's
# 64 KB method-size limit as the roster grows.
CHUNK = 40


def scala_string(value: str) -> str:
    return '"' + value.replace("\\", "\\\\").replace('"', '\\"') + '"'


def ident(slug: str) -> str:
    return "p_" + re.sub(r"[^a-z0-9]", "_", slug)


def scala_option(value) -> str:
    return "None" if value is None else f"Some({scala_string(value)})"


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


def placement_problems(venues: dict, pages: list) -> list[str]:
    """Every venue on exactly one page, and every page's venues in the roster."""
    on = collections.Counter(name for p in pages for name in p["cinemas"])
    return ([f"venue {name!r} is on {n} pages" for name, n in sorted(on.items()) if n > 1] +
            [f"venue {name!r} is on no page — rebuild pages.json" for name in sorted(set(venues) - set(on))] +
            [f"page venue {name!r} is not in the roster" for name in sorted(set(on) - set(venues))])


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

    lines = [
        "// GENERATED from data/spain/pages.json by data/spain/scripts/generate_roster.py",
        "// — do NOT edit by hand. Full Spanish cinema roster: "
        f"{len(pages)} pages / {len(venues)} cinemas (SensaCine, plus the Ocine",
        "// venues it does not list, from data/spain/ocine.json).",
        "// Regenerate with `python3 data/spain/scripts/generate_roster.py` after re-clustering;",
        "// see data/spain/README.md.",
        "package models",
        "",
        "private[models] object SpanishRosterData {",
        "  // (displayName, pillName, SensaCine theaterId, Ocine ticketing server) — a venue",
        "  // SensaCine does not list has no theaterId and is scraped off its own server",
        "  type C = (String, String, Option[String], Option[String])",
        "  // (slug, slug qualified with its autonomous community, name, province, lat, lon,",
        "  //  zoneId, multiTown, towns, cinemas)",
        "  type R = (String, String, String, String, Double, Double, String, Boolean, Seq[String], Seq[C])",
        "",
    ]
    for page in pages:
        rows = ",\n".join(
            "    ({}, {}, {}, {})".format(
                scala_string(name), scala_string(name),
                scala_option(venues[name]["theaterId"]), scala_option(venues[name].get("ocineServer")))
            for name in page["cinemas"])
        lines.append(
            "  private def {}: R = ({}, {}, {}, {}, {}, {}, {}, {}, Seq({}), Seq(\n{}\n  ))".format(
                ident(page["slug"]), scala_string(page["slug"]), scala_string(page["qualifiedSlug"]),
                scala_string(page["name"]), scala_string(page["province"]), page["lat"], page["lon"],
                scala_string(page["zoneId"]), "true" if page["kind"] == "cluster" else "false",
                ", ".join(scala_string(t) for t in page["towns"]), rows))

    lines.append("")
    names = [ident(p["slug"]) for p in pages]
    chunks = [names[i:i + CHUNK] for i in range(0, len(names), CHUNK)]
    for index, chunk in enumerate(chunks):
        lines.append(f"  private def chunk{index}: Seq[R] = Seq({', '.join(chunk)})")
    lines.append("  val pages: Seq[R] = " + " ++ ".join(f"chunk{i}" for i in range(len(chunks))))
    lines.append("")
    lines.append("  /** Pages that no longer exist — the provinces Spain's pages were until 2026-10")
    lines.append("   *  among them — and the page now holding most of their venues: (slug, slug")
    lines.append("   *  qualified with its autonomous community, the page's slug). */")
    lines.append("  val retired: Seq[(String, String, String)] = Seq(")
    lines.extend(f"    ({scala_string(slug)}, {scala_string(r['qualifiedSlug'])}, {scala_string(r['page'])}),"
                 for slug, r in sorted(retired_pages.items()))
    lines.append("  )")
    lines.append("}")

    OUT.write_text("\n".join(lines) + "\n")
    print(f"Wrote {OUT.relative_to(ROOT)}: {len(pages)} pages / {len(venues)} cinemas / "
          f"{len(retired_pages)} retired slugs")
    return 0


if __name__ == "__main__":
    sys.exit(main())
