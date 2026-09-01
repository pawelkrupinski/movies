#!/usr/bin/env python3
"""
Build data/spain/provinces.json -- the roster the app loads -- from
data/spain/theaters-raw.json (the crawl) and data/spain/province-coords.json
(the geocode).

One object per province, sorted by name:
  {"slug", "name", "lat", "lon", "zoneId", "towns": [...], "cinemas": [...]}
Each cinema: {"theaterId", "name", "town", "displayName"}.

displayName is the wire key every stored showtime is filed under, so it MUST
be globally unique across all of Spain. The rule (see qualify_display_names
below): start from the raw venue name; if it repeats, qualify with the town
("Cinesa Diagonal (Barcelona)"); if name+town also repeats, qualify with the
province too. If a collision survives both qualification passes, the script
refuses to emit -- sys.exit(1) with the offending names -- rather than
silently dropping a duplicate.
"""
import collections
import json
import re
import sys
import unicodedata

THEATERS_RAW_PATH = "data/spain/theaters-raw.json"
COORDS_PATH = "data/spain/province-coords.json"
OUT_PATH = "data/spain/provinces.json"


def slugify(name: str) -> str:
    s = unicodedata.normalize("NFKD", name)
    s = "".join(c for c in s if not unicodedata.combining(c))
    s = s.lower()
    s = re.sub(r"[^a-z0-9]+", "-", s)
    return s.strip("-")


def build_display_names(theaters):
    """Returns (theaterId -> displayName, town-qualified count,
    town+province-qualified count, example town-qualified names,
    example province-qualified names). Exits non-zero if a collision
    survives both qualification passes."""
    by_name = collections.defaultdict(list)
    for t in theaters:
        by_name[t["name"]].append(t)

    display = {}
    qualified_by_town = 0
    qualified_by_province = 0
    examples_town, examples_province = [], []

    for name, group in by_name.items():
        if len(group) == 1:
            display[group[0]["theaterId"]] = name
            continue
        by_town = collections.defaultdict(list)
        for t in group:
            by_town[t["town"]].append(t)
        for town, subgroup in by_town.items():
            if len(subgroup) == 1:
                t = subgroup[0]
                dn = f"{name} ({town})"
                display[t["theaterId"]] = dn
                qualified_by_town += 1
                if len(examples_town) < 3:
                    examples_town.append(dn)
            else:
                for t in subgroup:
                    dn = f"{name} ({town}, {t['provinceName']})"
                    display[t["theaterId"]] = dn
                    qualified_by_province += 1
                    if len(examples_province) < 3:
                        examples_province.append(dn)

    counts = collections.Counter(display.values())
    unresolved = {v: c for v, c in counts.items() if c > 1}
    if unresolved:
        print(f"FATAL: {len(unresolved)} displayName(s) still collide after "
              f"town+province qualification: {unresolved}", file=sys.stderr)
        sys.exit(1)

    return display, qualified_by_town, qualified_by_province, examples_town, examples_province


def main():
    theaters = json.load(open(THEATERS_RAW_PATH, encoding="utf-8"))
    coords = json.load(open(COORDS_PATH, encoding="utf-8"))

    display, qt, qp, ex_town, ex_province = build_display_names(theaters)

    by_province = collections.defaultdict(list)
    for t in theaters:
        by_province[t["provinceName"]].append(t)

    missing_coords = sorted(p for p in by_province if p not in coords)
    if missing_coords:
        print(f"FATAL: missing geocoded coordinates for provinces: {missing_coords}",
              file=sys.stderr)
        sys.exit(1)

    provinces_out = []
    for province_name in sorted(by_province):
        theaters_in = by_province[province_name]
        c = coords[province_name]
        towns = sorted({t["town"] for t in theaters_in})
        cinemas = sorted(
            (
                {
                    "theaterId": t["theaterId"],
                    "name": t["name"],
                    "town": t["town"],
                    "displayName": display[t["theaterId"]],
                }
                for t in theaters_in
            ),
            key=lambda x: x["name"],
        )
        provinces_out.append({
            "slug": slugify(province_name),
            "name": province_name,
            "lat": c["lat"],
            "lon": c["lon"],
            "zoneId": c["zoneId"],
            "towns": towns,
            "cinemas": cinemas,
        })

    dupe_slugs = {s: c for s, c in collections.Counter(p["slug"] for p in provinces_out).items() if c > 1}
    if dupe_slugs:
        print(f"FATAL: duplicate province slugs: {dupe_slugs}", file=sys.stderr)
        sys.exit(1)

    with open(OUT_PATH, "w", encoding="utf-8") as f:
        json.dump(provinces_out, f, ensure_ascii=False, indent=2)

    total_cinemas = sum(len(p["cinemas"]) for p in provinces_out)
    print(f"WROTE {len(provinces_out)} provinces, {total_cinemas} cinemas to {OUT_PATH}")
    print(f"displayName qualification: {qt} qualified by (name, town), "
          f"{qp} qualified by (name, town, province)")
    if ex_town:
        print(f"  town-qualified examples: {ex_town}")
    if ex_province:
        print(f"  province-qualified examples: {ex_province}")


if __name__ == "__main__":
    main()
