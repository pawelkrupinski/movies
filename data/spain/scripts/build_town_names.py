#!/usr/bin/env python3
"""Build data/spain/town-names.json — the accents SensaCine drops.

SensaCine writes its own town headers unaccented and title-cased: "Alcala De
Henares" for Alcalá de Henares, "Alcorcon" for Alcorcón, "Villaviciosa de Odon"
for Odón. Only 28 of the 423 towns it gives us keep their accents. That was
cosmetic while the town was an internal grouping key; it is not now that
`City.coveredPlaces` puts these names in the page's `<h1>`, its meta
description and its schema.org `containsPlace`, on 48 of Spain's 52 province
pages.

The accents are not in anything SensaCine serves — not the visible header, not
the address block, not the JSON-LD `addressLocality`, not the `<title>` — so
they come from GeoNames' Spanish municipality dump, which `geocode_provinces.py`
already fetches and parses for the province coordinates.

The correction can only ever RE-ACCENT a town, never rename one: a GeoNames name
is accepted only when it folds to exactly the same ASCII key as the harvested
name, so "Alcala De Henares" can become "Alcalá de Henares" and cannot become
anything else. Where several municipalities share a key the most populous wins,
which is the same tie-break geocode_provinces.py uses and is right for the same
reason: it is the one with the cinema.

Usage:
    mkdir -p data/spain/geonames
    curl -sL https://download.geonames.org/export/dump/ES.zip -o data/spain/geonames/ES.zip
    unzip -o data/spain/geonames/ES.zip -d data/spain/geonames
    python3 data/spain/scripts/build_town_names.py
    rm -rf data/spain/geonames   # ~11MB dump, not checked in
"""
from __future__ import annotations

import collections
import json
import pathlib
import sys
import unicodedata

ROOT = pathlib.Path(__file__).resolve().parents[3]
DATA = ROOT / "data" / "spain"
GEONAMES = DATA / "geonames" / "ES.txt"
PROVINCES = DATA / "provinces.json"
OUT = DATA / "town-names.json"

# Same columns, same populated-place codes as geocode_provinces.py.
COLS = [
    "geonameid", "name", "asciiname", "alternatenames", "latitude", "longitude",
    "feature_class", "feature_code", "country_code", "cc2", "admin1", "admin2",
    "admin3", "admin4", "population", "elevation", "dem", "timezone", "moddate",
]
POPULATED = {
    "PPL", "PPLA", "PPLA2", "PPLA3", "PPLA4", "PPLA5", "PPLC", "PPLF",
    "PPLG", "PPLL", "PPLR", "PPLS", "PPLX", "PPLQ",
}


def fold(s: str) -> str:
    """A town's ASCII identity: accents stripped, case and spacing flattened.

    Two names folding the same ARE the same town written two ways, which is the
    whole safety property here — the correction is applied only within a fold
    class, so it can restore an accent and can never substitute a town.
    """
    s = unicodedata.normalize("NFKD", s)
    s = "".join(c for c in s if not unicodedata.combining(c))
    return " ".join(s.lower().split())


def geonames_by_fold() -> dict[str, list[tuple[int, str]]]:
    try:
        handle = open(GEONAMES, encoding="utf-8")
    except FileNotFoundError:
        print(f"FATAL: {GEONAMES.relative_to(ROOT)} not found. Fetch it first:\n"
              f"  mkdir -p data/spain/geonames\n"
              f"  curl -sL https://download.geonames.org/export/dump/ES.zip "
              f"-o data/spain/geonames/ES.zip\n"
              f"  unzip -o data/spain/geonames/ES.zip -d data/spain/geonames", file=sys.stderr)
        return {}

    index: dict[str, list[tuple[int, str]]] = collections.defaultdict(list)
    with handle:
        for line in handle:
            parts = line.rstrip("\n").split("\t")
            if len(parts) < 15:
                continue
            row = dict(zip(COLS, parts))
            if row["feature_class"] != "P" or row["feature_code"] not in POPULATED:
                continue
            try:
                population = int(row["population"] or 0)
            except ValueError:
                population = 0
            # Only the canonical name is a candidate SPELLING; the alternates are
            # other languages and exonyms, which are not what we are correcting.
            index[fold(row["name"])].append((population, row["name"]))
    return index


def corrections(towns: set[str], index: dict[str, list[tuple[int, str]]]) -> dict[str, str]:
    out = {}
    for town in sorted(towns):
        candidates = index.get(fold(town), [])
        if not candidates:
            continue
        best = max(candidates)[1]
        if best != town:
            out[town] = best
    return out


def main() -> int:
    index = geonames_by_fold()
    if not index:
        return 1

    provinces = json.loads(PROVINCES.read_text())
    towns = {c["town"] for p in provinces for c in p["cinemas"] if c.get("town")}
    fixed = corrections(towns, index)

    OUT.write_text(json.dumps(fixed, indent=1, ensure_ascii=False, sort_keys=True) + "\n")
    accented = sum(1 for k, v in fixed.items() if fold(k) == fold(v) and k.lower() != v.lower())
    print(f"Wrote {OUT.relative_to(ROOT)}: {len(fixed)} of {len(towns)} towns corrected "
          f"({accented} differing by more than case)")
    unmatched = sorted(t for t in towns if fold(t) not in index)
    if unmatched:
        print(f"  {len(unmatched)} towns GeoNames does not know, left as harvested: "
              f"{unmatched[:8]}", file=sys.stderr)
    return 0


if __name__ == "__main__":
    sys.exit(main())
