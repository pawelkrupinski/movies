#!/usr/bin/env python3
"""Build data/pl/venues.json — the town each Polish venue sits in.

Poland was the last roster whose pages named no town but Trójmiasto's, and the
assumption behind that (that a Polish city page covers one town) is simply
false: 36 of the 41 cover more than one. `/tarnow/` lists cinemas in Biecz,
Gorlice, Bochnia, Brzesko, Tuchów, Solec-Zdrój and Dąbrowa Tarnowska;
`/walbrzych/` reaches Kłodzko, Świdnica and Dzierżoniów.

Unlike the UK this needs no harvest at all, because the answer was already
written down. Whoever wired an out-of-town venue annotated it on the line:

    case object KinoFarys extends Cinema("Farys", "Farys")   // Biecz — filmweb 2315

That is exactly the set worth having — the annotation exists BECAUSE the venue
is somewhere the city name does not say — so this reads those comments rather
than fetching anything.

A comment is prose, though, and one of them names a venue rather than a town
("Ursynowskie Centrum Kultury"). So every candidate is checked against the
GeoNames Polish gazetteer and dropped if it is not a real Polish place. Unlike
Spain the gazetteer only VALIDATES: GeoNames files Polish towns under English
exonyms (Warszawa is "Warsaw" there), so the name we keep is the one in the
comment, which is already correct Polish.

Usage:
    mkdir -p data/pl/geonames
    curl -sL https://download.geonames.org/export/dump/PL.zip -o data/pl/geonames/PL.zip
    unzip -o data/pl/geonames/PL.zip -d data/pl/geonames
    python3 data/pl/scripts/build_venue_towns.py
    rm -rf data/pl/geonames   # ~4MB dump, not checked in
"""
from __future__ import annotations

import json
import pathlib
import re
import sys
import unicodedata

ROOT = pathlib.Path(__file__).resolve().parents[3]
GEONAMES = ROOT / "data" / "pl" / "geonames" / "PL.txt"
CINEMA = ROOT / "common/src/main/scala/models/Cinema.scala"
CITY = ROOT / "common/src/main/scala/models/City.scala"
OUT = ROOT / "data" / "pl" / "venues.json"

POPULATED = {
    "PPL", "PPLA", "PPLA2", "PPLA3", "PPLA4", "PPLA5", "PPLC", "PPLF",
    "PPLG", "PPLL", "PPLR", "PPLS", "PPLX", "PPLQ",
}

# Abbreviations the annotations use for the qualifier half of a compound name.
# Poland has several towns distinguished only by it — Ostrów Wielkopolski from
# Ostrów Mazowiecka — so the qualifier has to be expanded, not dropped.
ABBREVIATIONS = {
    "wlkp.": "Wielkopolski", "wlkp": "Wielkopolski",
    "śl.": "Śląski", "śl": "Śląski",
    "maz.": "Mazowiecki", "maz": "Mazowiecki",
    "młp.": "Małopolski", "młp": "Małopolski",
    "zdr.": "Zdrój", "zdr": "Zdrój",
}


def fold(s: str) -> str:
    """A Polish place name's ASCII identity. `ł` has no combining form, so it is
    mapped by hand before the accents are stripped off everything else."""
    s = s.replace("ł", "l").replace("Ł", "L")
    s = unicodedata.normalize("NFKD", s)
    return " ".join("".join(c for c in s if not unicodedata.combining(c)).lower().split())


def expand(candidate: str) -> str:
    return " ".join(ABBREVIATIONS.get(w.lower(), w) for w in candidate.split())


def gazetteer() -> tuple[set, dict]:
    """Every Polish populated place, by fold — and by fold of its first word, so
    a short form ("Połczyn") still finds its town ("Połczyn-Zdrój") when it is
    the only one that starts that way."""
    try:
        handle = open(GEONAMES, encoding="utf-8")
    except FileNotFoundError:
        print(f"FATAL: {GEONAMES.relative_to(ROOT)} not found. Fetch it first:\n"
              f"  mkdir -p data/pl/geonames\n"
              f"  curl -sL https://download.geonames.org/export/dump/PL.zip "
              f"-o data/pl/geonames/PL.zip\n"
              f"  unzip -o data/pl/geonames/PL.zip -d data/pl/geonames", file=sys.stderr)
        sys.exit(1)

    exact: set = set()
    prefixes: dict = {}
    with handle:
        for line in handle:
            parts = line.rstrip("\n").split("\t")
            if len(parts) < 15 or parts[6] != "P" or parts[7] not in POPULATED:
                continue
            alternates = parts[3].split(",") if parts[3] else []
            for name in {parts[1], parts[2], *alternates}:
                if not name:
                    continue
                key = fold(name)
                exact.add(key)
                head = key.split("-")[0].split(" ")[0]
                if head and head != key:
                    prefixes.setdefault(head, set()).add(key)
    return exact, prefixes


def polish_venues() -> list[tuple[str, str]]:
    """(city slug, case object) for every venue of every Polish city, in order."""
    city, cinema = CITY.read_text(), CINEMA.read_text()
    cities = re.findall(
        r'case object \w+ extends City\(\s*\n?\s*slug\s*=\s*"([^"]+)"'
        r'[\s\S]{0,400}?val cinemas: Seq\[Cinema\]\s*=\s*Cinema\.(\w+)', city)
    lists = dict(re.findall(
        r'val (\w+): Seq\[Cinema\]\s*=\s*Seq\(([^)]*(?:\([^)]*\)[^)]*)*)\)', cinema))
    out = []
    for slug, list_name in cities:
        for obj in re.findall(r'\b([A-Z][A-Za-z0-9_]+)\b', lists.get(list_name, "")):
            out.append((slug, obj))
    return out


def annotations() -> tuple[dict, dict]:
    """case object -> (displayName, trailing same-line comment or "")."""
    text = CINEMA.read_text()
    names = dict(re.findall(
        r'case object ([A-Za-z0-9_]+)\s+extends \w*Cinema\("((?:[^"\\]|\\.)*)"', text))
    comments = dict(re.findall(
        r'case object ([A-Za-z0-9_]+)\s+extends \w*Cinema\('
        r'"(?:[^"\\]|\\.)*",\s*"(?:[^"\\]|\\.)*"\)[ \t]*//[ \t]*([^\n]*)', text))
    return names, comments


def town_of(comment: str, exact: set, prefixes: dict) -> str:
    """The town an annotation names, or "" when it names something else.

    The comment is `<Town> — <where the showtimes come from>`; the town is the
    half in front. It is kept only if the gazetteer knows it, which is what
    stops "Ursynowskie Centrum Kultury" being served as a town name.
    """
    candidate = expand(re.split(r"\s+[—–]\s+|\s+-\s+", comment.strip())[0].strip())
    if not candidate:
        return ""
    key = fold(candidate)
    if key in exact:
        return candidate
    matches = prefixes.get(key, set())
    return candidate if len(matches) == 1 else ""


def main() -> int:
    exact, prefixes = gazetteer()
    names, comments = annotations()

    venues, unknown = [], []
    for slug, obj in polish_venues():
        comment = comments.get(obj, "")
        town = town_of(comment, exact, prefixes) if comment else ""
        if comment and not town:
            unknown.append((obj, comment))
        venues.append({
            "citySlug": slug,
            "cinemaObject": obj,
            "displayName": names.get(obj, ""),
            "town": town,
            "annotation": comment,
        })

    OUT.parent.mkdir(parents=True, exist_ok=True)
    OUT.write_text(json.dumps(venues, indent=1, ensure_ascii=False) + "\n")
    with_town = sum(1 for v in venues if v["town"])
    cities = len({v["citySlug"] for v in venues if v["town"]})
    print(f"Wrote {OUT.relative_to(ROOT)}: {len(venues)} venues, {with_town} with a town, "
          f"across {cities} cities")
    if unknown:
        print(f"  {len(unknown)} annotations name no place the gazetteer knows, "
              f"left without a town: {unknown[:6]}", file=sys.stderr)
    return 0


if __name__ == "__main__":
    sys.exit(main())
