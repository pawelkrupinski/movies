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

`--audit` answers the question this table cannot answer about itself: whether a
venue is out of town and simply never got annotated. See [[audit]].
"""
from __future__ import annotations

import argparse
import json
import pathlib
import re
import sys
import time
import unicodedata
import urllib.parse
import urllib.request

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


FILMWEB_LISTING = "https://www.filmweb.pl/showtimes/"
UA = "Mozilla/5.0 (compatible; kinowo-cinema-discovery/1.0)"
LISTING_LINK = re.compile(r'href="/showtimes/[^"/]+/([^"]+)-(\d+)"')


def city_labels() -> dict:
    """City slug -> the city's own name, which is also its name on Filmweb."""
    return dict(re.findall(
        r'slug\s*=\s*"([^"]+)",\s*\n\s*labels\s*=\s*CityLabels\(nominative\s*=\s*"([^"]+)"',
        CITY.read_text()))


def tokens(name: str) -> set:
    return {w for w in re.split(r"[^a-z0-9]+", fold(name)) if len(w) > 2}


def audit() -> int:
    """Find a venue that is out of town and was never annotated.

    The table is built from annotations, so it cannot tell you about a venue
    nobody annotated — an in-town venue and a forgotten out-of-town one look
    exactly alike in it. Filmweb can: it files each cinema under the town it is
    actually in, so a venue that does NOT appear under its own city's listing is
    somewhere else, and if it has no annotation either, nothing on the page will
    ever say where.

    One GET per city, 41 of them. Run when the roster gains venues.
    """
    venues = json.loads(OUT.read_text())
    labels = city_labels()

    listings, unchecked = {}, []
    for slug in sorted({v["citySlug"] for v in venues}):
        name = labels.get(slug)
        try:
            request = urllib.request.Request(
                FILMWEB_LISTING + urllib.parse.quote(name), headers={"User-Agent": UA})
            with urllib.request.urlopen(request, timeout=30) as response:
                html = response.read().decode("utf-8", "replace")
            found = [urllib.parse.unquote(m.group(1)).replace("+", " ")
                     for m in LISTING_LINK.finditer(html)]
        except Exception as e:                                    # noqa: BLE001
            found, e = [], e
            print(f"  {slug}: listing could not be fetched ({e})", file=sys.stderr)
        if found:
            listings[slug] = found
        else:
            unchecked.append(slug)
        time.sleep(0.4)

    suspects = []
    for venue in venues:
        if venue["town"] or venue["citySlug"] not in listings:
            continue
        ours = tokens(venue["displayName"])
        if not any(ours & tokens(listed) for listed in listings[venue["citySlug"]]):
            suspects.append((venue["citySlug"], venue["displayName"]))

    print(f"audited {len(listings)} cities; {len(unchecked)} have no Filmweb listing of "
          f"their own and were skipped: {unchecked}")
    if suspects:
        print(f"{len(suspects)} venues are not listed under their own city and carry no "
              f"annotation — find out where they are and annotate them:", file=sys.stderr)
        for slug, name in suspects:
            print(f"  {slug}: {name}", file=sys.stderr)
        return 1
    print("no venue is out of town without an annotation")
    return 0


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--audit", action="store_true",
                        help="check against Filmweb that no venue is out of town without "
                             "an annotation; one GET per city, no files written")
    if parser.parse_args().audit:
        return audit()

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
