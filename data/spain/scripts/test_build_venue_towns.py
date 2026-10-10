#!/usr/bin/env python3
"""Tests for build_venue_towns.py's municipality resolution, on synthetic GeoNames
rows (no dump), and for the committed town-coords.json.

    python3 data/spain/scripts/test_build_venue_towns.py
"""
import json
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent))
import build_venue_towns as b  # noqa: E402


def muni(name, pop, alternates=()):
    names, full = set(), set()
    for n in (name, *alternates):
        names |= b.spellings(n)
        full |= b.spellings(n, strip_article=False)
    return {"name": b.uninvert(name), "names": names, "full": full, "pop": pop, "admin2": "X",
            "lat": 41.0, "lon": 2.0}


MUNIS = {
    "08088": muni("la Garriga", 15000),
    "08042": muni("Cànoves i Samalús", 3000),
    "08118": muni("Masnou, El", 23000),
    "08101": muni("l'Hospitalet de Llobregat", 257000, ["L'Hospitalet de Llobregat"]),
    "04053": muni("Huércal-Overa", 18000),
    "14024": muni("Espiel", 2400),
    "46131": muni("Gandia", 79000),
    "33019": muni("Corvera de Asturias", 16000),
    "33004": muni("Avilés", 76000),
    "02003": muni("Albacete", 172000),
    "02081": muni("Villarrobledo", 25000),
}
POSTAL = {
    "08530": [("La Garriga", "08042")],   # GeoNames files La Garriga's code under Cànoves
    "04600": [("Huercal-Overa", "04053")],
    "46730": [("Gandia", "46131"), ("Puerto De Gandia", "46131")],
    "33468": [("Los Campos", "33019")],
    "02600": [("Villarrobledo", "02081")],
}


def resolve(name, town, locality, code):
    ine, how = b.resolve((name, town, locality, code), MUNIS, POSTAL, {})
    return MUNIS[ine]["name"] if ine else None, how


def check(name, cond):
    print(("PASS " if cond else "FAIL ") + name)
    if not cond:
        sys.exit(1)


def synthetic():
    check("the locality names the municipality, over a wrong header",
          resolve("Cine Alhambra", "Samalus", "La Garriga", "8530")[0] == "la Garriga")
    check("a header without its article still names the municipality",
          resolve("Cine la Calandria", "Masnou", "Masnou", "08320")[0] == "El Masnou")
    check("a header naming a hamlet elsewhere falls back to the postal code",
          resolve("Cine Municipal Huércal-Overa", "Estacion De Espiel", "Estacion De Espiel", "04600")
          == ("Huércal-Overa", "postal"))
    check("a district header resolves to its municipality through the postal code",
          resolve("Ozone Gandía", "Grau I Platja", "Grau I Platja", "46730")[0] == "Gandia")
    check("header and locality disagreeing: the postal code's municipality wins",
          resolve("Odeon Multicines Parque Astur", "Corvera de Asturias", "Aviles", "33468")[0]
          == "Corvera de Asturias")
    saved = dict(b.PINNED)
    b.PINNED["Gran Teatro de Villarrobledo"] = "Villarrobledo"
    try:
        check("a pinned venue goes where it is pinned, whatever its header says",
              resolve("Gran Teatro de Villarrobledo", "Albacete", "Albacete", "02600") == ("Villarrobledo", "pinned"))
    finally:
        b.PINNED.clear()
        b.PINNED.update(saved)
    check("a postal code in another province never matches a namesake there",
          resolve("Somewhere", "Gandia", "Gandia", "04999")[0] is None)
    check("a header's particles are lowercased, its first word kept",
          b.spanish_case("Arroyo De La Luz") == "Arroyo de la Luz" and b.spanish_case("La Coruna") == "La Coruna")
    check("GeoNames' article-last municipality reads article-first",
          b.uninvert("Ejido, El") == "El Ejido" and b.uninvert("Eliana, l'") == "l'Eliana")


def committed():
    data = json.loads((b.OUT).read_text())
    towns, venues = data["towns"], data["venues"]
    check("town-coords.json: every venue has a known town", set(venues.values()) <= set(towns))
    check("town-coords.json: 602 venues", len(venues) == 602)
    check("town-coords.json: Huércal-Overa's cinema is in Almería, not Córdoba",
          towns[venues["Cine Municipal Huércal-Overa"]]["province"] == "Almería")
    check("town-coords.json: the Autocinema Tenerife is on Tenerife",
          towns[venues["Autocinema Tenerife"]]["province"] == "Santa Cruz de Tenerife")


def main():
    synthetic()
    committed()
    print("all passed")


if __name__ == "__main__":
    main()
