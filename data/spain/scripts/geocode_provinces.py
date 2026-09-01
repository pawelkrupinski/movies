#!/usr/bin/env python3
"""
Geocode all 52 Spanish provinces to their CAPITAL city's lat/lon, using the
free GeoNames bulk dump (data/spain/geonames/ES.txt, tab-separated) -- same
technique as data/germany/scripts/geocode_cities.py.

The dump itself (~11MB uncompressed) is NOT checked in -- only its small
derived output (data/spain/province-coords.json) is. Fetch it first:
    mkdir -p data/spain/geonames
    curl -sL https://download.geonames.org/export/dump/ES.zip -o data/spain/geonames/ES.zip
    unzip -o data/spain/geonames/ES.zip -d data/spain/geonames

Spain's 50 provinces + Ceuta + Melilla (the two autonomous cities, which
sensacine.com's /cines/ index treats as provinces) each have one capital
city. The capital is usually the province's namesake but not always (e.g.
Álava's capital is Vitoria-Gasteiz, Vizcaya's is Bilbao) -- PROVINCE_CAPITAL
below is the explicit, human-verified mapping.

Matches against GeoNames feature class P (populated place); prefers the
highest-population candidate for that name. Prints any of the 52 that fail
to resolve so they can be fixed by hand (this run: all 52 resolved).
"""
import json
import sys
import unicodedata
from collections import defaultdict

GEONAMES_PATH = "data/spain/geonames/ES.txt"
PROVINCES_RAW_PATH = "data/spain/theaters-raw.json"

# province display name (as scraped from sensacine.com) -> capital city name
# to geocode. Verified by hand against the standard list of Spanish
# provincial capitals.
PROVINCE_CAPITAL = {
    "A Coruña": "A Coruña",
    "Álava": "Vitoria-Gasteiz",
    "Albacete": "Albacete",
    "Alicante": "Alicante",
    "Almería": "Almería",
    "Asturias": "Oviedo",
    "Ávila": "Ávila",
    "Badajoz": "Badajoz",
    "Barcelona": "Barcelona",
    "Burgos": "Burgos",
    "Cáceres": "Cáceres",
    "Cádiz": "Cádiz",
    "Cantabria": "Santander",
    "Castellón": "Castellón de la Plana",
    "Ceuta": "Ceuta",
    "Ciudad Real": "Ciudad Real",
    "Córdoba": "Córdoba",
    "Cuenca": "Cuenca",
    "Girona": "Girona",
    "Granada": "Granada",
    "Guadalajara": "Guadalajara",
    "Guipúzcoa": "San Sebastián",
    "Huelva": "Huelva",
    "Huesca": "Huesca",
    "Islas Baleares": "Palma de Mallorca",
    "Jaén": "Jaén",
    "La Rioja": "Logroño",
    "Las Palmas": "Las Palmas de Gran Canaria",
    "León": "León",
    "Lérida": "Lleida",
    "Lugo": "Lugo",
    "Madrid": "Madrid",
    "Málaga": "Málaga",
    "Melilla": "Melilla",
    "Murcia": "Murcia",
    "Navarra": "Pamplona",
    "Ourense": "Ourense",
    "Palencia": "Palencia",
    "Pontevedra": "Pontevedra",
    "Salamanca": "Salamanca",
    "Santa Cruz de Tenerife": "Santa Cruz de Tenerife",
    "Segovia": "Segovia",
    "Sevilla": "Sevilla",
    "Soria": "Soria",
    "Tarragona": "Tarragona",
    "Teruel": "Teruel",
    "Toledo": "Toledo",
    "Valladolid": "Valladolid",
    "Valencia": "Valencia",
    "Vizcaya": "Bilbao",
    "Zamora": "Zamora",
    "Zaragoza": "Zaragoza",
}

# The two Canary Islands provinces run on Atlantic/Canary (one hour behind
# the rest of Spain, which is Europe/Madrid); everyone else gets Europe/Madrid.
CANARY_PROVINCES = {"Las Palmas", "Santa Cruz de Tenerife"}

POPULATED_CODES = {
    "PPL", "PPLA", "PPLA2", "PPLA3", "PPLA4", "PPLA5", "PPLC", "PPLF",
    "PPLG", "PPLL", "PPLR", "PPLS", "PPLX", "PPLQ",
}

GEONAMES_COLS = [
    "geonameid", "name", "asciiname", "alternatenames", "latitude", "longitude",
    "feature_class", "feature_code", "country_code", "cc2", "admin1", "admin2",
    "admin3", "admin4", "population", "elevation", "dem", "timezone", "moddate",
]


def normalize_key(s: str) -> str:
    s = unicodedata.normalize("NFKD", s)
    s = "".join(c for c in s if not unicodedata.combining(c))
    return s.lower().strip()


def load_index():
    index = defaultdict(list)  # normalized name -> [(population, lat, lon, canonical_name, feature_code)]
    try:
        f = open(GEONAMES_PATH, encoding="utf-8")
    except FileNotFoundError:
        print(f"FATAL: {GEONAMES_PATH} not found. Fetch it first:\n"
              f"  mkdir -p data/spain/geonames\n"
              f"  curl -sL https://download.geonames.org/export/dump/ES.zip -o data/spain/geonames/ES.zip\n"
              f"  unzip -o data/spain/geonames/ES.zip -d data/spain/geonames", file=sys.stderr)
        sys.exit(1)
    with f:
        for line in f:
            parts = line.rstrip("\n").split("\t")
            if len(parts) < 15:
                continue
            row = dict(zip(GEONAMES_COLS, parts))
            if row["feature_class"] != "P" or row["feature_code"] not in POPULATED_CODES:
                continue
            try:
                pop = int(row["population"]) if row["population"] else 0
            except ValueError:
                pop = 0
            lat, lon = float(row["latitude"]), float(row["longitude"])
            names = {row["name"], row["asciiname"]}
            if row["alternatenames"]:
                names.update(row["alternatenames"].split(","))
            entry = (pop, lat, lon, row["name"], row["feature_code"])
            for n in names:
                if n:
                    index[normalize_key(n)].append(entry)
    return index


def best_match(index, city_name: str):
    key = normalize_key(city_name)
    candidates = index.get(key)
    if not candidates:
        return None
    return max(candidates, key=lambda e: e[0])  # highest population


def main():
    provinces = sorted({t["provinceName"] for t in json.load(open(PROVINCES_RAW_PATH, encoding="utf-8"))})
    print(f"Geocoding {len(provinces)} provinces from {PROVINCES_RAW_PATH}", file=sys.stderr)

    missing_capital = [p for p in provinces if p not in PROVINCE_CAPITAL]
    if missing_capital:
        print(f"FATAL: no capital-city mapping for: {missing_capital}", file=sys.stderr)
        sys.exit(1)

    print("Loading GeoNames ES.txt...", file=sys.stderr)
    index = load_index()
    print(f"Indexed {len(index)} normalized name keys", file=sys.stderr)

    results = {}
    unmatched = []
    for province in provinces:
        capital = PROVINCE_CAPITAL[province]
        m = best_match(index, capital)
        if m:
            pop, lat, lon, name, code = m
            zone_id = "Atlantic/Canary" if province in CANARY_PROVINCES else "Europe/Madrid"
            results[province] = {
                "capital": capital, "lat": lat, "lon": lon,
                "population": pop, "matched_name": name, "feature_code": code,
                "zoneId": zone_id,
            }
        else:
            unmatched.append((province, capital))

    print(f"Matched: {len(results)}/{len(provinces)}", file=sys.stderr)
    if unmatched:
        print(f"UNMATCHED (need manual fix): {unmatched}", file=sys.stderr)

    json.dump(results, open("data/spain/province-coords.json", "w", encoding="utf-8"),
               ensure_ascii=False, indent=2)
    print("Wrote data/spain/province-coords.json", file=sys.stderr)

    if unmatched:
        sys.exit(1)


if __name__ == "__main__":
    main()
