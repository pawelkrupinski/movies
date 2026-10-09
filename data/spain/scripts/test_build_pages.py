#!/usr/bin/env python3
"""Tests for Spain's build_pages.py on a synthetic map (no GeoNames), and for the
committed pages.json. The clustering rule itself is Poland's too and is tested in
data/pl/scripts/test_build_pages.py; these cover what Spain adds — majors by
population, islands, and where a retired province's URL goes.

    python3 data/spain/scripts/test_build_pages.py
"""
import json
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent))
import build_pages as b  # noqa: E402

COMMUNITIES = {"Sur": "Andalucía", "Las Palmas": "Canarias", "Ceuta": "Ceuta", "Toledo": "Castilla-La Mancha"}
ZONES = {"Sur": "Europe/Madrid", "Las Palmas": "Atlantic/Canary", "Ceuta": "Europe/Madrid", "Toledo": "Europe/Madrid"}


def town(km_north, km_east, pop, province="Sur"):
    return {"lat": 37.0 + km_north / 111.0, "lon": -5.0 + km_east / 89.0, "pop": pop, "province": province}


def run(towns, venues, previous, majors=("Sevilla",), islands=None):
    venue_town = {v: t for t, v in venues}
    saved = b.MAJORS, dict(b.ISLANDS)
    b.MAJORS = list(majors)
    b.ISLANDS.update(islands or {})
    try:
        pages, retired = b.build([v for _, v in venues], venue_town, towns, ZONES, COMMUNITIES, previous, {})
    finally:
        b.MAJORS, b.ISLANDS = saved
    return {p["slug"]: p for p in pages}, retired


def check(name, cond):
    print(("PASS " if cond else "FAIL ") + name)
    if not cond:
        sys.exit(1)


def province(cinemas, km_north, km_east, community_slug):
    return {"cinemas": cinemas, "kind": "province", "lat": 37.0 + km_north / 111.0, "lon": -5.0 + km_east / 89.0,
            "qualifiedSlug": community_slug}


def synthetic():
    towns = {
        "Sevilla": town(0, 0, 700000),
        "Dos Hermanas": town(-12, 0, 130000),  # 3 venues → its own page, though next to a major
        "Camas": town(4, 0, 27000),            # 1 venue, 4 km from Sevilla: majors absorb nothing
        "Tomares": town(6, 0, 25000),
        "Algeciras": town(-200, 0, 120000, "Sur"),
        "Ceuta": town(-228, 0, 85000, "Ceuta"),  # 28 km across the Strait
        "Arrecife": town(-900, 0, 60000, "Las Palmas"),
        "Puerto del Rosario": town(-911, 0, 40000, "Las Palmas"),  # 11 km of sea away
    }
    venues = [("Sevilla", "S1"), ("Dos Hermanas", "D1"), ("Dos Hermanas", "D2"), ("Dos Hermanas", "D3"),
              ("Camas", "C1"), ("Tomares", "T1"), ("Algeciras", "A1"), ("Algeciras", "A2"), ("Ceuta", "CE1"),
              ("Arrecife", "L1"), ("Puerto del Rosario", "F1")]
    previous = {"sevilla": province(["S1", "D1", "D2", "D3", "C1", "T1"], 0, 0, "sevilla-andalucia"),
                "cadiz": province(["A1", "A2", "CE1"], -150, 0, "cadiz-andalucia"),
                "las-palmas": province(["L1", "F1"], -905, 0, "las-palmas-canarias")}
    islands = {"Arrecife": "Lanzarote", "Puerto del Rosario": "Fuerteventura"}
    pages, retired = run(towns, venues, previous, islands=islands)

    check("a major city keeps only its own venues", pages["sevilla"]["kind"] == "major"
          and pages["sevilla"]["cinemas"] == ["S1"])
    check("a 3-venue town next to a major is a page of its own", pages["dos-hermanas"]["kind"] == "town")
    check("towns by a major cluster among themselves", pages["camas"]["towns"] == ["Camas", "Tomares"])
    check("a town across the sea does not join a cluster 28 km away",
          pages["ceuta"]["towns"] == ["Ceuta"] and "Ceuta" not in pages["algeciras"]["towns"])
    check("two islands 11 km apart stay two pages",
          pages["arrecife"]["towns"] == ["Arrecife"] and pages["puerto-del-rosario"]["towns"] == ["Puerto del Rosario"])
    check("a page carries its anchor's province, community and zone",
          (pages["arrecife"]["province"], pages["arrecife"]["community"], pages["arrecife"]["zoneId"])
          == ("Las Palmas", "Canarias", "Atlantic/Canary"))
    check("a province whose slug is still a page is not retired", "sevilla" not in retired)
    check("a retired province goes to the page holding most of its venues", retired["cadiz"]["page"] == "algeciras")
    check("a tie goes to the page nearest the province's old centre",
          retired["las-palmas"]["page"] == "arrecife")
    check("a retired slug keeps the slug it was qualified to on a collision",
          retired["cadiz"]["qualifiedSlug"] == "cadiz-andalucia")
    check("every venue is on exactly one page",
          sorted(c for p in pages.values() for c in p["cinemas"]) == sorted(v for _, v in venues))

    try:
        run({"Isla": town(-900, 0, 1, "Las Palmas")}, [("Isla", "I1")], {})
        refused = False
    except SystemExit:
        refused = True
    check("a town in an island province must be placed on an island", refused)


def committed():
    data = json.loads((b.DATA / "pages.json").read_text())
    pages = {p["slug"]: p for p in data["pages"]}
    venues = [c for p in data["pages"] for c in p["cinemas"]]
    check("pages.json: every venue on exactly one page", len(venues) == len(set(venues)) == 602)
    check("pages.json: every province slug is a page or redirects to one",
          all(p["slug"] in pages or data["retired"][p["slug"]]["page"] in pages
              for p in json.loads((b.DATA / "provinces.json").read_text())))
    check("pages.json: Barcelona keeps only its own venues", pages["barcelona"]["towns"] == ["Barcelona"])


def main():
    synthetic()
    committed()
    print("all passed")


if __name__ == "__main__":
    main()
