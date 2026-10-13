#!/usr/bin/env python3
"""Tests for geocode_towns.py — which town each UK venue is in — on a synthetic
GeoNames extract, and for the committed towns.json.

    python3 data/uk/scripts/test_geocode_towns.py
"""
import json
import sys
import tempfile
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent))
import geocode_towns as g  # noqa: E402


def row(gid, name, lat, lon, country, admin1, admin2, pop):
    """One cities1000.txt line: 19 tab-separated GeoNames fields."""
    f = [""] * 19
    f[0], f[1], f[2], f[4], f[5], f[8], f[10], f[11], f[14] = (
        str(gid), name, name, str(lat), str(lon), country, admin1, admin2, str(pop))
    return "\t".join(f)


GAZETTEER = [
    row(1, "London", 51.50853, -0.12574, "GB", "ENG", "GLA", 8961989),
    row(2, "Croydon", 51.38333, -0.1, "GB", "ENG", "GLA", 173314),
    row(3, "Kilburn", 51.5333, -0.2, "GB", "ENG", "GLA", 29027),
    row(4, "Newport", 51.58774, -2.99835, "GB", "WLS", "Y6", 161506),
    row(5, "Newport", 50.70146, -1.29124, "GB", "ENG", "G2", 24884),
    row(6, "Kingston upon Hull", 53.7446, -0.33525, "GB", "ENG", "G6", 314018),
    row(7, "Glasgow", 55.86515, -4.25763, "GB", "SCT", "V2", 626410),
    row(8, "Cumbernauld", 55.94685, -3.99051, "GB", "SCT", "V8", 52270),
    row(9, "Rothesay", 55.83599, -5.05506, "GB", "SCT", "T8", 4800),
    row(10, "Saint Helier", 49.18804, -2.10491, "JE", "", "", 28000),
    row(11, "Belfast", 54.59682, -5.92541, "GB", "NIR", "N09000003", 348005),
    row(12, "Chilwell", 52.9167, -1.2333, "GB", "ENG", "J9", 12864),
]


def venue(obj, town, lat, lon):
    return {"cinemaObject": obj, "town": town, "lat": lat, "lon": lon}


def check(name, cond):
    print(("PASS " if cond else "FAIL ") + name)
    if not cond:
        sys.exit(1)


def refuses(f):
    try:
        f()
    except SystemExit:
        return True
    return False


def synthetic():
    with tempfile.NamedTemporaryFile("w", suffix=".txt", delete=False) as f:
        f.write("\n".join(GAZETTEER) + "\n")
    gaz = g.load_gazetteer(f.name)
    # A reviewed figure for a town GeoNames DOES have is not a town it lacks.
    # Restored whatever happens, so a failing place() leaves the table as it found it.
    saved_populations = dict(g.POPULATIONS)
    g.POPULATIONS["Hull, East Riding of Yorkshire"] = 1
    try:
        towns, town_of, lacking = g.place([
            venue("Croydon1", "Croydon", 51.375, -0.098),
            venue("Hampstead1", "Hampstead", 51.556, -0.178),
            venue("NewportIow", "Newport", 50.699, -1.289),
            venue("NewportGwent", "Newport", 51.588, -2.995),
            venue("Hull1", "Hull", 53.74, -0.33),
            venue("Lanternhouse", "Glasgow", 55.9488, -3.97109),
            venue("Rothesay1", "Rothesay", 55.838, -5.055),
            venue("Jersey1", "Jersey", 49.1838, -2.1143),
            venue("Belfast1", "Belfast", 54.6, -5.93),
            venue("Beeston1", "Beeston", 52.926, -1.215),
        ], gaz, venue_towns={"Hampstead1": "London", "Jersey1": "Saint Helier"})
    finally:
        g.POPULATIONS.clear()
        g.POPULATIONS.update(saved_populations)
    check("every place in Greater London is London — a borough's postal town, and a neighbourhood the "
          "reviewed VENUE_TOWNS table names", town_of["Croydon1"] == town_of["Hampstead1"] == "London, Greater London")
    check("two towns of one name are two towns, keyed by their county",
          town_of["NewportIow"] == "Newport, Isle of Wight" and town_of["NewportGwent"] == "Newport, Gwent")
    check("a reviewed alias: Flicks' Hull is GeoNames' Kingston upon Hull, shown as Hull",
          town_of["Hull1"] == "Hull, East Riding of Yorkshire" and towns["Hull, East Riding of Yorkshire"]["population"])
    check("a postal town reaching into another council's area: Cumbernauld's 'Glasgow' venue is in Cumbernauld",
          town_of["Lanternhouse"] == "Cumbernauld, Lanarkshire")
    check("a town GeoNames lacks is its own, centred on its venues, with its reviewed population",
          towns[town_of["Beeston1"]]["population"] == g.POPULATIONS["Beeston, Nottinghamshire"]
          and towns[town_of["Beeston1"]]["name"] == "Beeston")
    check("the towns GeoNames lacks are only those it gave no population, whatever the table holds",
          lacking == ["Beeston, Nottinghamshire"]
          and towns["Hull, East Riding of Yorkshire"]["population"] > 1)
    check("a town with no population anywhere stops the run",
          refuses(lambda: g.place([venue("Boldon1", "Boldon", 55.87, -4.26)], gaz)))
    check("an island is its own landmass: Bute, Jersey, the Isle of Wight",
          towns[town_of["Rothesay1"]]["landmass"] == "Isle of Bute"
          and towns[town_of["Jersey1"]]["landmass"] == "Jersey"
          and towns[town_of["NewportIow"]]["landmass"] == "Isle of Wight")
    check("Northern Ireland is Ireland's landmass, filed under its nation",
          towns[town_of["Belfast1"]]["landmass"] == "Ireland"
          and towns[town_of["Belfast1"]]["nation"] == "Northern Ireland")
    check("a Crown Dependency is its own nation heading, and 'Saint' is written 'St'",
          town_of["Jersey1"] == "St Helier, Jersey" and towns["St Helier, Jersey"]["nation"] == "Crown Dependencies")
    check("a venue whose town matches nothing near it stops the run",
          refuses(lambda: g.place([venue("Lost1", "Atlantis", 57.0, -9.0)], gaz)))
    # The case that used to be guessed at: a misspelt town right beside a real
    # place was put in that place, silently.
    check("a name matching nothing stops the run even with a place 2 km away",
          refuses(lambda: g.place([venue("Typo1", "Lodnon", 51.53, -0.19)], gaz, venue_towns={})))
    check("a neighbourhood not in VENUE_TOWNS stops the run too",
          refuses(lambda: g.place([venue("Hampstead1", "Hampstead", 51.556, -0.178)], gaz, venue_towns={})))


def committed():
    data = json.loads((g.DATA / "towns.json").read_text())
    venues = json.loads((g.DATA / "venues.json").read_text())
    check("towns.json: every venue in venues.json has a town",
          {v["cinemaObject"] for v in venues} == set(data["venues"]))
    check("towns.json: every town a venue names is listed", set(data["venues"].values()) <= set(data["towns"]))
    far = [(o, k) for o, k in data["venues"].items()
           for v in venues if v["cinemaObject"] == o and k != "London, Greater London"
           and o not in g.MISPLACED
           and g.town_pages.km((v["lat"], v["lon"]), (data["towns"][k]["lat"], data["towns"][k]["lon"])) > 25]
    check(f"towns.json: every venue outside London is within 25 km of its town {far}", not far)
    for obj, town in sorted(g.VENUE_TOWNS.items()):
        check(f"towns.json: {obj} is in {town}, as VENUE_TOWNS says",
              data["towns"][data["venues"][obj]]["name"] == g.display(town))
    check("towns.json: no town is a county — the harvest's 'East Riding of Yorkshire' is Goole",
          data["venues"]["JunctionGoole"] == "Goole, East Riding of Yorkshire")


def main():
    synthetic()
    committed()
    print("all passed")


if __name__ == "__main__":
    main()
