#!/usr/bin/env python3
"""Add the venues harvest_sources.py found to the roster.

Every row of venue-sources.json that is not rostered yet -- a live Filmstarts venue
the id sweep recovered, or a venue only Cineamo or kinoheld lists -- joins
venues.json when its primary source lists at least MIN_WINDOW_SHOWS shows in the
measured fortnight: fewer is a venue on a break, a one-off or a listing nobody
keeps, and a page for it would sit empty.

  * A Filmstarts venue keeps its `theaterId`. A venue Filmstarts does not list has
    none; its `feed` names it in the source that found it ("cineamo:1239",
    "kinoheld:2417") -- the key its row in venue-sources.json, and so its sources in
    GermanSourcesData, are filed under.
  * Its `city` is the town it is in, read off its coordinates where a source gave
    them: the town its source names, when that town (in city-coords.json, or in the
    GeoNames gazetteer) is within its reach (`reach_km`); else the nearest populated place.
    A town new to city-coords.json is added with its GeoNames point, population and
    Land, which build_pages.py needs.
  * Its `displayName` (the wire key) is its name, qualified with its town when the
    roster already has the name.

Idempotent: a venue already in venues.json (by theaterId or feed) is left alone.

Run:  python3 data/germany/scripts/add_found_venues.py <path to GeoNames DE.txt>
      then build_pages.py, test_build_pages.py, generate_roster.py, generate_sources.py
"""

import json
import math
import os
import re
import sys
import unicodedata
from collections import defaultdict

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import build_pages  # noqa: E402  (its pinned towns and Länder)

HERE = os.path.dirname(os.path.abspath(__file__))
DATA = os.path.dirname(HERE)
MIN_WINDOW_SHOWS = 3


def reach_km(population):
    """How far from its point a town's venues can lie: Berlin's run 25 km out, a small
    town's a few -- Alzenau's cinema is not Aschaffenburg's, 14 km away."""
    return 5 + 4 * math.sqrt(max(population, 0) / 100_000)

POPULATED = {"PPL", "PPLA", "PPLA2", "PPLA3", "PPLA4", "PPLC"}


def fold(s):
    s = (s or "").replace("ß", "ss").replace("ä", "ae").replace("ö", "oe").replace("ü", "ue")
    s = s.replace("Ä", "Ae").replace("Ö", "Oe").replace("Ü", "Ue")
    s = unicodedata.normalize("NFKD", s)
    return re.sub(r"[^a-z0-9]+", " ", "".join(c for c in s if not unicodedata.combining(c)).lower()).strip()


def town_forms(town):
    """"Gießen, Lahn" -> Gießen; "Naumburg (Saale)" -> also Naumburg; "Herrenberg (im Gäu)" -> Herrenberg."""
    forms = [town, re.split(r",", town)[0], re.sub(r"\s*\(.*?\)", "", town)]
    return [f.strip() for f in dict.fromkeys(forms) if f.strip()]


def km(a_lat, a_lon, b_lat, b_lon):
    p = math.pi / 180
    h = (math.sin((b_lat - a_lat) * p / 2) ** 2
         + math.cos(a_lat * p) * math.cos(b_lat * p) * math.sin((b_lon - a_lon) * p / 2) ** 2)
    return 12742 * math.asin(math.sqrt(h))


def unique_name(place, known):
    """A place's name, qualified with its Land when the roster already has a town of that
    name somewhere else: Gronau, Westphalia beside the pinned Gronau (Leine)."""
    other = known.get(place["name"])
    if other and km(place["lat"], place["lon"], other["lat"], other["lon"]) > reach_km(other["population"]):
        return f"{place['name']} ({build_pages.LAENDER[place['admin1']]})"
    return place["name"]


class Gazetteer:
    """GeoNames' German populated places, by folded name."""

    def __init__(self, path):
        self.by_name, self.places = defaultdict(list), []
        with open(path, encoding="utf-8") as f:
            for line in f:
                c = line.rstrip("\n").split("\t")
                if len(c) < 15 or c[6] != "P" or c[7] not in POPULATED:
                    continue
                place = {"name": c[1], "lat": float(c[4]), "lon": float(c[5]),
                         "population": int(c[14] or 0), "admin1": c[10]}
                self.places.append(place)
                for n in {c[1], c[2], *c[3].split(",")}:
                    self.by_name[fold(n)].append(place)

    def named(self, name, near=None):
        """The place called `name` -- nearest to `near` (within its reach), else the most populous."""
        found = self.by_name.get(fold(name), [])
        if near:
            close = [p for p in found if km(near[0], near[1], p["lat"], p["lon"]) <= reach_km(p["population"])]
            return min(close, key=lambda p: km(near[0], near[1], p["lat"], p["lon"])) if close else None
        return max(found, key=lambda p: p["population"]) if found else None

    def nearest(self, lat, lon):
        towns = [p for p in self.places if p["population"] > 0]
        return min(towns, key=lambda p: km(lat, lon, p["lat"], p["lon"]))


# Venues the rules below would place wrong: key -> (the town they are in, its GeoNames name).
TOWN_OF = {
    "cineamo:1239": ("Alzenau", "Alzenau"),          # Cineamo files it under, and at, Aschaffenburg; 63755 is Alzenau
    "cineamo:1386": ("Rerik", "Rerik"),              # the Rerik campsite's cinema; GeoNames' nearest point is Bastorf
    "A0382": ("Gronau (Westf.)", "Gronau"),          # the roster's "Gronau" is Gronau (Leine), pinned in build_pages.py
}


def town_of(row, coords, gazetteer):
    """(town name, its city-coords entry if new) for a found venue. Towns are read the way
    build_pages.py reads them -- city-coords.json with its pins over it -- so a town the
    build pins elsewhere (Gronau is Gronau (Leine) there) is not taken for a namesake."""
    known = {**coords, **build_pages.TOWN_COORDS}
    near = (row["lat"], row["lon"]) if "lat" in row else None
    if row["key"] in TOWN_OF:
        town, geonames_name = TOWN_OF[row["key"]]
        return town, None if town in known else (gazetteer.named(geonames_name, near) or gazetteer.named(geonames_name))
    for form in town_forms(row["town"]):
        point = known.get(form)
        if point and (not near or km(near[0], near[1], point["lat"], point["lon"]) <= reach_km(point["population"])):
            return form, None
    for form in town_forms(row["town"]):
        place = gazetteer.named(form, near)
        if place:
            return unique_name(place, known), place
    if near:
        place = gazetteer.nearest(*near)
        return unique_name(place, known), place
    raise SystemExit(f"cannot place {row['key']} {row['name']} ({row['town']}): no coordinates, no such town")


def main(geonames_path):
    sources = json.load(open(os.path.join(DATA, "venue-sources.json")))
    venues = json.load(open(os.path.join(DATA, "venues.json")))
    coords = json.load(open(os.path.join(DATA, "city-coords.json")))
    gazetteer = Gazetteer(geonames_path)

    present = {v.get("theaterId") or v.get("feed") for v in venues}
    names = {v["displayName"] for v in venues}
    added = []
    for row in sources["venues"]:
        if row.get("rostered") or row["key"] in present or not row.get("primary"):
            continue
        if row["shows"][row["primary"]][0] < MIN_WINDOW_SHOWS:
            continue
        town, place = town_of(row, coords, gazetteer)
        if place and town not in coords:
            coords[town] = {"lat": place["lat"], "lon": place["lon"], "population": place["population"],
                            "admin1": place["admin1"]}
        name = row["name"].strip()
        display = name if name not in names else f"{name} ({town})"
        names.add(display)
        venue = ({"theaterId": row["filmstarts"]} if row.get("filmstarts") else {"feed": row["key"]})
        venue.update({"name": name, "city": town, "displayName": display})
        venues.append(venue)
        added.append(venue)

    with open(os.path.join(DATA, "venues.json"), "w") as f:
        f.write("[\n" + ",\n".join(" " + json.dumps(v, ensure_ascii=False) for v in venues) + "\n]\n")
    with open(os.path.join(DATA, "city-coords.json"), "w") as f:
        json.dump(dict(sorted(coords.items())), f, ensure_ascii=False, indent=1)   # as the file is kept: no final newline
    by_town = defaultdict(list)
    for v in added:
        by_town[v["city"]].append(v["displayName"])
    print(f"added {len(added)} venues ({sum(1 for v in added if 'theaterId' in v)} Filmstarts, "
          f"{sum(1 for v in added if 'feed' in v)} Cineamo/kinoheld-only) in {len(by_town)} towns")
    for town, ns in sorted(by_town.items(), key=lambda t: -len(t[1])):
        print(f"  {town}: {', '.join(ns)}")


if __name__ == "__main__":
    if len(sys.argv) != 2:
        sys.exit(__doc__)
    main(sys.argv[1])
