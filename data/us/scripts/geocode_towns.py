#!/usr/bin/env python3
"""Write data/us/towns.json — the town each US venue is in, and where that town is.

The page rule (`build_pages.py`) works on TOWNS, not venues: a town's population
decides whether it is a major city, its venue count whether it is a page of its
own, and its centre is what every distance is measured from. Flicks gives each
venue a `city` string and coordinates; this matches it to a GeoNames place
(`cities1000.txt`, every place of 1,000+ people):

  1. the venue's state's place of the same (normalised) name within 30 km of the
     venue, the most populous where several are — "Springfield" is a dozen towns;
  2. failing that, the state's place nearest the venue within 6 km — a venue
     Flicks files under a neighbourhood or a mall's postal town;
  3. failing that, the venue's own `city`, centred on its venues and with no
     population — a place too small for the gazetteer.

New York's boroughs and Los Angeles' neighbourhoods are the city they are in
(`ALIASES`): Brooklyn's cinemas are on the New York page, not on a "Brooklyn" page
beside it.

Committed, so the build needs nothing but the repository; re-run after a
re-harvest adds venues (`build_pages.py` refuses a venue missing here):

    curl -sO https://download.geonames.org/export/dump/cities1000.zip && unzip cities1000.zip
    python3 data/us/scripts/geocode_towns.py cities1000.txt
"""
import collections
import json
import pathlib
import re
import sys
from decimal import ROUND_HALF_EVEN, Decimal

ROOT = pathlib.Path(__file__).resolve().parents[3]
DATA = ROOT / "data" / "us"
OUT = DATA / "towns.json"
sys.path.insert(0, str(ROOT / "data" / "scripts"))
import town_pages  # noqa: E402

# GeoNames files the territories as countries of their own, and the states by
# their postal code in admin1.
POSTAL = {
    "AL": "Alabama", "AK": "Alaska", "AZ": "Arizona", "AR": "Arkansas", "CA": "California", "CO": "Colorado",
    "CT": "Connecticut", "DE": "Delaware", "DC": "District of Columbia", "FL": "Florida", "GA": "Georgia",
    "HI": "Hawaii", "ID": "Idaho", "IL": "Illinois", "IN": "Indiana", "IA": "Iowa", "KS": "Kansas",
    "KY": "Kentucky", "LA": "Louisiana", "ME": "Maine", "MD": "Maryland", "MA": "Massachusetts",
    "MI": "Michigan", "MN": "Minnesota", "MS": "Mississippi", "MO": "Missouri", "MT": "Montana",
    "NE": "Nebraska", "NV": "Nevada", "NH": "New Hampshire", "NJ": "New Jersey", "NM": "New Mexico",
    "NY": "New York", "NC": "North Carolina", "ND": "North Dakota", "OH": "Ohio", "OK": "Oklahoma",
    "OR": "Oregon", "PA": "Pennsylvania", "RI": "Rhode Island", "SC": "South Carolina", "SD": "South Dakota",
    "TN": "Tennessee", "TX": "Texas", "UT": "Utah", "VT": "Vermont", "VA": "Virginia", "WA": "Washington",
    "WV": "West Virginia", "WI": "Wisconsin", "WY": "Wyoming",
}
TERRITORIES = {"PR": "Puerto Rico", "GU": "Guam", "VI": "Virgin Islands", "AS": "American Samoa"}

NAME_MATCH_KM = 30
# A city of this many people sprawls past `NAME_MATCH_KM`: Houston's own cinemas
# reach 33 km from its GeoNames centre.
BIG_CITY_POPULATION = 500_000
BIG_CITY_MATCH_KM = 50
NEAREST_KM = 6
# A venue whose own state's town of its name is further than this, and which sits
# beside a town of that name in ANOTHER state, carries that other town's
# coordinates: Grand Theater Madison (Minnesota) was harvested at Madison, Wisconsin.
NAMESAKE_FAR_KM = 100

NYC = ["new york", "brooklyn", "bronx", "the bronx", "queens", "manhattan", "staten island", "flushing",
       "jamaica", "astoria", "long island city", "forest hills"]
LA = ["hollywood", "sherman oaks", "encino", "woodland hills", "van nuys", "north hollywood", "studio city",
      "canoga park", "northridge", "reseda", "west hills", "tarzana", "sylmar", "chatsworth", "granada hills",
      "pacific palisades", "san pedro", "wilmington", "west los angeles", "porter ranch", "panorama city",
      "sun valley", "century city"]
#: (normalised Flicks city, state) → the normalised GeoNames town it is part of.
ALIASES = {**{(n, "New York"): "new york city" for n in NYC},
           **{(n, "California"): "los angeles" for n in LA}}
#: GeoNames' admin2 codes of New York City's five boroughs. Every place in them is
#: New York City, whatever Flicks calls it — Bayside, Kew Gardens and Ridgewood are
#: Queens, not towns of their own.
NYC_COUNTIES = {"005", "047", "061", "081", "085"}


def norm(name):
    s = town_pages.fold(name or "")
    s = re.sub(r"\bsaint\b", "st", s)
    s = re.sub(r"\bmt\b", "mount", s)
    s = re.sub(r"\bft\b", "fort", s)
    return re.sub(r"[^a-z0-9]+", " ", s.replace("st.", "st")).strip()


def match_km(town):
    return BIG_CITY_MATCH_KM if town[4] >= BIG_CITY_POPULATION else NAME_MATCH_KM


def namesake_coordinates(venue, gazetteer):
    """The other state's town a venue's coordinates were taken from, when they
    were: none of its own state's towns of its name is near it, and a town of
    that name in another state is right beside it."""
    at, city = (float(venue["lat"]), float(venue["lon"])), norm(venue.get("city"))
    own = [g for g in gazetteer[venue["state"]] if g[0] == city]
    if own and min(town_pages.km(at, g[2:4]) for g in own) <= NAMESAKE_FAR_KM:
        return None
    return next((f"{g[1]}, {state}" for state, gs in sorted(gazetteer.items()) if state != venue["state"]
                 for g in gs if g[0] == city and town_pages.km(at, g[2:4]) <= NEAREST_KM), None)


def load_gazetteer(path):
    """state → [(normalised name, name, lat, lon, population)], a place inside New
    York City's boroughs carrying the city's own name, centre and population."""
    by_state = collections.defaultdict(list)
    boroughs = []
    with open(path, encoding="utf-8") as f:
        for line in f:
            p = line.rstrip("\n").split("\t")
            state = POSTAL.get(p[10]) if p[8] == "US" else TERRITORIES.get(p[8])
            if state:
                place = (norm(p[1]), p[1], float(p[4]), float(p[5]), int(p[14]))
                (boroughs if state == "New York" and p[11] in NYC_COUNTIES else by_state[state]).append(place)
    nyc = next(g for g in by_state["New York"] if g[1] == "New York City")
    by_state["New York"].extend((g[0], *nyc[1:]) for g in boroughs)
    return by_state


def exact_mean(values):
    """Mean of decimal strings, rounded half-even to five places — the same answer
    on every Python (see generate_roster.mean_coordinate)."""
    values = [Decimal(str(v)) for v in values]
    return float((sum(values, Decimal(0)) / len(values)).quantize(Decimal("0.00001"), rounding=ROUND_HALF_EVEN))


def place(venues, gazetteer):
    """(towns, venue slug → town key) for every venue with a state and coordinates.
    Refuses a venue harvested at a namesake town's coordinates, and venues too far
    from their town's gazetteer entry to be matched to it — the fix for both is the
    venue's coordinates in venues.json."""
    towns, town_of, own, problems = {}, {}, collections.defaultdict(list), []
    for v in sorted(venues, key=lambda v: v["slug"]):
        state = v.get("state")
        if state not in gazetteer or not v.get("lat"):
            continue
        namesake = namesake_coordinates(v, gazetteer)
        if namesake:
            problems.append(f"{v['slug']!r} ({v['city']}, {state}) sits at {namesake} — fix its coordinates")
        at = (float(v["lat"]), float(v["lon"]))
        city = norm(v.get("city"))
        city = ALIASES.get((city, state), city)
        named = [g for g in gazetteer[state] if g[0] == city and town_pages.km(at, g[2:4]) <= match_km(g)]
        if named:
            g = max(named, key=lambda g: (g[4], g[1]))
        else:
            near = [(town_pages.km(at, g[2:4]), g) for g in gazetteer[state]]
            near = [x for x in near if x[0] <= NEAREST_KM]
            g = min(near, key=lambda x: (x[0], x[1][1]))[1] if near else None
        if g:
            key = f"{g[1]}, {state}"
            towns[key] = {"name": g[1], "state": state, "lat": g[2], "lon": g[3], "population": g[4]}
        else:
            key = f"{v['city'].strip()}, {state}"
            own[key].append(v)
        town_of[v["slug"]] = key
    for key, vs in own.items():
        if key in towns:
            problems.append(f"{', '.join(v['slug'] for v in vs)}: too far from {key}'s gazetteer centre to be "
                            f"matched to it — check their coordinates")
            continue
        towns[key] = {"name": vs[0]["city"].strip(), "state": vs[0]["state"],
                      "lat": exact_mean(v["lat"] for v in vs), "lon": exact_mean(v["lon"] for v in vs),
                      "population": 0}
    if problems:
        sys.exit("\n".join(problems))
    return towns, town_of


def main(gazetteer_path):
    venues = json.loads((DATA / "venues.json").read_text())
    towns, town_of = place(venues, load_gazetteer(gazetteer_path))
    OUT.write_text(json.dumps({"towns": dict(sorted(towns.items())), "venues": dict(sorted(town_of.items()))},
                              ensure_ascii=False, indent=1) + "\n", encoding="utf-8")
    unmatched = sum(1 for t in towns.values() if not t["population"])
    print(f"Wrote {OUT.relative_to(ROOT)}: {len(town_of)} venues in {len(towns)} towns "
          f"({unmatched} not in the gazetteer, centred on their venues)")


if __name__ == "__main__":
    main(sys.argv[1])
