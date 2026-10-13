#!/usr/bin/env python3
"""Write data/uk/towns.json — the town each UK venue is in, and where that town is.

The page rule (`build_pages.py`) works on TOWNS, not venues: a town's population
decides whether it is a major city, its venue count whether it is a page of its
own, and its centre is what every distance is measured from. `venues.json` gives
each venue the postal town Flicks' address names (`harvest_towns.py`) and its
coordinates; this matches that to a GeoNames place (`cities1000.txt`, every place
of 1,000+ people, in Great Britain, Northern Ireland, the Isle of Man, Jersey and
Guernsey):

  1. a reviewed name for the venue itself (`VENUE_TOWNS`: Flicks files the Everyman
     Hampstead under "Hampstead", which is London, and Hay-on-Wye's bookshop under
     its postal town, Hereford, 30 km away), else a reviewed spelling of its town's
     name (`ALIASES`: Flicks writes "Hull", GeoNames "Kingston upon Hull");
  2. the place of that (normalised) name within 25 km of the venue, the most
     populous where several are;
  3. a town GeoNames does not list (`OWN_TOWNS`: Beeston, Cromarty), centred on
     its venues and with no population;
  4. failing all three, nothing: the run stops and names the venue, so a new
     spelling is reviewed into one of the tables above rather than guessed at —
     "Yrok" is not put in whatever village happens to be nearest.

A town is keyed by its name AND its county ("Newport, Isle of Wight" and
"Newport, Newport" are two towns 102 km apart), and every place in Greater London
is London — its boroughs' postal towns (Croydon, Romford, Kingston upon Thames) are
the city they are in, the way New York's boroughs are New York City.

Each town also carries its NATION and its picker COUNTY, read off GeoNames'
admin codes: England's ceremonial counties, Wales' preserved counties, Scotland's
and Northern Ireland's council areas grouped as `COUNTIES` says — and its
LANDMASS, the island it is on, which no cluster crosses.

Committed, so the build needs nothing but the repository; re-run after a
re-harvest adds venues (`build_pages.py` refuses a venue missing here):

    curl -sO https://download.geonames.org/export/dump/cities1000.zip && unzip cities1000.zip
    python3 data/uk/scripts/geocode_towns.py cities1000.txt
"""
import collections
import json
import pathlib
import re
import sys

ROOT = pathlib.Path(__file__).resolve().parents[3]
DATA = ROOT / "data" / "uk"
OUT = DATA / "towns.json"
sys.path.insert(0, str(ROOT / "data" / "scripts"))
import town_pages  # noqa: E402

COUNTRIES = {"GB", "IM", "JE", "GG"}
NATIONS = {"ENG": "England", "SCT": "Scotland", "WLS": "Wales", "NIR": "Northern Ireland"}
CROWN_DEPENDENCIES = {"IM": "Isle of Man", "JE": "Jersey", "GG": "Guernsey"}

NAME_MATCH_KM = 25
# See `place`: a venue further than this from the town its address names, with a
# place of another council area within `BESIDE_KM`, is in that place instead.
POSTAL_REACH_KM = 8
BESIDE_KM = 3
AREA_TOWN_KM = 10

#: GeoNames' admin2 code (within admin1) → the county the picker files a page
#: under. England's are its CEREMONIAL counties (a unitary authority such as
#: "Borough of Bolton" is Greater Manchester), Wales' its preserved counties (the
#: three Glamorgans as one), Scotland's its council areas grouped the way the
#: Flicks regions grouped them, Northern Ireland's its councils.
COUNTIES = {
    "ENG": {
        "A3": "South Yorkshire", "A4": "Somerset", "A7": "West Midlands", "A8": "Lancashire", "A9": "Lancashire",
        "AA": "Dorset", "B1": "Greater Manchester", "B3": "Berkshire", "B4": "West Yorkshire",
        "B6": "East Sussex", "B7": "Bristol", "B9": "Buckinghamshire", "C1": "Greater Manchester",
        "C2": "West Yorkshire", "C3": "Cambridgeshire", "C6": "Cornwall", "C7": "West Midlands",
        "C9": "Cumbria", "D1": "County Durham", "D2": "Derbyshire", "D3": "Derbyshire", "D4": "Devon",
        "D5": "South Yorkshire", "D6": "Dorset", "D7": "West Midlands", "D8": "County Durham",
        "E1": "East Riding of Yorkshire", "E2": "East Sussex", "E4": "Essex", "E5": "Tyne and Wear",
        "E6": "Gloucestershire", "E9": "Cheshire", "F2": "Hampshire", "F5": "County Durham",
        "F7": "Herefordshire", "F8": "Hertfordshire", "G2": "Isle of Wight", "G5": "Kent",
        "G6": "East Riding of Yorkshire", "G8": "West Yorkshire", "G9": "Merseyside", "GLA": "Greater London",
        "H2": "Lancashire", "H3": "West Yorkshire", "H4": "Leicestershire", "H5": "Leicestershire",
        "H7": "Lincolnshire", "H8": "Merseyside", "I1": "Bedfordshire", "I2": "Greater Manchester",
        "I3": "Kent", "I5": "North Yorkshire", "I6": "Buckinghamshire", "I7": "Tyne and Wear", "I9": "Norfolk",
        "J2": "Lincolnshire", "J3": "Lincolnshire", "J4": "Somerset", "J5": "Tyne and Wear",
        "J6": "Northumberland", "J7": "North Yorkshire", "J8": "Nottinghamshire", "J9": "Nottinghamshire",
        "K1": "Greater Manchester", "K2": "Oxfordshire", "K3": "Cambridgeshire", "K4": "Devon",
        "K6": "Hampshire", "K7": "Berkshire", "K9": "North Yorkshire", "L2": "Greater Manchester",
        "L3": "South Yorkshire", "L4": "Rutland", "L5": "Greater Manchester", "L6": "Shropshire",
        "L7": "West Midlands", "L8": "Merseyside", "L9": "South Yorkshire", "M1": "Berkshire",
        "M2": "West Midlands", "M3": "Somerset", "M4": "Hampshire", "M5": "Essex", "M6": "Gloucestershire",
        "M7": "Tyne and Wear", "M9": "Staffordshire", "N1": "Merseyside", "N2": "Greater Manchester",
        "N3": "County Durham", "N4": "Staffordshire", "N5": "Suffolk", "N6": "Tyne and Wear", "N7": "Surrey",
        "N9": "Wiltshire", "NNH": "Northamptonshire", "O1": "Greater Manchester", "O2": "Shropshire",
        "O3": "Essex", "O4": "Devon", "O6": "Greater Manchester", "O7": "West Yorkshire",
        "O8": "West Midlands", "P2": "Cheshire", "P3": "Warwickshire", "P4": "Berkshire", "P6": "West Sussex",
        "P7": "Greater Manchester", "P8": "Wiltshire", "P9": "Berkshire", "Q1": "Merseyside", "Q2": "Berkshire",
        "Q3": "West Midlands", "Q4": "Worcestershire", "Q5": "North Yorkshire", "WNH": "Northamptonshire",
        "Z5": "Bedfordshire", "Z6": "Bedfordshire", "Z7": "Cheshire", "Z8": "Cheshire", "Z9": "Cornwall",
    },
    "WLS": {
        "X1": "Gwynedd", "X2": "Gwent", "X3": "Glamorgan", "X4": "Glamorgan", "X5": "Glamorgan",
        "X6": "Dyfed", "X7": "Dyfed", "X8": "Clwyd", "X9": "Clwyd", "Y1": "Clwyd", "Y2": "Gwynedd",
        "Y3": "Glamorgan", "Y4": "Gwent", "Y5": "Glamorgan", "Y6": "Gwent", "Y7": "Dyfed", "Y8": "Powys",
        "Y9": "Glamorgan", "Z1": "Glamorgan", "Z2": "Gwent", "Z3": "Glamorgan", "Z4": "Clwyd",
    },
    "SCT": {
        "T5": "Aberdeenshire", "T6": "Aberdeenshire", "T7": "Tayside", "T8": "Argyll and Bute",
        "T9": "Scottish Borders", "U1": "Central Scotland", "U2": "Dumfries and Galloway", "U3": "Tayside",
        "U4": "Ayrshire", "U5": "Dunbartonshire", "U6": "Lothian", "U7": "Renfrewshire", "U8": "Lothian",
        "U9": "Central Scotland", "V1": "Fife", "V2": "Glasgow", "V3": "Highland", "V4": "Renfrewshire",
        "V5": "Lothian", "V6": "Moray", "V7": "Ayrshire", "V8": "Lanarkshire", "V9": "Orkney",
        "W1": "Tayside", "W2": "Renfrewshire", "W3": "Shetland", "W4": "Ayrshire", "W5": "Lanarkshire",
        "W6": "Central Scotland", "W7": "Dunbartonshire", "W8": "Western Isles", "W9": "Lothian",
    },
    "NIR": {
        "N09000001": "Antrim and Newtownabbey", "N09000002": "Armagh, Banbridge and Craigavon",
        "N09000003": "Belfast", "N09000004": "Causeway Coast and Glens", "N09000005": "Derry and Strabane",
        "N09000006": "Fermanagh and Omagh", "N09000007": "Lisburn and Castlereagh",
        "N09000008": "Mid and East Antrim", "N09000009": "Mid Ulster", "N09000010": "Newry, Mourne and Down",
        "N09000011": "Ards and North Down",
    },
}

#: Islands no cluster crosses to — a ferry is not a short drive, however short
#: the line: Ryde is 11 km from Portsmouth across the Solent, Rothesay 9 km from
#: Wemyss Bay. Most are their own GeoNames county; the Clyde's and the Inner
#: Hebrides' are not, so they are boxed: (south, north, west, east).
ISLAND_COUNTIES = {"Isle of Wight", "Orkney", "Shetland", "Western Isles"}
ISLAND_BOXES = {
    "Isle of Bute": (55.72, 55.94, -5.20, -4.98),
    "Isle of Arran": (55.42, 55.72, -5.45, -5.05),
    "Great Cumbrae": (55.74, 55.80, -4.96, -4.89),
    "Isle of Mull": (56.27, 56.66, -6.50, -5.63),
    "Islay": (55.57, 55.95, -6.53, -6.00),
}

#: (normalised harvested name) → the GeoNames name of the place it is: a spelling
#: GeoNames writes differently, or a part of a place Flicks names on its own.
#: Reviewed one by one; a venue whose name matches nothing stops the run instead.
ALIASES = {
    "hull": "Kingston upon Hull",
    "newcastle": "Newcastle upon Tyne",
    "burton on trent": "Burton upon Trent",
    "newark": "Newark on Trent",
    "leamington spa": "Royal Leamington Spa",
    "tunbridge wells": "Royal Tunbridge Wells",
    "letchworth": "Letchworth Garden City",
    "dereham": "East Dereham",
    "staines upon thames": "Staines",
    "torrington": "Great Torrington",
    "malvern": "Great Malvern",
    "londonderry": "Derry",
    "ellesmere port": "Ellesmere Port Town",
    "lynton lynmouth": "Lynton",
    "caenarfon": "Caernarfon",  # Flicks' misspelling
    "bowness on windemere": "Windermere",  # Bowness is part of Windermere; Flicks misspells it too
    "bishops": "Bishops Stortford",  # the address is cut short: "2 Anchor Street, Bishops CM23 3BP"
    "albion street": "Leeds",  # "Unit 4.15 Trinity Leeds, Albion Street LS1 5AY"
    "thanet": "Broadstairs",  # Westwood Cross is Thanet district's, in Broadstairs (CT10)
    "braehead": "Renfrew",  # the shopping centre on Renfrew's edge
    "heaton moor": "Stockport",  # a suburb of Stockport
    "glengormley": "Newtownabbey",  # the centre of Newtownabbey
    "newlyn": "Penzance",  # Newlyn runs into Penzance
}

#: cinemaObject → the GeoNames name of the place a venue is in, where neither the
#: town its address names nor an alias of that town will do. Reviewed one by one.
VENUE_TOWNS = {
    "EverymanCinemaHampstead": "London",       # "Hampstead": a London neighbourhood
    "EverymanCinemaMaidaVale": "London",       # "Maida Vale": likewise
    "ReelCinemaQuinton": "Halesowen",          # "Quinton": the B62 side of the M5, Halesowen's
    "RichardBoothSBookshopHayOnWye": "Hay",    # "Hereford", its postal town 30 km away
    "BeauSejourLeisureCentreGuernsey": "Saint Peter Port",   # "Guernsey": it is in St Peter Port
    "TheMallardCinemaGuernsey": "Forest Parish",             # "Guernsey": by the airport, in Forest
    "CineworldStHelierJersey": "Saint Helier",               # "Jersey"
}

#: Venues whose harvested coordinates are not where the venue is, so the town
#: their address names is trusted over them: Odeon Rochdale (Sandbrook Park,
#: Rochdale) was harvested at Bolton's coordinates, beside a "BL1" postcode.
MISPLACED = {"OdeonCinemaRochdale"}

#: Towns GeoNames' cities1000 does not list (or lists far from the town), each a
#: real town with a cinema: centred on its venues, with no population. Keyed by
#: the normalised harvested name; the value is the name the page shows.
OWN_TOWNS = {
    "beeston": "Beeston",  # Nottinghamshire; GeoNames' only Beeston is a Cheshire hamlet
    "boldon": "Boldon",
    "cromarty": "Cromarty",
    "crosby": "Crosby",  # Merseyside
    "pontardawe": "Pontardawe",
    "wellington": "Wellington",  # Somerset; GeoNames lists only Shropshire's and Herefordshire's
    "trefforest industrial estate": "Nantgarw",  # Showcase Cardiff, Parc Nantgarw
}

#: GeoNames' name → the one a visitor knows the town by, and so its page's name.
DISPLAY_NAMES = {
    "Kingston upon Hull": "Hull",
    "Royal Leamington Spa": "Leamington Spa",
    "Royal Tunbridge Wells": "Tunbridge Wells",
    "East Dereham": "Dereham",
    "Great Malvern": "Malvern",
    "Staines": "Staines-upon-Thames",
    "Ellesmere Port Town": "Ellesmere Port",
    "Hay": "Hay-on-Wye",
    "Forest Parish": "Forest",
    # Hyphenated the way the towns write themselves (and Flicks writes them).
    "Westgate on Sea": "Westgate-on-Sea",
    "Ross on Wye": "Ross-on-Wye",
    "Barton under Needwood": "Barton-under-Needwood",
    "Barrow in Furness": "Barrow-in-Furness",
    "Newcastle under Lyme": "Newcastle-under-Lyme",
    "Newark on Trent": "Newark-on-Trent",
    "Blaenau-Ffestiniog": "Blaenau Ffestiniog",
    "Bo’ness": "Bo'ness",
    "Berwick-Upon-Tweed": "Berwick-upon-Tweed",
    "Bishops Stortford": "Bishop's Stortford",
    # Every venue there, and the page it was on, says Londonderry.
    "Derry": "Londonderry",
}


def norm(name):
    s = town_pages.fold(name or "").replace("&", " ")
    s = re.sub(r"\bsaint\b", "st", s.replace("st.", "st"))
    return re.sub(r"[^a-z0-9]+", " ", s).strip()


def display(name):
    """The name a page shows: GeoNames' "Saint" written the British way."""
    return re.sub(r"^Saint ", "St ", DISPLAY_NAMES.get(name, name))


def load_gazetteer(path):
    """[(normalised names, name, lat, lon, population, nation, county, where, area)]
    for every place in the UK and the Crown Dependencies. `where` is the place's
    own (lat, lon), which distances to a venue are measured from, and `area` its
    council area (GeoNames' admin1.admin2); every place in Greater London carries
    London's own name, centre and population beside them."""
    places, london_parts = [], []
    with open(path, encoding="utf-8") as f:
        for line in f:
            p = line.rstrip("\n").split("\t")
            if p[8] not in COUNTRIES:
                continue
            if p[8] in CROWN_DEPENDENCIES:
                nation, county = "Crown Dependencies", CROWN_DEPENDENCIES[p[8]]
            else:
                nation = NATIONS.get(p[10])
                county = COUNTIES.get(p[10], {}).get(p[11])
                if not nation or not county:
                    continue
            names = {norm(p[1]), norm(p[2])}
            where = (float(p[4]), float(p[5]))
            place = (names, p[1], *where, int(p[14] or 0), nation, county, where, f"{p[8]}.{p[10]}.{p[11]}")
            (london_parts if county == "Greater London" and p[1] != "London" else places).append(place)
    london = next(g for g in places if g[1] == "London" and g[6] == "Greater London")
    places.extend((g[0], *london[1:7], *g[7:]) for g in london_parts)
    return places


def landmass(county, lat, lon):
    """The island a place is on — one no cluster reaches across — or the land of
    Great Britain or of Ireland."""
    if county in CROWN_DEPENDENCIES.values() or county in ISLAND_COUNTIES:
        return county
    for island, (s, n, w, e) in ISLAND_BOXES.items():
        if s <= lat <= n and w <= lon <= e:
            return island
    return None


def place(venues, gazetteer, venue_towns=VENUE_TOWNS):
    """(towns, cinemaObject → town key) for every venue. Refuses a venue it cannot
    place — see the module docstring for the order it tries."""
    towns, town_of, own, problems = {}, {}, collections.defaultdict(list), []

    def keep(g):
        key = f"{display(g[1])}, {g[6]}"
        towns[key] = {"name": display(g[1]), "nation": g[5], "county": g[6], "lat": g[2], "lon": g[3],
                      "population": g[4]}
        return key

    def nearest(at, within):
        near = [(town_pages.km(at, g[7]), g) for g in gazetteer]
        near = [x for x in near if x[0] <= within]
        return min(near, key=lambda x: (x[0], x[1][1]))[1] if near else None

    for v in sorted(venues, key=lambda v: v["cinemaObject"]):
        at = (float(v["lat"]), float(v["lon"]))
        name = norm(v["town"])
        name = norm(venue_towns.get(v["cinemaObject"]) or ALIASES.get(name, name))
        named = [g for g in gazetteer if name in g[0] and town_pages.km(at, g[7]) <= NAME_MATCH_KM]
        if named:
            g = max(named, key=lambda g: (g[4], g[1]))
            # A postal town reaches past its own council's edge: Cumbernauld's
            # Lanternhouse is "Glasgow G67", Pocklington's arts centre "York YO42".
            # A venue that far from the town its address names, and right beside
            # a place in another council area, is in that area's biggest town
            # nearby — Showcase Coatbridge sits in Bargeddie, which is Coatbridge.
            beside = nearest(at, BESIDE_KM)
            if (town_pages.km(at, g[7]) > POSTAL_REACH_KM and beside and beside[8] != g[8]
                    and v["cinemaObject"] not in MISPLACED):
                g = max((h for h in gazetteer if h[8] == beside[8] and town_pages.km(at, h[7]) <= AREA_TOWN_KM),
                        key=lambda h: (h[4], h[1]))
            town_of[v["cinemaObject"]] = keep(g)
        elif name in OWN_TOWNS:
            # Filed under the county of the place nearest it, wherever that is.
            g = nearest(at, 50)
            key = f"{OWN_TOWNS[name]}, {g[6]}"
            own[key].append((v, g))
            town_of[v["cinemaObject"]] = key
        else:
            problems.append(f"{v['cinemaObject']} ({v['town']!r}): no place of that name within "
                            f"{NAME_MATCH_KM} km — review it into VENUE_TOWNS, ALIASES or OWN_TOWNS")
    for key, vgs in own.items():
        if key in towns:
            problems.append(f"{key} is both a GeoNames place and an OWN_TOWNS entry")
            continue
        _, g = vgs[0]
        towns[key] = {"name": key.split(", ")[0], "nation": g[5], "county": g[6],
                      "lat": town_pages.exact_mean(v["lat"] for v, _ in vgs), "lon": town_pages.exact_mean(v["lon"] for v, _ in vgs),
                      "population": 0}
    if problems:
        sys.exit("\n".join(problems))
    for t in towns.values():
        island = landmass(t["county"], t["lat"], t["lon"])
        t["landmass"] = island or ("Ireland" if t["nation"] == "Northern Ireland" else "Great Britain")
    return towns, town_of


def main(gazetteer_path):
    venues = json.loads((DATA / "venues.json").read_text())
    towns, town_of = place(venues, load_gazetteer(gazetteer_path))
    OUT.write_text(json.dumps({"towns": dict(sorted(towns.items())), "venues": dict(sorted(town_of.items()))},
                              ensure_ascii=False, indent=1) + "\n", encoding="utf-8")
    unmatched = sum(1 for t in towns.values() if not t["population"])
    print(f"Wrote {OUT.relative_to(ROOT)}: {len(town_of)} venues in {len(towns)} towns "
          f"({unmatched} with no GeoNames population)")


if __name__ == "__main__":
    main(sys.argv[1])
