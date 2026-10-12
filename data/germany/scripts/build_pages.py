#!/usr/bin/env python3
"""Build data/germany/pages.json — which German listing page each venue belongs to.

A German page used to be a REGION: every venue within ~35 km of a hub city, so
`/koeln/` listed Düsseldorf, Bonn and Leverkusen and `/berlin/` Potsdam. It is now
the rule Poland and Spain use, from the shared `data/scripts/town_pages.py`:

  1. A MAJOR city — a municipality of 100,000 people or more (GeoNames' figure in
     `city-coords.json`) — lists only the venues inside it.
  2. Any other town with at least 3 venues is a page of its own.
  3. Every remaining town is clustered: a 2-venue town takes the 1- and 2-venue
     towns within 10 km; the 1-venue towns left cluster within 20 km of the
     biggest of them; a town still alone joins the nearest cluster within 30 km.

A cluster is named after its anchor — the town with the most venues, then the most
people ("Bayreuth und Umgebung"; the phrase is the Scala side's, `GermanPage`).

Inputs, all committed:

  venues.json       every venue ever harvested: its Filmstarts theaterId -- or,
                    for a venue Filmstarts does not list, the `feed` that found it
                    ("cineamo:997", add_found_venues.py) -- name, the town it is
                    in, and its displayName (the wire key). `retired.json` drops
                    the closed ones.
  city-coords.json  each Filmstarts city → GeoNames lat/lon, population and admin1
                    (`geocode_cities.py`). A venue in a city missing here stops
                    the build — geocode it first.
  TOWN_ALIASES      (below) Filmstarts' second spellings of one town.
  TOWN_COORDS       (below) towns `city-coords.json` matched to a namesake.

A page that existed before and is not a page any more is listed under "retired",
pointing at the page holding the town it was named after (`/vechta/` → Cloppenburg,
whose cluster Vechta is on) — or, when that town has no venue left, at the page now
holding MOST of its venues. 19 of the 158 regions of the first build (2026-10) left
no page under their own slug.
Every page carries the Bundesland of its anchor (from the coordinates, not from the Filmstarts lander page
a venue was crawled under, which files Münster under Hessen); the picker groups by
it, and a slug another country already serves is qualified with it.

Adding venues: append them to venues.json, then

    python3 data/germany/scripts/build_pages.py
    python3 data/germany/scripts/test_build_pages.py
    python3 data/germany/scripts/generate_roster.py

The build refuses a town placed more than 60 km from the page its venues were on
before — the shape a gazetteer namesake takes. Pin the town in TOWN_COORDS.
"""
import collections
import json
import pathlib
import sys

ROOT = pathlib.Path(__file__).resolve().parents[3]
DATA = ROOT / "data" / "germany"
OUT = DATA / "pages.json"
sys.path.insert(0, str(ROOT / "data" / "scripts"))
import retired_venues  # noqa: E402
import town_pages  # noqa: E402
from town_pages import km  # noqa: E402,F401  (the tests' spelling)

MAJOR_MIN_POPULATION = 100_000

# Tighter than Poland's and Spain's 10/25/35 km. Germany's small towns are dense
# enough that at those radii villages chain into clusters 66 km across (the
# 2026-10 simulation); at 10/20/30 no cinema is more than ~43 km from the town its
# page is named after, and 95% are within 21 km.
RULES = town_pages.Rules(two_venue_radius_km=10, one_venue_radius_km=20, absorb_radius_km=30)

# GeoNames admin1 code → Bundesland, as the picker heads it.
LAENDER = {
    "01": "Baden-Württemberg", "02": "Bayern", "03": "Bremen", "04": "Hamburg", "05": "Hessen",
    "06": "Niedersachsen", "07": "Nordrhein-Westfalen", "08": "Rheinland-Pfalz", "09": "Saarland",
    "10": "Schleswig-Holstein", "11": "Brandenburg", "12": "Mecklenburg-Vorpommern", "13": "Sachsen",
    "14": "Sachsen-Anhalt", "15": "Thüringen", "16": "Berlin",
}

# Filmstarts files some towns under two spellings — or under a district — and
# each spelling was geocoded separately, to the same point. One town each; the
# name kept is the one the town's region page was called by where it had one
# (`/freiburg/`, `/brandenburg/`), else the town's own official name.
TOWN_ALIASES = {
    "Frankfurt": "Frankfurt am Main",
    "Freiburg im Breisgau": "Freiburg",
    "Oldenburg (Oldenburg)": "Oldenburg",
    "Paderborn (Kernstadt)": "Paderborn",
    "Brandenburg an der Havel": "Brandenburg",
    "Dessau": "Dessau-Roßlau",
    "Frankfurt (Oder)": "Frankfurt an der Oder",
    "Fürstenwalde": "Fürstenwalde an der Spree",
    "Wittenberg": "Lutherstadt Wittenberg",
    "Kelkheim (Taunus)": "Kelkheim",
    "Esslingen": "Esslingen am Neckar",
    "Neumarkt": "Neumarkt in der Oberpfalz",
    "Neustadt an der Weinstrasse": "Neustadt an der Weinstraße",
    "Singen": "Singen am Hohentwiel",
    "Weilheim": "Weilheim in Oberbayern",
    "Lahr": "Lahr am Schwarzwald",
    "Neunkirchen (Innenstadt)": "Neunkirchen",
    # Places Filmstarts names by a district or a district authority.
    "Altona": "Hamburg",   # the Hamburg drive-in, in Bahrenfeld
    "Kalk": "Köln",
    "Regionalverband Saarbrücken": "Völklingen",   # Residenz, 66333 Völklingen
}
# An alias joins two spellings of ONE place: a pair further apart than this is
# two places, and the alias a mistake.
ALIAS_MAX_KM = 10

# Towns `city-coords.json` placed at a namesake — the name matched, the place did
# not; each checked against the venue's own address on Filmstarts (2026-10).
TOWN_COORDS = {
    # Lohne (Oldenburg), Niedersachsen — not Löhne, Westphalia, whose point it had.
    "Lohne": {"lat": 52.66625, "lon": 8.2375, "population": 27_000, "admin1": "06"},
    # Kinopolis Main-Taunus, 65843 Sulzbach (Taunus) — not Sulzbach-Rosenberg, Bavaria.
    "Sulzbach": {"lat": 50.13194, "lon": 8.52722, "population": 9_200, "admin1": "05"},
    # Gronauer Lichtspiele, 31028 Gronau (Leine) — not Gronau, Westphalia.
    "Gronau": {"lat": 52.08472, "lon": 9.78111, "population": 10_800, "admin1": "06"},
    # Kinomobil, 72661 Grafenberg (Reutlingen) — not Grafenberg, Franconia.
    "Grafenberg": {"lat": 48.56167, "lon": 9.30472, "population": 2_600, "admin1": "01"},
    # Film Theater Ebersbach, 02730 Ebersbach-Neugersdorf (Saxony) — not Ebersbach an der Fils.
    "Ebersbach": {"lat": 50.99444, "lon": 14.6, "population": 12_000, "admin1": "13"},
    # Olympia, 69493 Hirschberg an der Bergstraße — not Hirschberg an der Saale, Thuringia.
    "Hirschberg": {"lat": 49.50583, "lon": 8.66528, "population": 9_900, "admin1": "01"},
    # Kinomobil, 74399 Walheim (Ludwigsburg) — not Walheim, a district of Aachen.
    "Walheim": {"lat": 49.01444, "lon": 9.15139, "population": 3_200, "admin1": "01"},
    # KuKi, 94405 Landau an der Isar — not Landau in der Pfalz, which has a page of its own.
    "Landau": {"lat": 48.67278, "lon": 12.69306, "population": 13_500, "admin1": "02"},
    # Park-Theater, 15377 Buckow (Märkische Schweiz) — not Buckow, a district of Berlin.
    "Buckow": {"lat": 52.57, "lon": 14.07, "population": 1_500, "admin1": "11"},
    # Kinocafe, 84416 Taufkirchen (Vils) — not Taufkirchen outside Munich.
    "Taufkirchen (Vils)": {"lat": 48.34361, "lon": 12.13056, "population": 10_000, "admin1": "02"},
    # Not in city-coords.json at all: the Residenz's town, which Filmstarts files
    # under its district authority (see TOWN_ALIASES).
    "Völklingen": {"lat": 49.25028, "lon": 6.85917, "population": 39_000, "admin1": "09"},
}


def de_slug(name):
    """A German URL slug: umlauts spelt out (`muenster`, `nuernberg`), as every
    German page slug has been since the launch."""
    for a, b in {"ä": "ae", "ö": "oe", "ü": "ue", "ß": "ss", "Ä": "Ae", "Ö": "Oe", "Ü": "Ue"}.items():
        name = name.replace(a, b)
    return town_pages.slugify(name)


def load_coords():
    return json.loads((DATA / "city-coords.json").read_text())


def load_towns(coords, aliases=TOWN_ALIASES, pins=TOWN_COORDS):
    """(Filmstarts city → town, town → {lat, lon, pop, land}). Refuses an alias
    to a town the gazetteer lacks, or one joining two places far apart."""
    merged = {**coords, **pins}
    towns = {name: {"lat": c["lat"], "lon": c["lon"], "pop": c["population"], "land": LAENDER[c["admin1"]],
                    "pinned": name in pins}
             for name, c in merged.items()}
    for spelling, town in aliases.items():
        if town not in towns:
            sys.exit(f"TOWN_ALIASES: {spelling!r} → {town!r}, which city-coords.json does not have")
        if spelling in towns:
            d = km((towns[spelling]["lat"], towns[spelling]["lon"]), (towns[town]["lat"], towns[town]["lon"]))
            if d > ALIAS_MAX_KM:
                sys.exit(f"TOWN_ALIASES: {spelling!r} is {d:.0f} km from {town!r} — two places, not one")
    return (lambda city: aliases.get(city, city)), towns


def roster_venues():
    """Every rostered venue, in venues.json order — the harvest less the retired."""
    retired = retired_venues.load(DATA)
    return [v for v in json.loads((DATA / "venues.json").read_text()) if venue_key(v) not in retired]


def venue_key(venue):
    """A venue's key: its Filmstarts theaterId, or the `feed` that found it."""
    return venue.get("theaterId") or venue["feed"]


def build(venues, venue_town, towns, previous, prior_retired, rules=RULES):
    """venues: every displayName, in roster order; venue_town: displayName →
    town; towns: town → {lat, lon, pop, land}; previous: slug → {"cinemas",
    "lat", "lon", "kind", "qualifiedSlug", "anchor"} for every page the last build had (the
    regions, before the first)."""
    by_town = collections.defaultdict(list)
    for name in venues:
        by_town[venue_town[name]].append(name)
    unplaced = sorted(t for t in by_town if t not in towns)
    if unplaced:
        sys.exit(f"towns with no coordinates in city-coords.json (geocode them): {unplaced}")
    majors_here = sorted((t for t in by_town if towns[t]["pop"] >= MAJOR_MIN_POPULATION),
                         key=lambda t: (-towns[t]["pop"], t))

    def page(slug, kind, anchor, members):
        t = towns[anchor]
        return {"slug": slug, "kind": kind, "name": anchor, "anchor": anchor, "bundesland": t["land"],
                "qualifiedSlug": de_slug(f"{anchor} {t['land']}"),
                "lat": round(t["lat"], 4), "lon": round(t["lon"], 4), "towns": members, "cinemas": []}

    majors = {de_slug(m): page(de_slug(m), "major", m, [m]) for m in majors_here}
    major_town = {m: de_slug(m) for m in majors_here}
    result, page_of_town = town_pages.build(
        by_town, majors, major_town, towns,
        display=lambda anchor: anchor,
        make_page=page,
        region_slug=lambda anchor: towns[anchor]["land"],
        cinema_of=lambda name: name,
        rules=rules,
        slug_of=de_slug)

    # A town far from the page its venues were on before has been matched to a
    # namesake — unless its coordinate is pinned on purpose.
    before = {v: slug for slug, old in previous.items() for v in old["cinemas"]}
    centres = {p: (r["lat"], r["lon"]) for p, r in previous.items()}
    pos = {t: (towns[t]["lat"], towns[t]["lon"]) for t in by_town}
    drifted = town_pages.drifted(
        [t for t in by_town if t not in major_town and not towns[t].get("pinned")], pos,
        lambda t: [centres[before[v]] for v in by_town[t] if v in before], rules.max_drift_km)
    if drifted:
        sys.exit(f"towns placed > {rules.max_drift_km} km from their previous page (namesake?): {drifted} "
                 f"— pin them in TOWN_COORDS")

    # A page gone since the last build is named after a town, and a visitor typing
    # /vechta/ wants Vechta: it goes to the page holding that town. Only when the
    # town has no venue left does it go to the page now holding most of its venues
    # (`town_pages.heir` has the tie-breaks).
    def heir(old):
        if old.get("anchor") in page_of_town:
            return page_of_town[old["anchor"]]
        held = town_pages.held_by(old["cinemas"], venue_town, page_of_town)
        return town_pages.heir(held, (old["lat"], old["lon"]), result)
    gone = {slug: heir(old) for slug, old in previous.items() if slug not in result}
    retired = town_pages.retire(gone, {k: v["page"] for k, v in prior_retired.items()}, result)
    qualified_of = {**{k: v["qualifiedSlug"] for k, v in prior_retired.items()},
                    **{slug: old["qualifiedSlug"] for slug, old in previous.items()}}
    retired = {slug: {"page": page, "qualifiedSlug": qualified_of[slug]} for slug, page in sorted(retired.items())}
    pages = list(result.values())
    pages.sort(key=lambda p: (p["kind"] != "major", majors_here.index(p["name"]) if p["kind"] == "major" else 0,
                              p["slug"]))
    return pages, retired


def main():
    roster = roster_venues()
    town_of, towns = load_towns(load_coords())
    prior = json.loads(OUT.read_text())
    previous = {p["slug"]: p for p in prior["pages"]}
    pages, retired = build([v["displayName"] for v in roster], {v["displayName"]: town_of(v["city"]) for v in roster},
                           towns, previous, prior["retired"])
    OUT.write_text(json.dumps({"pages": pages, "retired": retired}, ensure_ascii=False, indent=1) + "\n",
                   encoding="utf-8")
    kinds = collections.Counter(p["kind"] for p in pages)
    sizes = sorted(len(p["cinemas"]) for p in pages)
    print(f"Wrote {OUT.relative_to(ROOT)}: {len(pages)} pages {dict(kinds)}, {sum(sizes)} venues "
          f"(median {sizes[len(sizes) // 2]}, max {sizes[-1]} per page, {sizes.count(1)} with one), "
          f"{len(retired)} retired slugs")


if __name__ == "__main__":
    main()
