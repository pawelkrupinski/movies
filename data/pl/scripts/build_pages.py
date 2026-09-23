#!/usr/bin/env python3
"""Build data/pl/pages.json — which Polish listing page each venue belongs to.

A Polish page used to be a city plus whatever towns had been folded into it
(Poznań listed Buk and Wronki; Konin listed Turek and Koło). The rule is now:

  1. A MAJOR city (the 41 original city pages, Trójmiasto being Gdańsk, Gdynia
     and Sopot) lists only the venues inside that city.
  2. Any other town with at least 3 venues is a page of its own.
  3. Every remaining town is clustered:
     a. towns with 2 venues anchor a cluster that takes every 1- or 2-venue
        town within 10 km of them;
     b. the 1-venue towns left over cluster among themselves within 25 km of
        the biggest of them (by population);
     c. a town still alone after that joins the nearest multi-town cluster
        within 35 km, if there is one.

A cluster is named after its anchor town ("Turek i okolice") and slugged after
it; a page that existed before and is not a page any more is listed under
"retired" with the page now holding its town, so its old URL still lands there. Distances are straight-line between GeoNames
town centres — 25 km is roughly a half-hour drive on Polish county roads.

Inputs: data/pl/venues.json (venue → town, today's page), common/.../City.scala
(the pages that exist now and their coordinates), data/pl/town_forms.json (the
locative of each anchor, hand-checked), and the GeoNames Poland dump:

    mkdir -p data/pl/geonames
    curl -sL https://download.geonames.org/export/dump/PL.zip -o data/pl/geonames/PL.zip
    unzip -o data/pl/geonames/PL.zip -d data/pl/geonames
    python3 data/pl/scripts/build_pages.py
    rm -rf data/pl/geonames
    python3 data/pl/scripts/generate_polish_pages.py
"""
import json
import math
import re
import sys
from collections import Counter, defaultdict
from pathlib import Path

ROOT = Path(__file__).resolve().parents[3]
VENUES = ROOT / "data/pl/venues.json"
FORMS = ROOT / "data/pl/town_forms.json"
OUT = ROOT / "data/pl/pages.json"
CITY_SCALA = ROOT / "common/src/main/scala/models/City.scala"
GEONAMES = ROOT / "data/pl/geonames/PL.txt"

MAJOR_SLUGS = [
    "poznan", "wroclaw", "warszawa", "krakow", "lodz", "katowice", "szczecin", "bialystok", "trojmiasto",
    "bydgoszcz", "lublin", "czestochowa", "radom", "sosnowiec", "torun", "kielce", "rzeszow", "gliwice",
    "zabrze", "olsztyn", "bielsko-biala", "opole", "rybnik", "gorzow-wielkopolski", "elblag", "koszalin",
    "kalisz", "zielona-gora", "tychy", "walbrzych", "tarnow", "wloclawek", "legnica", "plock", "bytom",
    "dabrowa-gornicza", "nowy-sacz", "slupsk", "jelenia-gora", "przemysl", "konin",
]
TRICITY = {"Gdańsk", "Gdynia", "Sopot"}
OWN_PAGE_MIN_VENUES = 3
TWO_VENUE_RADIUS_KM = 10
ONE_VENUE_RADIUS_KM = 25
ABSORB_RADIUS_KM = 35

# GeoNames admin1 code → voivodeship, as the picker heads it.
VOIVODESHIPS = {
    "72": "Dolnośląskie", "73": "Kujawsko-pomorskie", "74": "Łódzkie", "75": "Lubelskie",
    "76": "Lubuskie", "77": "Małopolskie", "78": "Mazowieckie", "79": "Opolskie",
    "80": "Podkarpackie", "81": "Podlaskie", "82": "Pomorskie", "83": "Śląskie",
    "84": "Świętokrzyskie", "85": "Warmińsko-mazurskie", "86": "Wielkopolskie", "87": "Zachodniopomorskie",
}
# How a town is SHOWN where GeoNames spells it differently from its official name.
DISPLAY = {"Jastrzębie Zdrój": "Jastrzębie-Zdrój"}
# Towns whose name GeoNames gives to more than one place, where the most
# populous one (the default pick) is the wrong one. Janki the Cinema City mall
# village is by Raszyn, 13 km from Warsaw; GeoNames' biggest Janki is in
# Podlaskie. `load_gazetteer` pins these, and `build` refuses any town that lands
# more than MAX_DRIFT_KM from the page it used to be on — how Janki was caught.
TOWN_COORDS = {"Janki": {"lat": 52.13733, "lon": 20.90012, "pop": 0, "admin1": "78"}}
MAX_DRIFT_KM = 60
# Annotations that spell a town differently from GeoNames.
TOWN_ALIASES = {"Połczyn": "Połczyn-Zdrój", "Krynica Zdrój": "Krynica-Zdrój", "Rabka Zdrój": "Rabka-Zdrój",
                "Jastrzębie-Zdrój": "Jastrzębie Zdrój"}


def fold(s):
    import unicodedata
    s = s.replace("ł", "l").replace("Ł", "L")
    return "".join(c for c in unicodedata.normalize("NFD", s) if unicodedata.category(c) != "Mn").lower()


def slugify(name):
    return re.sub(r"[^a-z0-9]+", "-", fold(name)).strip("-")


def km(a, b):
    p = math.radians
    return 2 * 6371 * math.asin(math.sqrt(math.sin(p(b[0] - a[0]) / 2) ** 2 +
                                          math.cos(p(a[0])) * math.cos(p(b[0])) * math.sin(p(b[1] - a[1]) / 2) ** 2))


def load_gazetteer():
    towns = {}
    with open(GEONAMES, encoding="utf-8") as f:
        for line in f:
            c = line.rstrip("\n").split("\t")
            if c[6] != "P":
                continue
            name, pop = c[1], int(c[14] or 0)
            if name not in towns or pop > towns[name]["pop"]:
                towns[name] = {"lat": float(c[4]), "lon": float(c[5]), "pop": pop, "admin1": c[10]}
    towns.update(TOWN_COORDS)
    return towns


def current_pages():
    """slug → (object, nominative, lat, lon) for every Polish City declared today."""
    src = CITY_SCALA.read_text(encoding="utf-8").split("// ── United Kingdom")[0]
    pages = {}
    for m in re.finditer(r'case object (\w+) extends City\(\s*slug\s*=\s*"([^"]+)",\s*labels\s*=\s*CityLabels\('
                         r'nominative = "([^"]+)".*?lat\s*=\s*([\d.]+),\s*lon\s*=\s*([\d.]+)', src, re.S):
        pages[m[2]] = (m[1], m[3], float(m[4]), float(m[5]))
    return pages


def build(venues, gaz, forms, pages, previous, prior_retired):
    majors = {slug: pages[slug] for slug in MAJOR_SLUGS}
    major_town = {pages[s][1]: s for s in MAJOR_SLUGS if s != "trojmiasto"}
    for t in TRICITY:
        major_town[t] = "trojmiasto"
    page_town = {slug: nom for slug, (_, nom, _, _) in pages.items()}

    def town_of(v):
        t = v["town"] or page_town[v["citySlug"]]
        if t == "Trójmiasto":
            t = "Gdańsk"
        return TOWN_ALIASES.get(t, t)

    by_town = defaultdict(list)
    for v in venues:
        by_town[town_of(v)].append(v)
    missing = [t for t in by_town if t not in major_town and t not in gaz]
    if missing:
        sys.exit(f"towns missing from GeoNames: {missing}")

    result = {}   # slug -> page
    for slug in MAJOR_SLUGS:
        obj, nom, lat, lon = majors[slug]
        result[slug] = {"slug": slug, "kind": "major", "object": obj, "name": nom, "anchor": nom,
                        "lat": lat, "lon": lon, "towns": [], "cinemas": []}

    minor = {t: vs for t, vs in by_town.items() if t not in major_town}
    count = {t: len(vs) for t, vs in minor.items()}
    pos = {t: (gaz[t]["lat"], gaz[t]["lon"]) for t in minor}
    pop = {t: gaz[t]["pop"] for t in minor}

    own = sorted(t for t in minor if count[t] >= OWN_PAGE_MIN_VENUES)
    pool = set(minor) - set(own)
    clusters = [[t] for t in own]
    own_set = set(own)

    def grab(anchor, radius, eligible):
        members = [t for t in pool if t in eligible and km(pos[anchor], pos[t]) <= radius]
        members.sort(key=lambda t: (t != anchor, -count[t], -pop[t], t))
        pool.difference_update(members)
        return members

    two = {t for t in pool if count[t] == 2}
    while pool & two:
        anchor = max(pool & two, key=lambda t: (pop[t], t))
        clusters.append(grab(anchor, TWO_VENUE_RADIUS_KM, set(pool)))
    while pool:
        anchor = max(pool, key=lambda t: (pop[t], t))
        clusters.append(grab(anchor, ONE_VENUE_RADIUS_KM, set(pool)))

    # Only a ONE-venue town left alone is folded into a neighbour; a 2-venue town
    # that found nobody within 10 km is a page of its own, like a 3-venue one.
    multi = [c for c in clusters if len(c) > 1]
    final = []
    for c in clusters:
        if len(c) == 1 and count[c[0]] == 1 and multi:
            nearest = min(multi, key=lambda m: km(pos[c[0]], pos[m[0]]))
            if km(pos[c[0]], pos[nearest[0]]) <= ABSORB_RADIUS_KM:
                nearest.append(c[0])
                continue
        final.append(c)

    used = set(result)
    for c in final:
        # The anchor — the town the page is named after — is the one with the most
        # venues, then the most people.
        c.sort(key=lambda t: (-count[t], -pop[t], t))
        anchor = c[0]
        name = DISPLAY.get(anchor, anchor)
        slug = slugify(name)
        if slug in used:
            slug = f"{slug}-{slugify(VOIVODESHIPS[gaz[anchor]['admin1']])}"
        used.add(slug)
        if anchor not in forms:
            sys.exit(f"no locative for anchor {anchor!r} — add it to {FORMS.name}")
        result[slug] = {"slug": slug, "kind": "town" if len(c) == 1 else "cluster", "object": None,
                        "name": name, "anchor": anchor, "locative": forms[anchor],
                        "lat": round(pos[anchor][0], 4), "lon": round(pos[anchor][1], 4),
                        "towns": c, "cinemas": []}

    page_of_town = dict(major_town)
    for slug, p in result.items():
        for t in p["towns"]:
            page_of_town[t] = slug
    for t, vs in by_town.items():
        page = result[page_of_town[t]]
        page["cinemas"].extend(v["cinemaObject"] for v in vs)

    # A town whose gazetteer position is far from the page its venues were on
    # before has almost certainly been matched to a namesake — stop and pin it.
    drifted = []
    for t in minor:
        if t in TOWN_COORDS:   # pinned on purpose — its old page may be the mistake
            continue
        before = {v["citySlug"] for v in by_town[t]}
        centres = [(pages[s][2], pages[s][3]) for s in before if s in pages]
        centres += [(result[s]["lat"], result[s]["lon"]) for s in before if s in result]
        if centres and min(km(pos[t], c) for c in centres) > MAX_DRIFT_KM:
            drifted.append(t)
    if drifted:
        sys.exit(f"towns placed > {MAX_DRIFT_KM} km from their previous page (namesake?): {drifted} "
                 f"— pin them in TOWN_COORDS")

    admin_of = lambda lat, lon, name: gaz.get(name, min(gaz.values(), key=lambda g: km((lat, lon), (g["lat"], g["lon"]))))["admin1"]
    for p in result.values():
        anchor = "Gdańsk" if p["slug"] == "trojmiasto" else p["anchor"]
        p["voivodeship"] = VOIVODESHIPS[admin_of(p["lat"], p["lon"], anchor)]

    # Every page that existed before this build and is not a page any more: where
    # its URL goes now — the page holding its town. Kept across builds (a URL
    # outlives the re-cluster that retired it), and re-pointed when the page an
    # earlier retirement landed on has itself moved on.
    retired = {}
    for slug, town in previous.items():
        if slug not in result:
            retired[slug] = page_of_town[TOWN_ALIASES.get(town, town)]
    for slug, target in prior_retired.items():
        if slug not in result:
            retired[slug] = target if target in result else retired.get(target, target)
    missing_targets = {s: t for s, t in retired.items() if t not in result}
    if missing_targets:
        sys.exit(f"retired slugs point at pages that no longer exist: {missing_targets}")
    return list(result.values()), retired


def main():
    venues = json.loads(VENUES.read_text(encoding="utf-8"))
    forms = json.loads(FORMS.read_text(encoding="utf-8")) if FORMS.exists() else {}
    prior = json.loads(OUT.read_text(encoding="utf-8")) if OUT.exists() else None
    # The pages the last build produced (slug → the town it is named after), or,
    # before the first build, every Polish city declared in City.scala.
    previous = ({p["slug"]: p["anchor"] for p in prior["pages"]} if prior
                else {slug: nom for slug, (_, nom, _, _) in current_pages().items()})
    prior_retired = prior["retired"] if prior else {}
    pages, retired = build(venues, load_gazetteer(), forms, current_pages(), previous, prior_retired)
    pages.sort(key=lambda p: (p["kind"] != "major", MAJOR_SLUGS.index(p["slug"]) if p["kind"] == "major" else 0, p["slug"]))
    OUT.write_text(json.dumps({"pages": pages, "retired": retired}, ensure_ascii=False, indent=1) + "\n", encoding="utf-8")
    kinds = Counter(p["kind"] for p in pages)
    print(f"Wrote {OUT.relative_to(ROOT)}: {len(pages)} pages {dict(kinds)}, "
          f"{sum(len(p['cinemas']) for p in pages)} venues, {len(retired)} retired slugs")


if __name__ == "__main__":
    main()
