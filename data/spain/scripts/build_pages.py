#!/usr/bin/env python3
"""Build data/spain/pages.json — which Spanish listing page each venue belongs to.

A Spanish page used to be a whole PROVINCE: /barcelona/ was 55 cinemas in 34
towns, Berga's and Sitges' among them. It is now Poland's rule, from the shared
`data/scripts/town_pages.py`:

  1. A MAJOR city — one of the 43 municipalities of 150,000 people or more
     (GeoNames' municipal population; Badajoz, 149,746, is the first one out) —
     lists only the venues inside that municipality.
  2. Any other town with at least 3 venues is a page of its own.
  3. Every remaining town is clustered: a 2-venue town takes the 1- and 2-venue
     towns within 10 km; the 1-venue towns left cluster within 25 km of the
     biggest of them; a town still alone joins the nearest cluster within 35 km.

A cluster is named after its anchor ("Sitges y alrededores" — the phrase is the
Scala side's, `SpanishPage`). A town is a MUNICIPALITY, and its position and
population come from `town-coords.json` (`build_venue_towns.py`), which resolves
every venue to one from its own postal address — not from SensaCine's section
headers, which file Huércal-Overa's cinema under a Córdoba hamlet.

A page that existed before and is not a page any more is listed under "retired",
pointing at the page now holding MOST of its venues — for the 52 provinces of the
first build, usually the capital (`/asturias/` → Oviedo or Gijón, whichever has
more). Each retired entry carries the slug its old page was QUALIFIED to where
another country already has the plain one (`toledo-castilla-la-mancha`), because
that qualification happens in Scala (`City.spanishSlug`) and the redirect has to
come from the URL the page really had.

Every page — and every town on it — is reported with the province it is in; the
picker groups by the anchor's province.

    python3 data/spain/scripts/build_pages.py
    python3 data/spain/scripts/test_build_pages.py
    python3 data/spain/scripts/generate_roster.py
"""
import collections
import json
import pathlib
import sys

ROOT = pathlib.Path(__file__).resolve().parents[3]
DATA = ROOT / "data" / "spain"
OUT = DATA / "pages.json"
sys.path.insert(0, str(ROOT / "data" / "scripts"))
import retired_venues  # noqa: E402
import town_pages  # noqa: E402
from town_pages import km, slugify  # noqa: E402

# Every Spanish municipality of 150,000 or more (GeoNames ADM3 population, 2026-10),
# biggest first. All 43 have venues.
MAJORS = [
    "Madrid", "Barcelona", "Valencia", "Sevilla", "Zaragoza", "Málaga", "Murcia", "Palma de Mallorca",
    "Las Palmas de Gran Canaria", "Bilbao", "Alicante", "Córdoba", "Valladolid", "Vigo", "Gijón",
    "L'Hospitalet de Llobregat", "A Coruña", "Vitoria-Gasteiz", "Granada", "Elche", "Oviedo", "Badalona",
    "Terrassa", "Cartagena", "Jerez de la Frontera", "Sabadell", "Santa Cruz de Tenerife", "Móstoles",
    "Alcalá de Henares", "Pamplona", "Fuenlabrada", "Almería", "Leganés", "San Sebastián", "Getafe",
    "Castellón de la Plana", "Burgos", "Santander", "Albacete", "Alcorcón", "San Cristóbal de La Laguna",
    "Salamanca", "Logroño",
]


# Off the mainland, a straight line under 35 km can cross the sea: Ceuta is 28 km
# from Algeciras, Lanzarote 11 km from Fuerteventura. Every town in these
# provinces is on its island (the build refuses one missing here), and towns on
# different landmasses never share a page. Ceuta and Melilla are their own.
ISLAND_PROVINCES = {"Islas Baleares", "Las Palmas", "Santa Cruz de Tenerife", "Ceuta", "Melilla"}
ISLANDS = {
    "Palma de Mallorca": "Mallorca", "Manacor": "Mallorca", "Marratxí": "Mallorca",
    "Maó": "Menorca", "Ciutadella de Menorca": "Menorca",
    "Ibiza": "Ibiza", "Sant Antoni de Portmany": "Ibiza", "Santa Eulària des Riu": "Ibiza",
    "Las Palmas de Gran Canaria": "Gran Canaria", "Telde": "Gran Canaria", "Santa Lucía de Tirajana": "Gran Canaria",
    "Arrecife": "Lanzarote",
    "Puerto del Rosario": "Fuerteventura", "Antigua": "Fuerteventura",
    "Santa Cruz de Tenerife": "Tenerife", "San Cristóbal de La Laguna": "Tenerife", "Adeje": "Tenerife",
    "Arona": "Tenerife", "Candelaria": "Tenerife", "La Orotava": "Tenerife", "Los Realejos": "Tenerife",
    "Santa Cruz de la Palma": "La Palma", "Los Llanos de Aridane": "La Palma",
    "Ceuta": "Ceuta", "Melilla": "Melilla",
}


def landmass(towns):
    unplaced = sorted(t for t, r in towns.items() if r["province"] in ISLAND_PROVINCES and t not in ISLANDS)
    if unplaced:
        sys.exit(f"towns off the mainland with no island in ISLANDS: {unplaced}")
    return lambda town: ISLANDS.get(town, "mainland")


def provinces():
    """The roster's provinces — the pages before the first build, and each one's
    zone and autonomous community."""
    rows = json.loads((DATA / "provinces.json").read_text())
    communities = json.loads((DATA / "communities.json").read_text())
    return rows, communities


def province_venues(rows):
    """Each province's rostered venues (displayNames, roster order): SensaCine's
    less the retired, plus the Ocine venues it does not list."""
    retired = retired_venues.load(DATA)
    ocine = json.loads((DATA / "ocine.json").read_text())
    return {p["name"]: [c["displayName"] for c in p["cinemas"] if c["theaterId"] not in retired]
            + [u["name"] for u in ocine["unlisted"] if u["province"] == p["name"]] for p in rows}


def qualified(name, community):
    """The slug `City.spanishSlug` gives a page whose own one another country has."""
    return slugify(f"{name} {community}")


def build(venues, venue_town, towns, zones, communities, previous, prior_retired, rules=town_pages.Rules()):
    """venues: every displayName, in roster order; venue_town: displayName →
    town; towns: town → {lat, lon, pop, province}; zones: province → zoneId;
    previous: slug → {"cinemas": [...], "qualifiedSlug": ...} for every page the
    last build had (the provinces, before the first)."""
    by_town = collections.defaultdict(list)
    for name in venues:
        by_town[venue_town[name]].append(name)
    majors_here = [m for m in MAJORS if m in by_town]

    def page(slug, kind, anchor, members):
        t = towns[anchor]
        return {"slug": slug, "kind": kind, "name": anchor, "anchor": anchor, "province": t["province"],
                "community": communities[t["province"]], "zoneId": zones[t["province"]],
                "lat": round(t["lat"], 4), "lon": round(t["lon"], 4), "towns": members, "cinemas": []}

    majors = {slugify(m): page(slugify(m), "major", m, [m]) for m in majors_here}
    major_town = {m: slugify(m) for m in majors_here}
    result, page_of_town = town_pages.build(
        by_town, majors, major_town, towns,
        display=lambda anchor: anchor,
        make_page=page,
        region_slug=lambda anchor: towns[anchor]["province"],
        cinema_of=lambda name: name,
        rules=rules,
        landmass=landmass({t: towns[t] for t in by_town}))

    # A town far from the page its venues were on in the last town-page build has
    # been matched to a namesake. (The provinces of the first build are no
    # reference — Badajoz's spans 200 km — and need none: each town is resolved
    # inside the province its venue's postal code names.)
    before = {v: slug for slug, old in previous.items() for v in old["cinemas"]}
    centres = {p: (r["lat"], r["lon"]) for p, r in previous.items() if r.get("kind") != "province"}
    pos = {t: (towns[t]["lat"], towns[t]["lon"]) for t in by_town}
    drifted = town_pages.drifted(
        [t for t in by_town if t not in major_town], pos,
        lambda t: [centres[before[v]] for v in by_town[t] if before.get(v) in centres], rules.max_drift_km)
    if drifted:
        sys.exit(f"towns placed > {rules.max_drift_km} km from their previous page (namesake?): {drifted}")

    # A page gone since the last build goes to the page now holding most of its
    # venues; on a tie, a major city's (/asturias/: Gijón over Parque Astur's two
    # screens in Corvera), then the one nearest where it was centred — /soria/'s
    # three one-cinema towns go to Golmayo, Soria's own cinema in all but name.
    def heir(old):
        held = collections.Counter(page_of_town[venue_town[v]] for v in old["cinemas"] if v in venue_town)
        centre = (old["lat"], old["lon"])
        return max(held, key=lambda s: (held[s], result[s]["kind"] == "major",
                                        -km(centre, (result[s]["lat"], result[s]["lon"])), s))
    gone = {slug: heir(old) for slug, old in previous.items() if slug not in result}
    retired = town_pages.retire(gone, {k: v["page"] for k, v in prior_retired.items()}, result)
    qualified_of = {**{k: v["qualifiedSlug"] for k, v in prior_retired.items()},
                    **{slug: old["qualifiedSlug"] for slug, old in previous.items()}}
    retired = {slug: {"page": page, "qualifiedSlug": qualified_of[slug]} for slug, page in sorted(retired.items())}
    for p in result.values():
        p["qualifiedSlug"] = qualified(p["name"], p["community"])
    return list(result.values()), retired


def main():
    rows, communities = provinces()
    zones = {p["name"]: p["zoneId"] for p in rows}
    coords = json.loads((DATA / "town-coords.json").read_text())
    prior = json.loads(OUT.read_text()) if OUT.exists() else None
    held = province_venues(rows)
    if prior:
        previous = {p["slug"]: p for p in prior["pages"]}
        prior_retired = prior["retired"]
    else:
        previous = {p["slug"]: {"cinemas": held[p["name"]],
                                "qualifiedSlug": qualified(p["name"], communities[p["name"]]),
                                "kind": "province", "lat": p["lat"], "lon": p["lon"]}
                    for p in rows}
        prior_retired = {}
    pages, retired = build([v for p in rows for v in held[p["name"]]], coords["venues"], coords["towns"], zones, communities,
                           previous, prior_retired)
    pages.sort(key=lambda p: (p["kind"] != "major", MAJORS.index(p["name"]) if p["kind"] == "major" else 0,
                              p["slug"]))
    OUT.write_text(json.dumps({"pages": pages, "retired": retired}, ensure_ascii=False, indent=1) + "\n",
                   encoding="utf-8")
    kinds = collections.Counter(p["kind"] for p in pages)
    sizes = sorted(len(p["cinemas"]) for p in pages)
    print(f"Wrote {OUT.relative_to(ROOT)}: {len(pages)} pages {dict(kinds)}, {sum(sizes)} venues "
          f"(median {sizes[len(sizes) // 2]}, max {sizes[-1]} per page), {len(retired)} retired slugs")


if __name__ == "__main__":
    main()
