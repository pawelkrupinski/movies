#!/usr/bin/env python3
"""Build data/us/pages.json — which US listing page each venue belongs to.

A US page used to be a METRO: every venue within 75 km of a hub, folded into a
neighbour within 150 km, never across a state line — so `/los-angeles/` listed
Lancaster and Ventura, 315 of 468 pages held a cinema more than 50 km from the
town they were named after, and one travel-shed split at a state line into two
pages (Kansas City, Portland, Cincinnati). It is now the rule Poland, Spain and
Germany use, from the shared `data/scripts/town_pages.py`, because people do not
drive far just to see a film:

  1. A MAJOR city — a place of 100,000 people or more (GeoNames' figure in
     `towns.json`) — lists only the venues inside it.
  2. Any other town with at least 3 venues is a page of its own.
  3. Every remaining town is clustered: a 2-venue town takes the 1- and 2-venue
     towns within 20 km; the 1-venue towns left cluster within 30 km of the
     biggest of them; a town still alone joins the nearest cluster within 40 km.

Wider than Germany's 10/20/30 because American small towns are further apart;
at these radii 95% of cinemas are within ~28 km of the town their page is named
after. A cluster may cross a state line — Kansas City's suburbs on both sides are
one page now — and is filed in the picker under its ANCHOR's state. It is named
after its anchor, the town with the most venues, then the most people
("Bend area"; the phrase is the Scala side's, `UsCity`).

Inputs, all committed:

  venues.json   every venue ever harvested from Flicks; `retired.json` drops the
                closed ones, and a venue with no state is not rostered.
  towns.json    the town each venue is in and that town's GeoNames centre and
                population (`geocode_towns.py`). A venue missing there stops the
                build — geocode it first.

A page that existed before and is not a page any more is listed under "retired",
pointing at the page holding the town it was named after (`/kansas-city-kansas/`
→ the page holding Kansas City, Kansas) — or, when no such town has a venue, at
the page now holding MOST of its venues. Retired slugs are kept across builds
and re-pointed when the page they landed on itself goes.

    python3 data/us/scripts/build_pages.py
    python3 data/us/scripts/test_build_pages.py
    python3 data/us/scripts/generate_roster.py

The build refuses a venue more than 1,500 km from the rest of its state (a
flipped longitude sign, a record filed under the wrong state) and a town placed
more than 60 km from every venue it holds — the shape a gazetteer namesake takes.
"""
import collections
import json
import pathlib
import sys

ROOT = pathlib.Path(__file__).resolve().parents[3]
DATA = ROOT / "data" / "us"
OUT = DATA / "pages.json"
sys.path.insert(0, str(ROOT / "data" / "scripts"))
import retired_venues  # noqa: E402
import town_pages  # noqa: E402
from town_pages import km  # noqa: E402,F401  (the tests' spelling)

MAJOR_MIN_POPULATION = 100_000

RULES = town_pages.Rules(two_venue_radius_km=20, one_venue_radius_km=30, absorb_radius_km=40)

# What a page anchored on a GeoNames place is called where GeoNames' name is not
# the one a visitor uses — and so its slug: `/new-york/` has been the URL since
# the US launched.
DISPLAY_NAMES = {"New York City, New York": "New York"}

# A venue this far from the median of its state's venues has coordinates that are
# not where it is. Each of the four this caught was contradicted by its own
# postcode: `Grand Theatre Perry` (Iowa) had a flipped longitude and sat in
# Mongolia; `Newport Performing Arts` is in Newport, Oregon, not Virginia.
MAX_VENUE_FROM_STATE_KM = 1500


def load_towns():
    """(venue slug → town key, town key → {name, state, lat, lon, pop})."""
    data = json.loads((DATA / "towns.json").read_text())
    towns = {key: {"name": t["name"], "state": t["state"], "lat": t["lat"], "lon": t["lon"], "pop": t["population"]}
             for key, t in data["towns"].items()}
    return data["venues"], towns


def roster_venues():
    """Every rostered venue, in venues.json order — the harvest less the retired
    and the few with no state, which no page can be filed under."""
    retired = retired_venues.load(DATA)
    return [v for v in json.loads((DATA / "venues.json").read_text())
            if v["slug"] not in retired and v.get("state")]


def check_coordinates(venues):
    """Refuse a venue whose coordinates cannot be where it is: one more than
    `MAX_VENUE_FROM_STATE_KM` from the median of its state's venues. A venue's
    coordinates decide its town and so its page, and a wrong one does not fail —
    it lands on a plausible page somewhere else."""
    by_state = collections.defaultdict(list)
    for v in venues:
        by_state[v["state"]].append(v)
    problems = []
    for state, vs in sorted(by_state.items()):
        median = (sorted(float(v["lat"]) for v in vs)[len(vs) // 2], sorted(float(v["lon"]) for v in vs)[len(vs) // 2])
        for v in vs:
            away = km((float(v["lat"]), float(v["lon"])), median)
            if away > MAX_VENUE_FROM_STATE_KM:
                problems.append(f"{state}: {v['slug']!r} is {away:,.0f} km from the state's other venues — check "
                                f"venues.json for a flipped longitude sign or a record filed under the wrong state")
    if problems:
        sys.exit("\n".join(problems))


def display(towns, key):
    return DISPLAY_NAMES.get(key, towns[key]["name"])


def build(venues, venue_town, towns, previous, prior_retired, rules=RULES):
    """venues: (Flicks slug, (lat, lon)) for every venue, in roster order;
    venue_town: slug → town key; towns: town key → {name, state, lat, lon, pop};
    previous: slug → {"cinemas", "lat", "lon", "qualifiedSlug", "anchor"} for every
    page the last build had (the metros, before the first)."""
    by_town = collections.defaultdict(list)
    for slug, _ in venues:
        by_town[venue_town[slug]].append(slug)
    unplaced = sorted(t for t in by_town if t not in towns)
    if unplaced:
        sys.exit(f"towns with no coordinates in towns.json (run geocode_towns.py): {unplaced}")

    # A namesake town is far from every venue it is supposed to hold.
    at = dict(venues)
    far = sorted(t for t, vs in by_town.items()
                 if min(km((towns[t]["lat"], towns[t]["lon"]), at[v]) for v in vs) > rules.max_drift_km)
    if far:
        sys.exit(f"towns placed > {rules.max_drift_km} km from every venue they hold (namesake?): {far} "
                 f"— fix towns.json")

    # Biggest first, so where two majors share a name (Springfield, Missouri and
    # Springfield, Massachusetts) the bigger keeps the bare slug.
    majors_here = sorted((t for t in by_town if towns[t]["pop"] >= MAJOR_MIN_POPULATION),
                         key=lambda t: (-towns[t]["pop"], t))

    def page(slug, kind, anchor, members):
        t = towns[anchor]
        return {"slug": slug, "kind": kind, "name": display(towns, anchor), "anchor": anchor, "state": t["state"],
                "qualifiedSlug": town_pages.slugify(f"{display(towns, anchor)} {t['state']}"),
                "lat": round(t["lat"], 4), "lon": round(t["lon"], 4), "towns": members, "cinemas": []}

    # …unless the bare slug already means another town: a slug a page had before,
    # or one retired since, keeps meaning the town it was named after while that
    # town has a venue. /springfield/ was Springfield, Illinois, and stays it;
    # Springfield, Missouri, the bigger, is /springfield-missouri/.
    reserved = town_pages.reserve({**prior_retired, **previous}, by_town)
    majors, major_town = {}, {}
    for m in majors_here:
        slug = town_pages.slugify(display(towns, m))
        if slug in majors or reserved.get(slug, m) != m:
            slug = town_pages.slugify(f"{display(towns, m)} {towns[m]['state']}")
        majors[slug] = page(slug, "major", m, [m])
        major_town[m] = slug
    result, page_of_town = town_pages.build(
        by_town, majors, major_town, towns,
        display=lambda anchor: display(towns, anchor),
        make_page=page,
        region_slug=lambda anchor: towns[anchor]["state"],
        cinema_of=lambda slug: slug,
        rules=rules,
        reserved=reserved)

    # A page gone since the last build is named after a town, and a visitor typing
    # /kansas-city-kansas/ wants Kansas City, Kansas: it goes to the page holding
    # that town. Only when no town by that name has a venue does it go to the page
    # now holding most of its venues (`town_pages.heir` has the tie-breaks).
    def heir(old):
        if old.get("anchor") in page_of_town:
            return page_of_town[old["anchor"]]
        held = collections.Counter(page_of_town[venue_town[v]] for v in old["cinemas"] if v in venue_town)
        return town_pages.heir(held, (old["lat"], old["lon"]), result)
    gone = {slug: heir(old) for slug, old in previous.items() if slug not in result}
    # A slug retired in an earlier build follows its town to whichever page holds
    # it now.
    retired = town_pages.retire(gone, {k: page_of_town.get(v.get("anchor"), v["page"])
                                       for k, v in prior_retired.items()}, result)
    remembered = {**prior_retired, **previous}
    retired = {slug: {"page": page, "qualifiedSlug": remembered[slug]["qualifiedSlug"],
                      "anchor": remembered[slug].get("anchor")}
               for slug, page in sorted(retired.items())}
    pages = list(result.values())
    pages.sort(key=lambda p: (p["kind"] != "major", majors_here.index(p["anchor"]) if p["kind"] == "major" else 0,
                              p["slug"]))
    return pages, retired


def reach(pages, towns):
    """Each page's furthest town from the town it is named after, in km."""
    return {p["slug"]: max(km((p["lat"], p["lon"]), (towns[t]["lat"], towns[t]["lon"])) for t in p["towns"])
            for p in pages}


def main():
    roster = roster_venues()
    check_coordinates(roster)
    venue_town, towns = load_towns()
    missing = sorted(v["slug"] for v in roster if v["slug"] not in venue_town)
    if missing:
        sys.exit(f"venues with no town in towns.json (run geocode_towns.py): {missing}")
    prior = json.loads(OUT.read_text())
    previous = {p["slug"]: p for p in prior["pages"]}
    pages, retired = build([(v["slug"], (float(v["lat"]), float(v["lon"]))) for v in roster], venue_town, towns,
                           previous, prior["retired"])
    OUT.write_text(json.dumps({"pages": pages, "retired": retired}, ensure_ascii=False, indent=1) + "\n",
                   encoding="utf-8")
    kinds = collections.Counter(p["kind"] for p in pages)
    sizes = sorted(len(p["cinemas"]) for p in pages)
    far = sorted(reach(pages, towns).values())
    print(f"Wrote {OUT.relative_to(ROOT)}: {len(pages)} pages {dict(kinds)}, {sum(sizes)} venues "
          f"(median {sizes[len(sizes) // 2]}, max {sizes[-1]} per page, {sizes.count(1)} with one), "
          f"furthest town {far[-1]:.0f} km, {sum(1 for d in far if d > 50)} pages reaching past 50 km, "
          f"{len(retired)} retired slugs")


if __name__ == "__main__":
    main()
