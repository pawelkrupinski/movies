#!/usr/bin/env python3
"""Build data/uk/pages.json — which UK listing page each venue belongs to.

A UK page used to be a Flicks REGION, filed by hand: mostly a county, so
`/west-yorkshire/` listed Leeds, Bradford, Halifax and Ilkley, and the
Highlands and Islands page reached 285 km from Inverness to Lerwick. It is now
the rule Poland, Spain, Germany and the US use, from the shared
`data/scripts/town_pages.py`, because people do not drive far just to see a film:

  1. A MAJOR city — a place of 100,000 people or more (GeoNames' figure in
     `towns.json`) — lists only the venues inside it.
  2. Any other town with at least 3 venues is a page of its own.
  3. Every remaining town is clustered: a 2-venue town takes the 1- and 2-venue
     towns within 10 km; the 1-venue towns left cluster within 25 km of the
     biggest of them; a town still alone joins the nearest cluster within 35 km.
  4. No venue is more than 40 km from the town its page is named after: a town
     the steps above would reach past that with is clustered again, or stands
     as a page of its own.

Poland's and Spain's radii, not the US's wider ones: British towns are close
together. A cluster is named after its anchor, the town with the most venues,
then the most people ("Penrith area"; the phrase is the Scala side's, `UkPage`).
No cluster crosses water (`towns.json`'s `landmass`): Ryde is 11 km from
Portsmouth across the Solent, Larne 35 km from Stranraer across the North
Channel.

ONE CONURBATION, MANY MAJORS. Postal towns cut a conurbation into the boroughs'
towns: Greater Manchester is Manchester, Salford, Stockport, Oldham, Bolton and
Wigan, each a major of its own, and the West Midlands Birmingham, Wolverhampton,
Dudley, Walsall, West Bromwich, Solihull and Sutton Coldfield. They stay
separate pages, as Katowice, Sosnowiec and Gliwice do in Poland and Essen and
Bochum in Germany: each is a town people name, its venues are its own, and a
page of all of Greater Manchester is the county page this replaced. GREATER
LONDON is the exception — one city, like New York's five boroughs, so every
place in it (Croydon, Romford, Kingston) is London (`geocode_towns.py`), and its
one page keeps its compass areas (`Cinema.londonAreas`).

A SLUG names one town. A page is slugged after its name; where another page or
another country has that slug it is qualified with its county
(`/newport-isle-of-wight/`, `/cambridge-cambridgeshire/`):

  - a slug a UK page had before keeps meaning its town (`town_pages.reserve`);
  - of two UK towns of one name the more populous takes the bare slug
    (`/bangor/` is County Down's, Gwynedd's is `/bangor-gwynedd/`);
  - a slug Poland, Germany, Spain or the US serves or redirects stays theirs —
    the UK, re-cut after them, is the newcomer (`/cambridge/` is Massachusetts').

Inputs, all committed:

  venues.json   every UK venue: its postal town off Flicks (`harvest_towns.py`),
                coordinates, and the `case object` the roster declares it as.
  towns.json    the town each venue is in and that town's GeoNames centre,
                population, county and landmass (`geocode_towns.py`). A venue
                missing there stops the build — geocode it first.

A page that existed before and is not a page any more is listed under "retired",
pointing at the page holding the town it was named after (`/manchester/` stays,
`/antrim/` → Antrim's page) — or, when no such town has a venue, at the page now
holding MOST of its venues (`/west-yorkshire/` → Leeds). Retired slugs are kept
across builds and re-pointed when the page they landed on itself goes.

    python3 data/uk/scripts/build_pages.py
    python3 data/uk/scripts/test_build_pages.py
    python3 data/uk/scripts/generate_roster.py
"""
import collections
import json
import pathlib
import sys

ROOT = pathlib.Path(__file__).resolve().parents[3]
DATA = ROOT / "data" / "uk"
OUT = DATA / "pages.json"
sys.path.insert(0, str(ROOT / "data" / "scripts"))
import town_pages  # noqa: E402
from town_pages import km  # noqa: E402,F401  (the tests' spelling)

MAJOR_MIN_POPULATION = 100_000

RULES = town_pages.Rules(max_reach_km=40)

#: The other countries' pages, whose slugs (live or redirecting) a UK page does
#: not take unless it held the slug first.
FOREIGN_PAGES = [ROOT / "data" / c / "pages.json" for c in ("pl", "germany", "spain", "us")]


def load_towns():
    """(cinemaObject → town key, town key → {name, nation, county, landmass, lat, lon, pop})."""
    data = json.loads((DATA / "towns.json").read_text())
    towns = {key: {"name": t["name"], "nation": t["nation"], "county": t["county"], "landmass": t["landmass"],
                   "lat": t["lat"], "lon": t["lon"], "pop": t["population"]}
             for key, t in data["towns"].items()}
    return data["venues"], towns


def roster_venues():
    """Every UK venue, in the order a page lists them: by display name."""
    venues = json.loads((DATA / "venues.json").read_text())
    return sorted(venues, key=lambda v: (v["displayName"].casefold(), v["cinemaObject"]))


def foreign_slugs():
    """Every slug another country's pages serve or redirect."""
    slugs = set()
    for path in FOREIGN_PAGES:
        data = json.loads(path.read_text())
        slugs.update(p["slug"] for p in data["pages"])
        slugs.update(data["retired"])
    return slugs


def build(venues, venue_town, towns, previous, prior_retired, foreign=frozenset(), rules=RULES):
    """venues: (cinemaObject, (lat, lon)) for every venue, in roster order;
    venue_town: cinemaObject → town key; towns: town key → {name, nation, county,
    landmass, lat, lon, pop}; previous: slug → {"cinemas", "lat", "lon",
    "qualifiedSlug", "anchor"} for every page the last build had (the hand-filed
    regions, before the first); foreign: the slugs other countries hold."""
    by_town = collections.defaultdict(list)
    for obj, _ in venues:
        by_town[venue_town[obj]].append(obj)
    unplaced = sorted(t for t in by_town if t not in towns)
    if unplaced:
        sys.exit(f"towns with no coordinates in towns.json (run geocode_towns.py): {unplaced}")
    at = dict(venues)

    def centre(t):
        return towns[t]["lat"], towns[t]["lon"]

    def reach(t, anchor):
        """How far town `t`'s furthest venue is from `anchor`."""
        return max(km(centre(anchor), at[v]) for v in by_town[t])

    far = sorted(t for t in by_town if min(km(centre(t), at[v]) for v in by_town[t]) > rules.max_drift_km)
    if far:
        sys.exit(f"towns placed > {rules.max_drift_km} km from every venue they hold (namesake?): {far} "
                 f"— fix towns.json")

    def qualified(t):
        return town_pages.slugify(f"{towns[t]['name']} {towns[t]['county']}")

    held = set(previous) | set(prior_retired)
    # A slug keeps meaning the town it was named after (`town_pages.reserve`); of
    # two UK towns sharing a name the more populous has the bare slug; and a slug
    # another country has is not the UK's to take.
    reserved = {slug: "<another country>" for slug in foreign - held}
    by_name = collections.defaultdict(list)
    for t in by_town:
        by_name[town_pages.slugify(towns[t]["name"])].append(t)
    for slug, ts in by_name.items():
        if len(ts) > 1:
            reserved.setdefault(slug, max(ts, key=lambda t: (towns[t]["pop"], t)))
    reserved.update(town_pages.reserve({**prior_retired, **previous}, by_town))

    majors_here = sorted((t for t in by_town if towns[t]["pop"] >= MAJOR_MIN_POPULATION),
                         key=lambda t: (-towns[t]["pop"], t))

    def page(slug, kind, anchor, members):
        t = towns[anchor]
        return {"slug": slug, "kind": kind, "name": t["name"], "anchor": anchor, "nation": t["nation"],
                "county": t["county"], "qualifiedSlug": qualified(anchor),
                "lat": round(t["lat"], 4), "lon": round(t["lon"], 4), "towns": members, "cinemas": []}

    majors, major_town = {}, {}
    for m in majors_here:
        slug = town_pages.slugify(towns[m]["name"])
        if slug in majors or reserved.get(slug, m) != m:
            slug = qualified(m)
        majors[slug] = page(slug, "major", m, [m])
        major_town[m] = slug
    result, page_of_town = town_pages.build(
        by_town, majors, major_town, towns,
        display=lambda anchor: towns[anchor]["name"],
        make_page=page,
        region_slug=lambda anchor: towns[anchor]["county"],
        cinema_of=lambda obj: obj,
        rules=rules,
        landmass=lambda t: towns[t]["landmass"],
        reserved=reserved,
        reach=reach)
    taken = sorted(s for s in result if s in foreign and s not in held)
    if taken:
        sys.exit(f"pages took slugs another country holds, qualified or not: {taken}")
    beyond = [f"{p['slug']}: {obj} is {km((p['lat'], p['lon']), at[obj]):.0f} km from {p['anchor']}"
              for p in result.values() for obj in p["cinemas"]
              if rules.max_reach_km is not None and km(centre(p["anchor"]), at[obj]) > rules.max_reach_km]
    if beyond:
        sys.exit(f"venues past the {rules.max_reach_km:.0f} km cap:\n" + "\n".join(beyond))

    # A page gone since the last build is named after a town or a county; a
    # visitor typing /antrim/ wants Antrim, so it goes to the page holding that
    # town, and only when no town by that name has a venue to the page now holding
    # most of its venues (`town_pages.heir` has the tie-breaks).
    def heir(old):
        if old.get("anchor") in page_of_town:
            return page_of_town[old["anchor"]]
        held_now = town_pages.held_by(old["cinemas"], venue_town, page_of_town)
        return town_pages.heir(held_now, (old["lat"], old["lon"]), result)
    gone = {slug: heir(old) for slug, old in previous.items() if slug not in result}
    retired = town_pages.retire(gone, {k: page_of_town.get(v.get("anchor"), v["page"])
                                       for k, v in prior_retired.items()}, result)
    remembered = {**prior_retired, **previous}
    retired = {slug: {"page": target, "qualifiedSlug": remembered[slug]["qualifiedSlug"],
                      "anchor": remembered[slug].get("anchor")}
               for slug, target in sorted(retired.items())}
    pages = list(result.values())
    pages.sort(key=lambda p: (p["kind"] != "major", majors_here.index(p["anchor"]) if p["kind"] == "major" else 0,
                              p["slug"]))
    return pages, retired


def reach_of(pages, venues):
    """Each page's furthest venue from the town it is named after, in km."""
    at = dict(venues)
    return {p["slug"]: max(km((p["lat"], p["lon"]), at[v]) for v in p["cinemas"]) for p in pages}


def main():
    roster = roster_venues()
    venue_town, towns = load_towns()
    missing = sorted(v["cinemaObject"] for v in roster if v["cinemaObject"] not in venue_town)
    if missing:
        sys.exit(f"venues with no town in towns.json (run geocode_towns.py): {missing}")
    prior = json.loads(OUT.read_text())
    previous = {p["slug"]: p for p in prior["pages"]}
    venues = [(v["cinemaObject"], (float(v["lat"]), float(v["lon"]))) for v in roster]
    pages, retired = build(venues, venue_town, towns, previous, prior["retired"], foreign_slugs())
    problems = town_pages.placement_problems([obj for obj, _ in venues], pages)
    if problems:
        sys.exit("\n".join(problems))
    OUT.write_text(json.dumps({"pages": pages, "retired": retired}, ensure_ascii=False, indent=1) + "\n",
                   encoding="utf-8")
    kinds = collections.Counter(p["kind"] for p in pages)
    sizes = sorted(len(p["cinemas"]) for p in pages)
    far = sorted(reach_of(pages, venues).values())
    print(f"Wrote {OUT.relative_to(ROOT)}: {len(pages)} pages {dict(kinds)}, {sum(sizes)} venues "
          f"(median {sizes[len(sizes) // 2]}, max {sizes[-1]} per page, {sizes.count(1)} with one), "
          f"furthest venue: median {far[len(far) // 2]:.0f} km, p95 {far[int(len(far) * 0.95)]:.0f} km, "
          f"max {far[-1]:.0f} km; {len(retired)} retired slugs")


if __name__ == "__main__":
    main()
