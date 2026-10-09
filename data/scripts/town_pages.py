"""Which listing page each venue belongs to — the rule Poland and Spain share.

A country's pages are built from its venues' towns:

  1. A MAJOR city lists only the venues inside that city.
  2. Any other town with at least `own_page_min_venues` venues is a page of its own.
  3. Every remaining town is clustered:
     a. towns with 2 venues anchor a cluster that takes every 1- or 2-venue
        town within `two_venue_radius_km` of them (biggest anchors first);
     b. the 1-venue towns left over cluster among themselves within
        `one_venue_radius_km` of the biggest of them (by population);
     c. a 1-venue town still alone after that joins the nearest multi-town
        cluster within `absorb_radius_km`, if there is one. A 2-venue town that
        found nobody stays a page of its own, like a 3-venue one.

A cluster is named and slugged after its anchor — the town with the most venues,
then the most people. Distances are straight-line (haversine) between gazetteer
town centres.

What differs per country is only at the edges, and is the caller's: how a
venue's town is read, which cities are major, what a page records beside its
towns and venues (Poland's locative, Spain's province), and where a page that
stopped existing now redirects. `data/pl/scripts/build_pages.py` and
`data/spain/scripts/build_pages.py` are those callers.
"""
import math
import re
import sys
import unicodedata
from dataclasses import dataclass


@dataclass(frozen=True)
class Rules:
    own_page_min_venues: int = 3
    two_venue_radius_km: float = 10
    one_venue_radius_km: float = 25
    absorb_radius_km: float = 35
    # A town placed further than this from the page its venues were on before has
    # almost certainly been matched to a gazetteer namesake.
    max_drift_km: float = 60


def fold(s):
    s = s.replace("ł", "l").replace("Ł", "L")
    return "".join(c for c in unicodedata.normalize("NFD", s) if unicodedata.category(c) != "Mn").lower()


def slugify(name):
    return re.sub(r"[^a-z0-9]+", "-", fold(name)).strip("-")


def km(a, b):
    """Great-circle kilometres between two (lat, lon) points."""
    p = math.radians
    return 2 * 6371 * math.asin(math.sqrt(math.sin(p(b[0] - a[0]) / 2) ** 2 +
                                          math.cos(p(a[0])) * math.cos(p(b[0])) * math.sin(p(b[1] - a[1]) / 2) ** 2))


def cluster(count, pos, pop, rules=Rules(), landmass=None):
    """Group the non-major towns into pages: a list of clusters, each a list of
    towns with its anchor first. `count`, `pos` and `pop` map each town to its
    venue count, (lat, lon) and population.

    `landmass` (town → key, optional) keeps towns a short line but a sea apart off
    one page: Ceuta is 28 km from Algeciras across the Strait, Fuerteventura 11 km
    from Lanzarote. Towns with different keys are never within any radius."""
    def dist(a, b):
        if landmass and landmass(a) != landmass(b):
            return math.inf
        return km(pos[a], pos[b])
    own = sorted(t for t in count if count[t] >= rules.own_page_min_venues)
    pool = set(count) - set(own)
    clusters = [[t] for t in own]

    def grab(anchor, radius, eligible):
        members = [t for t in pool if t in eligible and dist(anchor, t) <= radius]
        members.sort(key=lambda t: (t != anchor, -count[t], -pop[t], t))
        pool.difference_update(members)
        return members

    two = {t for t in pool if count[t] == 2}
    while pool & two:
        anchor = max(pool & two, key=lambda t: (pop[t], t))
        clusters.append(grab(anchor, rules.two_venue_radius_km, set(pool)))
    while pool:
        anchor = max(pool, key=lambda t: (pop[t], t))
        clusters.append(grab(anchor, rules.one_venue_radius_km, set(pool)))

    multi = [c for c in clusters if len(c) > 1]
    final = []
    for c in clusters:
        if len(c) == 1 and count[c[0]] == 1 and multi:
            nearest = min(multi, key=lambda m: dist(c[0], m[0]))
            if dist(c[0], nearest[0]) <= rules.absorb_radius_km:
                nearest.append(c[0])
                continue
        final.append(c)
    # The anchor — the town the page is named after — is the one with the most
    # venues, then the most people.
    for c in final:
        c.sort(key=lambda t: (-count[t], -pop[t], t))
    return final


def build(by_town, majors, major_town, gaz, display, make_page, region_slug, cinema_of, rules=Rules(),
          landmass=None):
    """Every page, as {slug: page}, and the page each town is on.

    by_town:    town → its venues, in roster order (the order a page lists them in)
    majors:     slug → the major city's page, its "cinemas" still empty
    major_town: town → the slug of the major city it is inside
    gaz:        town → {"lat", "lon", "pop"} for every non-major town
    display:    anchor → the name a page anchored there is called (and slugged) by
    make_page:  (slug, kind, anchor, towns) → the page record for a town or cluster
    region_slug: anchor → what qualifies its slug when another page has it already
    cinema_of:  venue → what the page's "cinemas" lists for it
    landmass:   see `cluster`
    """
    result = dict(majors)
    minor = {t: vs for t, vs in by_town.items() if t not in major_town}
    count = {t: len(vs) for t, vs in minor.items()}
    pos = {t: (gaz[t]["lat"], gaz[t]["lon"]) for t in minor}
    pop = {t: gaz[t]["pop"] for t in minor}

    used = set(result)
    for c in cluster(count, pos, pop, rules, landmass):
        anchor = c[0]
        slug = slugify(display(anchor))
        if slug in used:
            slug = f"{slug}-{slugify(region_slug(anchor))}"
        used.add(slug)
        result[slug] = make_page(slug, "town" if len(c) == 1 else "cluster", anchor, c)

    page_of_town = dict(major_town)
    for slug, p in result.items():
        for t in p["towns"]:
            page_of_town[t] = slug
    for t, vs in by_town.items():
        result[page_of_town[t]]["cinemas"].extend(cinema_of(v) for v in vs)
    return result, page_of_town


def drifted(towns, pos, previous_centres, max_km):
    """The towns placed further than `max_km` from every centre of the pages their
    venues were on before — the shape a gazetteer namesake produces."""
    return [t for t in towns
            if previous_centres(t) and min(km(pos[t], c) for c in previous_centres(t)) > max_km]


def retire(previous, prior_retired, result):
    """Where each page that existed before and is not a page any more now points.

    previous:      slug → the page now holding what it held, for every page the
                   previous build (or the roster before the first) had
    prior_retired: the previous build's own retired map — kept across builds (a
                   URL outlives the re-cluster that retired it), and re-pointed when
                   the page an earlier retirement landed on has itself moved on
    """
    retired = {slug: target for slug, target in previous.items() if slug not in result}
    for slug, target in prior_retired.items():
        if slug not in result:
            retired[slug] = target if target in result else retired.get(target, target)
    missing = {s: t for s, t in retired.items() if t not in result}
    if missing:
        sys.exit(f"retired slugs point at pages that no longer exist: {missing}")
    return retired
