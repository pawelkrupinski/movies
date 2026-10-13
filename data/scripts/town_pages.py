"""Which listing page each venue belongs to — the rule Poland, Spain, Germany, the US and the UK share.

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
     Only 1- and 2-venue towns ever cluster: a major or a town with
     `own_page_min_venues`+ venues never takes in a neighbour, however close.
  4. Where the country sets `max_reach_km` (the UK: 40), no venue on a cluster
     page is further than that from the town the page is named after. The steps
     above measure town centre to town centre, and a venue can sit well out from
     its town's — so a town whose venues are past the cap leaves its
     cluster, are clustered again among themselves by the same steps, and a
     1-venue town still alone joins the nearest cluster it is within reach of,
     else stands as a page of its own (Malton, whose cinema is 40.4 km from
     Beverley, the cluster it would otherwise join).

A cluster is named and slugged after its anchor — the town with the most venues,
then the most people. Distances are straight-line (haversine) between gazetteer
town centres.

What differs per country is only at the edges, and is the caller's: how a
venue's town is read, which cities are major, the radii (Germany's tighter, the US's wider),
how a name becomes a slug, what a page records beside its towns and venues
(Poland's locative, Spain's province, Germany's Bundesland, the US's state, the UK's
county), how far a page may reach (the UK's cap), and where a page that stopped existing
now redirects. `data/{pl,spain,germany,us,uk}/scripts/build_pages.py` are those callers.
"""
import collections
import math
import re
import sys
import unicodedata
from dataclasses import dataclass
from decimal import ROUND_HALF_EVEN, Decimal


@dataclass(frozen=True)
class Rules:
    own_page_min_venues: int = 3
    two_venue_radius_km: float = 10
    one_venue_radius_km: float = 25
    absorb_radius_km: float = 35
    # A town placed further than this from the page its venues were on before has
    # almost certainly been matched to a gazetteer namesake.
    max_drift_km: float = 60
    # No venue on a cluster page further than this from the page's anchor town
    # (step 4 above). None — Poland, Spain, Germany and the US — holds none.
    max_reach_km: float = None


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


def cluster(count, pos, pop, rules=Rules(), landmass=None, reach=None):
    """Group the non-major towns into pages: a list of clusters, each a list of
    towns with its anchor first. `count`, `pos` and `pop` map each town to its
    venue count, (lat, lon) and population.

    `landmass` (town → key, optional) keeps towns a short line but a sea apart off
    one page: Ceuta is 28 km from Algeciras across the Strait, Fuerteventura 11 km
    from Lanzarote. Towns with different keys are never within any radius.

    `reach` ((town, anchor) → km, optional) is how far a town's furthest venue is
    from an anchor town, which `rules.max_reach_km` caps; by default the distance
    between the two towns' centres."""
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
    if rules.max_reach_km is not None:
        final = within_reach(final, count, pos, pop, rules, dist, reach or (lambda t, a: km(pos[t], pos[a])),
                             landmass)
    return final


def within_reach(clusters, count, pos, pop, rules, dist, reach, landmass):
    """Step 4 of the rule: `clusters` with every town further than
    `rules.max_reach_km` (by `reach`) from its cluster's anchor taken out and
    placed again — clustered among themselves by the same rule (which caps them
    in turn), then a 1-venue town left alone joining the nearest cluster whose
    anchor it is within reach of, as `cluster`'s absorb step would. It only joins
    a cluster whose anchor still outranks it, so no anchor — and so no distance
    already checked — changes."""
    def ranks(t):
        return -count[t], -pop[t], t
    kept, out = [], []
    for c in clusters:
        kept.append([t for t in c if t == c[0] or reach(t, c[0]) <= rules.max_reach_km])
        out.extend(t for t in c if t not in kept[-1])
    if not out:
        return clusters
    final = list(kept)
    for c in cluster({t: count[t] for t in out}, pos, pop, rules, landmass, reach):
        t = c[0]
        if len(c) == 1 and count[t] == 1:
            near = [m for m in kept if len(m) > 1 and ranks(m[0]) < ranks(t)
                    and dist(t, m[0]) <= rules.absorb_radius_km and reach(t, m[0]) <= rules.max_reach_km]
            if near:
                joined = min(near, key=lambda m: (dist(t, m[0]), m[0]))
                joined.append(t)
                joined.sort(key=ranks)
                continue
        final.append(c)
    return final


def build(by_town, majors, major_town, gaz, display, make_page, region_slug, cinema_of, rules=Rules(),
          landmass=None, slug_of=slugify, reserved=None, reach=None):
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
    reach:      see `cluster`
    slug_of:    name → URL slug (German spells ä as "ae" where Polish and Spanish drop the accent)
    reserved:   slug → the town only a page anchored there may take it for (see `reserve`)
    """
    reserved = reserved or {}
    result = dict(majors)
    minor = {t: vs for t, vs in by_town.items() if t not in major_town}
    count = {t: len(vs) for t, vs in minor.items()}
    pos = {t: (gaz[t]["lat"], gaz[t]["lon"]) for t in minor}
    pop = {t: gaz[t]["pop"] for t in minor}

    used = set(result)
    for c in cluster(count, pos, pop, rules, landmass, reach):
        anchor = c[0]
        slug = slug_of(display(anchor))
        if slug in used or reserved.get(slug, anchor) != anchor:
            slug = f"{slug}-{slug_of(region_slug(anchor))}"
            if slug in used:
                sys.exit(f"two pages want the slug {slug!r}, qualified or not")
        used.add(slug)
        result[slug] = make_page(slug, "town" if len(c) == 1 else "cluster", anchor, c)

    page_of_town = dict(major_town)
    for slug, p in result.items():
        for t in p["towns"]:
            page_of_town[t] = slug
    for t, vs in by_town.items():
        result[page_of_town[t]]["cinemas"].extend(cinema_of(v) for v in vs)
    return result, page_of_town


def reserve(previous, towns):
    """slug → the town it must keep meaning: each earlier page's slug, held for the
    town the page was named after while that town still has a venue. A URL is
    published — a cookie, a bookmark, an index entry — so when two towns share a
    name (Springfield, Illinois and Springfield, Missouri) the one whose page had
    the bare slug keeps it, however the other compares, and a namesake that never
    had it is the one qualified. `previous`: slug → {"anchor", …}."""
    return {slug: p["anchor"] for slug, p in previous.items() if p.get("anchor") in towns}


def drifted(towns, pos, previous_centres, max_km):
    """The towns placed further than `max_km` from every centre of the pages their
    venues were on before — the shape a gazetteer namesake produces."""
    return [t for t in towns
            if previous_centres(t) and min(km(pos[t], c) for c in previous_centres(t)) > max_km]


def held_by(cinemas, venue_town, page_of_town):
    """page → how many of a gone page's `cinemas` it holds now — the `held` that
    `heir` takes. A venue whose town is on no page counts for none: its town's last
    cinema was retired, and the gazetteer (`venue_town`) still places it, because
    it keeps every venue it ever placed."""
    return collections.Counter(page_of_town[venue_town[v]] for v in cinemas if venue_town.get(v) in page_of_town)


def heir(held, centre, result):
    """The page a page that stopped existing now redirects to: the one holding
    most of its venues (`held`, page → how many); on a tie, a major city's, then
    the one nearest where the old page was centred. When none of its venues is
    left — the one cinema of a town page closed — simply the page nearest it."""
    def near(s):
        return -km(centre, (result[s]["lat"], result[s]["lon"]))
    if not held:
        return max(result, key=lambda s: (near(s), s))
    return max(held, key=lambda s: (held[s], result[s]["kind"] == "major", near(s), s))


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


def succession(previous, prior_retired, result, page_of_town, venue_town, carry=()):
    """The `retired` map a build writes: slug → {"page", "qualifiedSlug", "anchor",
    *carry} for every page that existed before and is not a page any more.

    Each goes where `heir_of` says — the page holding the town it was named after,
    else the page now holding most of its former venues — and that is re-derived
    on every build for an earlier retirement too, as long as the entry carries its
    `cinemas`, `lat` and `lon` (pass them in `carry` to keep them): a retired
    region follows its venues when they move again. An entry with neither an
    anchor on a page nor its venues keeps the page it pointed at (`retire`
    re-points it when that page itself goes)."""
    def heir_of(old):
        if old.get("anchor") in page_of_town:
            return page_of_town[old["anchor"]]
        if "cinemas" in old:
            return heir(held_by(old["cinemas"], venue_town, page_of_town), (old["lat"], old["lon"]), result)
        return None
    gone = {slug: heir_of(old) for slug, old in previous.items() if slug not in result}
    prior = {slug: heir_of(old) or old["page"] for slug, old in prior_retired.items()}
    retired = retire(gone, prior, result)
    remembered = {**prior_retired, **previous}
    return {slug: {"page": target, "qualifiedSlug": remembered[slug]["qualifiedSlug"],
                   "anchor": remembered[slug].get("anchor"),
                   **{k: remembered[slug][k] for k in carry if k in remembered[slug]}}
            for slug, target in sorted(retired.items())}


def sorted_pages(result, majors_here):
    """pages.json's order: the majors biggest first (`majors_here`'s order), then
    every other page by slug."""
    return sorted(result.values(),
                  key=lambda p: (p["kind"] != "major", majors_here.index(p["anchor"]) if p["kind"] == "major" else 0,
                                 p["slug"]))


def refuse_namesakes(by_town, centre, at, max_km):
    """Stop the build on a town placed more than `max_km` from every venue it holds
    — the shape a gazetteer namesake takes. `centre`: town → (lat, lon); `at`:
    venue → (lat, lon)."""
    far = sorted(t for t, vs in by_town.items() if min(km(centre(t), at[v]) for v in vs) > max_km)
    if far:
        sys.exit(f"towns placed > {max_km} km from every venue they hold (namesake?): {far} — fix towns.json")


def exact_mean(values):
    """Mean of decimal strings, rounded half-even to five places — the same answer
    on every Python."""
    values = [Decimal(str(v)) for v in values]
    return float((sum(values, Decimal(0)) / len(values)).quantize(Decimal("0.00001"), rounding=ROUND_HALF_EVEN))


def placement_problems(venues, pages):
    """Every venue on exactly one page, and every page's venues in the roster —
    what a roster generator checks pages.json against before emitting it: a venue
    on no page is never scraped, and nothing else says so."""
    on = collections.Counter(name for p in pages for name in p["cinemas"])
    return ([f"venue {name!r} is on {n} pages" for name, n in sorted(on.items()) if n > 1] +
            [f"venue {name!r} is on no page — rebuild pages.json" for name in sorted(set(venues) - set(on))] +
            [f"page venue {name!r} is not in the roster" for name in sorted(set(on) - set(venues))])
