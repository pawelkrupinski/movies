"""Distance-cluster a state's venues into the metro areas its picker groups by.

Flicks files every venue under a `region_slug`, but those 567 raw metros are far
too granular to group a picker by: several separate slugs cover one commuter
area (`dallas` and `fort-worth` are one metroplex a resident drives across
daily), a third of them hold one or two cinemas, and 788 venues carry no slug at
all. Grouping on them produced 607 areas across the 46 split states, 159 of them
holding two cinemas or fewer, plus a per-state "Other areas" catch-all.

Every venue carries `lat`/`lon`, so the grouping can be geography instead. This
is the US counterpart of `data/germany/scripts/cluster_regions.py` and follows
it: rank the state's towns, greedily pick hubs, attach every other town to the
nearest hub within a radius. Two differences, both because the US sprawls:

- **The radius is 75 km, not Germany's 35.** Tuned against the outcome, not in
  the abstract. It is the smallest radius that still merges the travel-sheds a
  resident treats as one place — Dallas + Fort Worth, the whole SF Bay — while
  keeping apart the ones they don't: greater LA stays separate from the Inland
  Empire (~85 km out), Philadelphia from the Lehigh Valley. At 90 km both of
  those over-merge (LA swallows the Inland Empire into a 175-cinema area); at
  60 km the Dallas metroplex splits in two.
- **A second FOLD pass.** A hub-and-radius pass alone strands rural towns as
  one-cinema areas — 161 of them at 75 km, worse fragmentation than the raw
  metros it replaces. So any cluster left holding fewer than three venues is
  folded into its nearest neighbouring cluster if one is within twice the
  radius; an isolated small-town cinema is filed under the metro people actually
  drive to. Only a venue with no neighbouring metro within 150 km keeps an area
  of its own, which is the honest answer for it.

Clusters never cross a state line, because a state is the `City` the areas
partition — so the New York and Kansas City metros are each split at their
border. See `data/us/README.md`.
"""
import math
import unicodedata
import re
from collections import Counter, defaultdict

#: Attach a town to a hub within this many km of it. See the module docstring.
RADIUS_KM = 75.0
#: A cluster holding fewer than this many venues is not a metro, it is a row of
#: chrome; the fold pass merges it into its nearest neighbour.
MIN_CLUSTER_VENUES = 3
#: …but only that far away. Twice the cluster radius: beyond it the venue is
#: genuinely isolated and keeps its own area rather than being filed under a
#: metro nobody would drive from it to.
FOLD_RADIUS_KM = 2 * RADIUS_KM


def haversine_km(a, b):
    """Great-circle km between two (lat, lon) pairs."""
    radius = 6371.0
    lat1, lon1 = a
    lat2, lon2 = b
    p1, p2 = math.radians(lat1), math.radians(lat2)
    dphi = math.radians(lat2 - lat1)
    dlambda = math.radians(lon2 - lon1)
    h = math.sin(dphi / 2) ** 2 + math.cos(p1) * math.cos(p2) * math.sin(dlambda / 2) ** 2
    return 2 * radius * math.asin(math.sqrt(h))


def slugify(label):
    """Mirror of Scala `tools.Slugify.stable` — deburr, lower-case, hyphenate.

    The roster does NOT ship this slug: `UsRoster` re-derives it from the label
    with the real `Slugify.stable`, which is the frozen fold clients persist an
    area under. This copy exists only so the generator can prove two of a
    state's labels will not collapse onto one slug before it emits them.
    """
    folded = ''.join(c for c in unicodedata.normalize('NFKD', label)
                     if not unicodedata.combining(c)).lower()
    return re.sub(r'(^-|-$)', '', re.sub(r'[^a-z0-9]+', '-', folded))


def _towns(venues):
    """A state's venues collapsed to towns: {city: {'pt', 'venues'}}.

    Clustering is per TOWN rather than per venue so a town's cinemas can never be
    split across two metros — the multiplex and the arthouse across the street
    belong to the same place by definition. A town's point is the centroid of
    its own venues, which is where its cinemas are rather than where a gazetteer
    puts its city hall.
    """
    grouped = defaultdict(list)
    for venue in venues:
        grouped[venue['city']].append(venue)
    return {
        city: {
            'pt': (sum(v['lat'] for v in members) / len(members),
                   sum(v['lon'] for v in members) / len(members)),
            'venues': members,
        }
        for city, members in sorted(grouped.items())
    }


def _cluster(towns):
    """{town: hub town} — greedy hub assignment at [[RADIUS_KM]], then the fold.

    Towns are ranked by their own cinema count, then by how many cinemas sit
    within a radius of them, then by name. The count picks out the real cities;
    the density tiebreak decides the long tail of one-cinema towns, where a name
    ordering would put whichever town sorts first alphabetically at the centre
    of its region. Both are pure functions of the input, so the ranking — and
    everything downstream of it — is deterministic.
    """
    names = list(towns)
    distance = {(a, b): haversine_km(towns[a]['pt'], towns[b]['pt'])
                for a in names for b in names}
    size = {c: len(towns[c]['venues']) for c in names}
    density = {c: sum(size[o] for o in names if distance[(c, o)] <= RADIUS_KM) for c in names}
    ranked = sorted(names, key=lambda c: (-size[c], -density[c], c))

    hubs, assigned = [], {}
    for town in ranked:
        nearest = min(hubs, key=lambda h: (distance[(town, h)], h), default=None)
        if nearest is not None and distance[(town, nearest)] <= RADIUS_KM:
            assigned[town] = nearest
        else:
            hubs.append(town)
            assigned[town] = town

    # Fold: smallest cluster first, so a two-cinema cluster absorbing a
    # one-cinema neighbour is reconsidered on the next pass rather than being
    # frozen under the threshold by the order it was visited in.
    while True:
        held = Counter()
        for town, hub in assigned.items():
            held[hub] += size[town]
        starved = sorted((h for h in hubs if held[h] < MIN_CLUSTER_VENUES),
                         key=lambda h: (held[h], h))
        merged = False
        for hub in starved:
            others = [o for o in hubs if o != hub]
            if not others:
                continue
            nearest = min(others, key=lambda o: (distance[(hub, o)], o))
            if distance[(hub, nearest)] > FOLD_RADIUS_KM:
                continue
            for town, h in assigned.items():
                if h == hub:
                    assigned[town] = nearest
            hubs.remove(hub)
            merged = True
            break
        if not merged:
            return assigned


def _preferred_label(hub, members, metro_labels, state):
    """What a cluster wants to be called: the place it is centred on.

    Its hub town's name is the FALLBACK, not the first choice, because Flicks'
    metro label usually reads that place better than any one town in it does.
    The Dallas metroplex clusters on Fort Worth but is called "Dallas Fort
    Worth"; greater Cleveland clusters on Akron, whose own cinemas outnumber
    Cleveland's only because Cleveland's are spread over a dozen named suburbs.
    So the label is the metro MOST of the cluster's venues are filed under, with
    the hub's own metro breaking a tie, and the hub town's name when the whole
    cluster is venues Flicks filed under no metro at all — or when that metro is
    named after the state itself (Flicks files most of Puerto Rico under
    `puerto-rico`), which inside that state's own picker says nothing.
    """
    metros = Counter(v['metro'] for t in members.values() for v in t['venues'] if v['metro'])
    if not metros:
        return hub
    own = {v['metro'] for v in members[hub]['venues'] if v['metro']}
    top = min(metros.items(), key=lambda kv: (-kv[1], kv[0] not in own, kv[0]))[0]
    return hub if metro_labels[top] == state else metro_labels[top]


def metros_for_state(state, venues, metro_labels):
    """{venue slug: metro label} for one state's venues.

    `metro_labels` is that state's `metros.labels_by_slug` entry. Labels have to
    be unique within the state — they name a `CinemaArea` the picker groups by —
    and several clusters can want the same one, because a rural Flicks metro is
    a catch-all covering half a state (every cluster on Colorado's eastern
    plains is filed under `denver`). Labels are handed out in one pass: the
    cluster CENTRED on the place goes first (the metro named "Grand Junction"
    should be the one Grand Junction is in), then the biggest, since that is the
    one a visitor looking for that metro means. A cluster that finds its label
    taken falls back to its hub town's name, distinct by construction, and the
    generator dies if even that is not enough.
    """
    towns = _towns(venues)
    assigned = _cluster(towns)
    members = defaultdict(dict)
    for town, hub in sorted(assigned.items()):
        members[hub][town] = towns[town]
    held = {hub: sum(len(t['venues']) for t in m.values()) for hub, m in members.items()}
    preferred = {hub: _preferred_label(hub, m, metro_labels, state)
                 for hub, m in sorted(members.items())}

    labels, taken = {}, set()
    for hub in sorted(members, key=lambda h: (preferred[h] != h, -held[h], h)):
        for candidate in (preferred[hub], hub):
            if candidate not in taken:
                labels[hub] = candidate
                taken.add(candidate)
                break
        else:
            raise SystemExit(f"{state}: cluster on {hub!r} cannot be named uniquely "
                             f"({preferred[hub]!r} and {hub!r} both taken)")

    for slug, group in _by_slug(labels.values()).items():
        if len(group) > 1:
            raise SystemExit(f"{state}: metros {sorted(group)} all slug to {slug!r}")

    return {v['slug']: labels[hub]
            for hub, m in members.items() for t in m.values() for v in t['venues']}


def _by_slug(labels):
    grouped = defaultdict(set)
    for label in labels:
        grouped[slugify(label)].add(label)
    return grouped


def _report():
    """Print the venue-count distribution the current constants produce.

    The tuning tool: `python3 data/us/scripts/cluster_metros.py` reports what
    moving RADIUS_KM / MIN_CLUSTER_VENUES does to the roster, so the radius is
    chosen against the outcome rather than in the abstract.
    """
    import json, sys, os
    sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
    from metros import labels_by_slug

    here = os.path.dirname(os.path.abspath(__file__))
    venues = json.load(open(os.path.join(here, '..', 'venues.json')))
    by_state = defaultdict(list)
    for v in venues:
        if v.get('state'):
            by_state[v['state']].append({
                'slug': v['slug'], 'city': v['city'], 'lat': float(v['lat']),
                'lon': float(v['lon']), 'metro': v.get('region_slug') or '',
            })
    labels = labels_by_slug({s: {v['metro'] for v in vs if v['metro']}
                             for s, vs in by_state.items()})
    print(f"radius={RADIUS_KM}km min={MIN_CLUSTER_VENUES} fold={FOLD_RADIUS_KM}km")
    sizes, per_state = [], {}
    for state in sorted(by_state):
        metros = metros_for_state(state, by_state[state], labels[state])
        counts = Counter(metros.values())
        per_state[state] = counts
        if len(by_state[state]) >= 30:   # UsRoster.MinCinemasToSplit
            sizes.extend(counts.values())
    print(f"areas in split states: {len(sizes)}  biggest {max(sizes)}  "
          f"one-venue {sum(1 for s in sizes if s == 1)}  "
          f"two-or-fewer {sum(1 for s in sizes if s <= 2)}  "
          f"median {sorted(sizes)[len(sizes) // 2]}")
    for state, counts in sorted(per_state.items(), key=lambda kv: -sum(kv[1].values()))[:8]:
        top = ', '.join(f"{n} {c}" for n, c in counts.most_common(4))
        print(f"  {state:14s} {sum(counts.values()):4d} venues -> {len(counts):3d} metros ({top})")


if __name__ == '__main__':
    _report()
