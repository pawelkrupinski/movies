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
border.

The same pass runs a SECOND time inside each metro too big to browse as one
list (`sub_areas_for_metro`), at 6 km instead of 75, splitting it into the
districts a local names — Manhattan, Brooklyn, Santa Monica, Fort Worth. See
`data/us/README.md`.
"""
import math
import unicodedata
import re
from collections import Counter, defaultdict
from datetime import datetime
from functools import lru_cache
from zoneinfo import ZoneInfo

#: Attach a town to a hub within this many km of it. See the module docstring.
RADIUS_KM = 75.0
#: A cluster holding fewer than this many venues is not a metro, it is a row of
#: chrome; the fold pass merges it into its nearest neighbour.
MIN_CLUSTER_VENUES = 3
#: …but only that far away. Twice the cluster radius: beyond it the venue is
#: genuinely isolated and keeps its own area rather than being filed under a
#: metro nobody would drive from it to.
FOLD_RADIUS_KM = 2 * RADIUS_KM

#: The metros big enough to be sub-divided into districts — see
#: `sub_areas_for_metro` and `models.UsMetroSubAreas`. Five clear it: Los
#: Angeles (133 venues), New York (102), San Francisco (79), Chicago and Dallas
#: Fort Worth (78 each); the next two down, Seattle (70) and Boston (62), do
#: not. 75 rather than a round 80 because San Francisco is 79 and is one of the
#: three the split was asked for — and nothing distinguishes Chicago and Dallas
#: Fort Worth at 78 from it.
MIN_VENUES_TO_SUBDIVIDE = 75
#: The same hub-and-radius pass, re-entered INSIDE one metro at a twelfth of the
#: metro radius. Tuned against the outcome exactly as the 75 km was: at 8 km the
#: five boroughs collapse into one 52-venue "New York" (Manhattan, Brooklyn, the
#: Bronx and half of Queens are all within 8 km of each other), which is the
#: whole thing this split exists to avoid; at 5 km greater Los Angeles shatters
#: into 32 districts, eleven of them holding two venues — Century City apart
#: from Los Angeles, Marina del Rey apart from Santa Monica. 6 km keeps the
#: boroughs apart AND keeps the LA districts the size of places people name.
SUB_RADIUS_KM = 6.0
#: A two-cinema district is a real place — the Bronx has exactly two — where a
#: two-cinema METRO is a row of chrome. So the sub-pass folds only the true
#: singletons, not everything under three.
MIN_SUB_CLUSTER_VENUES = 2
#: Three times the sub-radius, not the metro pass's two. A lone suburban cinema
#: in these metros typically sits 12-18 km from the next district's hub; at 2x
#: (12 km) a dozen of them across the five metros stay stranded as one-venue
#: areas, at 3x each is filed under the district a resident drives from. Beyond
#: 18 km it is genuinely on its own and keeps its own area — Avalon, on Catalina
#: Island, is the honest example.
SUB_FOLD_RADIUS_KM = 3 * SUB_RADIUS_KM

#: A town's venues must all fall within this of each other. Clustering collapses
#: a town to ONE point (see `_towns`), so a venue filed under the wrong town name
#: silently drags every cinema sharing that name to a centroid between them —
#: `Regal Largo Mall` (Largo, Pinellas County) arrived as `Key Largo` and put
#: itself AND the real Key Largo cinema 380 km away in the Naples metro. The
#: metro radius is the ceiling because a town wider than a whole metro's reach is
#: not a town; the widest real one is Houston at 56 km.
MAX_TOWN_SPAN_KM = RADIUS_KM
#: …and a venue must fall within this of its state's other venues. Catches the
#: errors a town-span check cannot see because the venue is alone in its town: a
#: flipped longitude sign (`Grand Theatre Perry`, Iowa, landed in Mongolia) and a
#: record filed under the wrong state entirely (`Newport Performing Arts` is in
#: Newport, OREGON — its own postcode says so — not Newport News, Virginia).
#: Generous on purpose: it is a sanity bound, not a clustering rule, and Alaska
#: legitimately reaches 1,100 km from its own median.
MAX_VENUE_FROM_STATE_KM = 1500.0
#: The two dates a timezone is compared on. One is not enough: Phoenix and Denver
#: share an offset in January and differ in July, because Arizona does not keep
#: DST — a single-date probe would call them the same zone.
TZ_PROBE_DATES = (datetime(2026, 1, 15, 12), datetime(2026, 7, 15, 12))

#: Folds the straight line says are one drive and the map says are not, as
#: `(state, starved hub, nearest hub)`. The fold pass measures great-circle km,
#: which is the right approximation almost everywhere the US puts cinemas — and
#: is not one where a mountain crest or open water lies across it.
#:
#: The bar is NOT "no road": it is that the metro's NAME would be a containment
#: no resident would say, which is the same test `SUB_AREA_REGIONS` applies one
#: level down. Southeast Alaska reaches Sitka, Petersburg and Wrangell only by
#: ferry or plane and is still listed as one place, because "Southeast Alaska" is
#: what that place is called and a visitor browsing it knows how they get around
#: it. "Fresno" is not what Bishop is in.
#:
#: - Bishop and Mammoth Lakes sit on US-395 on the EAST side of the Sierra, 59 km
#:   apart and a real pair. The fold hands them to Fresno, 120 km away in a
#:   straight line over the crest: the direct route is Tioga Pass, closed by snow
#:   roughly November to May, and the road that is open all year runs ~400 km
#:   round the south end of the range. They are the Eastern Sierra, which is why
#:   `UsRoster.MetroDisplayNames` renames the metro they keep.
#:
#: Every entry must actually fire — `generate_roster.py` refuses a stale one, the
#: way it refuses an unmapped sub-area region. A barrier that stopped applying is
#: a fold that changed under it, and the next reader should be told rather than
#: left reading a rule with no effect.
FOLD_BARRIERS = {
    ('California', 'Mammoth Lakes', 'Fresno'),
}

#: The `(hub, nearest)` pairs that actually blocked a fold this run — the state
#: is dropped because the pair is what `_cluster` matches on. Accumulates for the
#: life of the process; read by `generate_roster.py` once every state has
#: clustered, to refuse a [[FOLD_BARRIERS]] entry that no longer applies.
BARRIERS_APPLIED = set()

#: Towns whose Flicks `city` is not the name a local uses for the place, renamed
#: before the sub-pass clusters on them. Keyed by (metro label, city) so a
#: rename can never reach a same-named town in another metro, and applied to the
#: TOWN rather than to the finished label so that two spellings of one town
#: merge into one district instead of facing each other as two.
#:
#: Deliberately tiny — the dominant town's own name is right for all but four of
#: the 105 districts, and every entry here is a `city` that is wrong or
#: ambiguous for the venues filed under it, never a matter of taste:
#:
#: - Manhattan's venues are filed under "New York", which inside the metro ALSO
#:   called New York names nothing.
#: - Arlington, TX arrives as both "Arlington" and "Arlington Heights" (a Fort
#:   Worth neighbourhood 20 km away), splitting one town's four cinemas in two.
#: - "Kellerville" is not a place in Texas; the venue is Cinepolis Keller.
#: - The Aquarius and the Stanford are both in Palo Alto, not in the separate
#:   city of East Palo Alto they are filed under.
SUB_AREA_NAMES = {
    ('New York', 'New York'): 'Manhattan',
    ('Dallas Fort Worth', 'Arlington Heights'): 'Arlington',
    ('Dallas Fort Worth', 'Kellerville'): 'Keller',
    ('San Francisco', 'East Palo Alto'): 'Palo Alto',
}

#: Metros whose districts fold AGAIN, into the named regions a local browses by.
#: Keyed by (metro label, district label) so a region can never reach a
#: same-named district in another metro; applied to the finished district labels
#: rather than to the towns, so the clustering is unchanged and the fold is a
#: pure relabelling of its output.
#:
#: A metro belongs here when its districts are separate PLACES its own name does
#: not cover. Naming a district after its dominant town is right whenever the
#: metro is one city and the district is inside it — Chicago's suburbs are all
#: Chicagoland, and Dallas Fort Worth is already named for both its anchors, so
#: neither is listed. It is wrong for the three that sprawl across places with
#: names and identities of their own, where the raw list asserts a containment
#: no resident would say:
#:
#: - San Francisco is the whole Bay, not a city. "San Francisco 16 / San Jose 10
#:   / Oakland 9" reads as if San Jose were a neighbourhood of San Francisco.
#: - New York files 35 of its 102 venues under towns in Long Island (21),
#:   Westchester (11) and Rockland (3) — none of which anyone calls New York.
#:   Its boroughs also arrive at inconsistent depth: Brooklyn is one district
#:   while Queens is split into Forest Hills and Bayside.
#: - Los Angeles reaches 11 venues into Orange County and 8 into Ventura, and
#:   scatters the San Fernando Valley across four separate town names.
#:
#: The regions below are what a local says instead. Eighteen, eighteen and
#: twenty-eight town names are also more list than a picker can be browsed as,
#: so the fold buys legibility on top of accuracy.
#:
#: The fold is by district and not by radius because these regions are not
#: radial — the East Bay is a 70 km strip along the water and the Bay itself
#: sits in the middle of the metro, which is the same reason a compass split
#: fails here (see `models.UsMetroSubAreas`). Grouping the districts the
#: clustering already found keeps the geography and only renames the buckets.
#:
#: It also means a region is only ever as precise as the district under it. The
#: 6 km sub-pass pulls one Astoria and one Long Island City venue into the
#: Manhattan cluster (they sit across the river from Midtown), so both land in
#: Manhattan rather than Queens. Two venues, and splitting a district would mean
#: a second per-venue table; the district fold is the honest granularity.
#:
#: A metro listed here must be mapped EXHAUSTIVELY — `sub_areas_for_metro` dies
#: on a district with no region — so a re-harvest that clusters a new district
#: fails the generator instead of quietly showing "Berkeley" beside "East Bay".
SUB_AREA_REGIONS = {
    ('San Francisco', 'San Francisco'): 'San Francisco',
    ('San Francisco', 'Brentwood'):     'East Bay',
    ('San Francisco', 'Concord'):       'East Bay',
    ('San Francisco', 'Fremont'):       'East Bay',
    ('San Francisco', 'Hayward'):       'East Bay',
    ('San Francisco', 'Livermore'):     'East Bay',
    ('San Francisco', 'Moraga'):        'East Bay',
    ('San Francisco', 'Oakland'):       'East Bay',
    ('San Francisco', 'Richmond'):      'East Bay',
    ('San Francisco', 'San Ramon'):     'East Bay',
    ('San Francisco', 'Mountain View'): 'South Bay',
    ('San Francisco', 'San Jose'):      'South Bay',
    ('San Francisco', 'Santa Clara'):   'South Bay',
    ('San Francisco', 'Palo Alto'):     'Peninsula',
    ('San Francisco', 'San Mateo'):     'Peninsula',
    ('San Francisco', 'Larkspur'):      'North Bay',
    ('San Francisco', 'Novato'):        'North Bay',
    ('San Francisco', 'Vallejo'):       'North Bay',

    ('New York', 'Manhattan'):     'Manhattan',
    ('New York', 'Brooklyn'):      'Brooklyn',
    ('New York', 'The Bronx'):     'The Bronx',
    ('New York', 'Staten Island'): 'Staten Island',
    ('New York', 'Bayside'):       'Queens',
    ('New York', 'Forest Hills'):  'Queens',
    ('New York', 'Bellmore'):      'Long Island',
    ('New York', 'Farmingdale'):   'Long Island',
    ('New York', 'Huntington'):    'Long Island',
    ('New York', 'Lynbrook'):      'Long Island',
    ('New York', 'Manhasset'):     'Long Island',
    ('New York', 'Northport'):     'Long Island',
    ('New York', 'Stony Brook'):   'Long Island',
    ('New York', 'Westbury'):      'Long Island',
    ('New York', 'New Rochelle'):  'Westchester',
    ('New York', 'White Plains'):  'Westchester',
    ('New York', 'Yonkers'):       'Westchester',
    # Rockland, not "Hudson Valley": that is a metro of its own in this state.
    ('New York', 'Nanuet'):        'Rockland',

    ('Los Angeles', 'Los Angeles'):     'Los Angeles',
    ('Los Angeles', 'Marina del Rey'):  'Westside',
    ('Los Angeles', 'Santa Monica'):    'Westside',
    ('Los Angeles', 'Burbank'):         'San Fernando Valley',
    ('Los Angeles', 'Calabasas'):       'San Fernando Valley',
    ('Los Angeles', 'Encino'):          'San Fernando Valley',
    ('Los Angeles', 'North Hollywood'): 'San Fernando Valley',
    ('Los Angeles', 'Northridge'):      'San Fernando Valley',
    ('Los Angeles', 'Arcadia'):         'San Gabriel Valley',
    ('Los Angeles', 'Covina'):          'San Gabriel Valley',
    ('Los Angeles', 'Glendale'):        'San Gabriel Valley',
    ('Los Angeles', 'Pasadena'):        'San Gabriel Valley',
    ('Los Angeles', 'El Segundo'):      'South Bay',
    ('Los Angeles', 'Inglewood'):       'South Bay',
    ('Los Angeles', 'Torrance'):        'South Bay',
    ('Los Angeles', 'Cerritos'):        'Gateway Cities',
    ('Los Angeles', 'Downey'):          'Gateway Cities',
    ('Los Angeles', 'Montebello'):      'Gateway Cities',
    # Long Beach is the metro's second-biggest city, not a Gateway suburb.
    ('Los Angeles', 'Long Beach'):      'Long Beach',
    ('Los Angeles', 'Buena Park'):      'Orange County',
    ('Los Angeles', 'Garden Grove'):    'Orange County',
    ('Los Angeles', 'La Habra'):        'Orange County',
    ('Los Angeles', 'Camarillo'):       'Ventura County',
    ('Los Angeles', 'Simi Valley'):     'Ventura County',
    ('Los Angeles', 'Thousand Oaks'):   'Ventura County',
    ('Los Angeles', 'Lancaster'):       'Antelope Valley',
    ('Los Angeles', 'Santa Clarita'):   'Santa Clarita',
    # 35 km of sea from anywhere else; it kept its own district and keeps its
    # own region for the same reason.
    ('Los Angeles', 'Avalon'):          'Catalina Island',
}


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


@lru_cache(maxsize=1)
def _timezone_finder():
    """The lat/lon -> IANA zone lookup, imported lazily with an actionable error.

    A generator-only dependency: the roster is regenerated by hand after a
    re-harvest, never at build or test time, so this never has to resolve on a
    developer's machine that is not regenerating it. See `data/us/README.md`.
    """
    try:
        from timezonefinder import TimezoneFinder
    except ImportError:
        raise SystemExit("timezonefinder is not installed — `python3 -m pip install timezonefinder` "
                         "(needed only to regenerate the roster; see data/us/README.md)")
    return TimezoneFinder()


@lru_cache(maxsize=None)
def _offsets(zone):
    """A zone's UTC offsets on [[TZ_PROBE_DATES]] — what a CLOCK does, which is
    the only part of a zone name that reaches a reader.

    `America/Indiana/Indianapolis` and `America/New_York` are different names for
    the same clock, so a majority taken over names would split a metro that has
    no disagreement in it. Taken over offsets, they are one.
    """
    tz = ZoneInfo(zone)
    return tuple(tz.utcoffset(d).total_seconds() for d in TZ_PROBE_DATES)


def zone_for(venues):
    """The IANA zone a group of venues keeps its clock in.

    A `City`'s zone decides when its day rolls over, when a showtime is treated
    as started, and the UTC offset its schema.org `ScreeningEvent`s carry — all
    of them per-city, so a group of venues gets ONE zone however it is spread.
    The zone is therefore the majority: the clock most of the group is really on,
    rather than the state's nominal zone, which put Knoxville on Central and El
    Paso on Eastern.

    Majority by OFFSET first (see [[_offsets]]), then by name within the winning
    offset, so a metro straddling `America/Indiana/Indianapolis` and
    `America/New_York` — the same clock — is decided by weight rather than by
    which spelling happens to lead. Ties break alphabetically, so the answer is a
    pure function of the input.

    Eighteen metros do straddle a real boundary and the minority venues keep a
    clock that is not theirs; `generate_roster.py` reports how many. Splitting a
    metro at a zone line instead would cut travel-sheds people drive across
    daily, which is a worse answer than an hour's drift on the fold's edge.
    """
    finder = _timezone_finder()
    zones = [z for z in (finder.timezone_at(lat=v['lat'], lng=v['lon']) for v in venues) if z]
    if not zones:
        raise SystemExit(f"no timezone resolves for any of {len(venues)} venues at "
                         f"{[(v['lat'], v['lon']) for v in venues][:3]}")
    by_offset = Counter(_offsets(z) for z in zones)
    winner = min(by_offset, key=lambda o: (-by_offset[o], o))
    names = Counter(z for z in zones if _offsets(z) == winner)
    return min(names, key=lambda n: (-names[n], n))


def same_clock(a, b):
    """Whether two IANA zones show the same time all year — see [[_offsets]]."""
    return _offsets(a) == _offsets(b)


def check_coordinates(state, venues):
    """Refuse a state whose coordinates cannot be what they claim.

    Both checks are for BAD DATA, not for clustering: a venue's coordinates are
    the only thing that decides which metro — which `City`, which URL — it ends
    up in, and a wrong one is invisible downstream. It does not fail to cluster;
    it clusters somewhere plausible-looking and wrong, and it takes every cinema
    sharing its town name with it. All four errors this caught had been shipping
    silently, and each is contradicted by the record's OWN postcode.

    Loud rather than lenient: the fix belongs in `venues.json`, where it is one
    field, and a generator that quietly repaired it would hide the next one.
    """
    points = [(v['lat'], v['lon']) for v in venues]
    median = (sorted(p[0] for p in points)[len(points) // 2],
              sorted(p[1] for p in points)[len(points) // 2])
    for venue in venues:
        away = haversine_km((venue['lat'], venue['lon']), median)
        if away > MAX_VENUE_FROM_STATE_KM:
            raise SystemExit(
                f"{state}: {venue['slug']!r} is {away:,.0f} km from the state's other venues "
                f"(lat={venue['lat']}, lon={venue['lon']}) — check venues.json for a flipped "
                f"longitude sign or a record filed under the wrong state")
    for town, members in sorted(_by_town(venues).items()):
        span = max(haversine_km(a, b)
                   for a in ((v['lat'], v['lon']) for v in members)
                   for b in ((v['lat'], v['lon']) for v in members))
        if span > MAX_TOWN_SPAN_KM:
            raise SystemExit(
                f"{state}: the venues filed under town {town!r} are {span:,.0f} km apart "
                f"({', '.join(sorted(v['slug'] for v in members))}) — clustering collapses a "
                f"town to one point, so one of them is filed under the wrong town in venues.json")


def _by_town(venues):
    grouped = defaultdict(list)
    for venue in venues:
        grouped[venue['city']].append(venue)
    return grouped


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
    return {
        city: {
            'pt': (sum(v['lat'] for v in members) / len(members),
                   sum(v['lon'] for v in members) / len(members)),
            'venues': members,
        }
        for city, members in sorted(_by_town(venues).items())
    }


def _cluster(towns, radius_km=None, min_venues=MIN_CLUSTER_VENUES, fold_radius_km=None,
             barriers=frozenset()):
    """{town: hub town} — greedy hub assignment at `radius_km`, then the fold.

    Defaults to the metro pass's own constants; `sub_areas_for_metro` re-enters
    it at a smaller radius to split ONE metro into its districts. Same algorithm
    either way — a metro and a neighbourhood differ in scale, not in kind.

    Towns are ranked by their own cinema count, then by how many cinemas sit
    within a radius of them, then by name. The count picks out the real cities;
    the density tiebreak decides the long tail of one-cinema towns, where a name
    ordering would put whichever town sorts first alphabetically at the centre
    of its region. Both are pure functions of the input, so the ranking — and
    everything downstream of it — is deterministic.
    """
    radius_km = RADIUS_KM if radius_km is None else radius_km
    fold_radius_km = 2 * radius_km if fold_radius_km is None else fold_radius_km
    names = list(towns)
    distance = {(a, b): haversine_km(towns[a]['pt'], towns[b]['pt'])
                for a in names for b in names}
    size = {c: len(towns[c]['venues']) for c in names}
    density = {c: sum(size[o] for o in names if distance[(c, o)] <= radius_km) for c in names}
    ranked = sorted(names, key=lambda c: (-size[c], -density[c], c))

    hubs, assigned = [], {}
    for town in ranked:
        nearest = min(hubs, key=lambda h: (distance[(town, h)], h), default=None)
        if nearest is not None and distance[(town, nearest)] <= radius_km:
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
        starved = sorted((h for h in hubs if held[h] < min_venues),
                         key=lambda h: (held[h], h))
        merged = False
        for hub in starved:
            others = [o for o in hubs if o != hub]
            if not others:
                continue
            nearest = min(others, key=lambda o: (distance[(hub, o)], o))
            if distance[(hub, nearest)] > fold_radius_km:
                continue
            if (hub, nearest) in barriers:
                # A barrier leaves the cluster starved on purpose — exactly what
                # happens to a cluster with no neighbour in range at all, which
                # the fold already treats as the honest answer for it.
                BARRIERS_APPLIED.add((hub, nearest))
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
    assigned = _cluster(towns, barriers={(hub, into) for st, hub, into in FOLD_BARRIERS if st == state})
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


def sub_areas_for_metro(metro, venues):
    """{venue slug: sub-area label} for ONE metro's venues, or `{}` if it is
    under [[MIN_VENUES_TO_SUBDIVIDE]].

    The metro pass again, one level down: the same towns, the same greedy hubs,
    the same fold — at [[SUB_RADIUS_KM]] instead of 75 km. What changes is the
    NAME. A metro is named after the Flicks region most of it is filed under,
    because that reads the travel-shed better than any one town; a district
    inside a metro has no such label, and does not need one — it is named after
    the town it centres on, which for New York is literally the borough
    (Brooklyn, The Bronx, Staten Island) and for Los Angeles is Santa Monica,
    Pasadena, Burbank, Long Beach. [[SUB_AREA_NAMES]] fixes the four towns whose
    `city` is not what a local says, before the clustering so that two spellings
    of one town cluster as one.

    Hub towns are distinct by construction, so labels are unique within the
    metro; this still checks their SLUGS, which is what a client persists.

    A metro in [[SUB_AREA_REGIONS]] then folds those districts once more onto
    the regions a local browses it by — the Bay Area's East Bay / South Bay /
    Peninsula / North Bay rather than eighteen town names.
    """
    if len(venues) < MIN_VENUES_TO_SUBDIVIDE:
        return {}
    towns = _towns([dict(v, city=SUB_AREA_NAMES.get((metro, v['city']), v['city']))
                    for v in venues])
    assigned = _cluster(towns, SUB_RADIUS_KM, MIN_SUB_CLUSTER_VENUES, SUB_FOLD_RADIUS_KM)
    assigned = _regions_for(metro, assigned)
    for slug, group in _by_slug(set(assigned.values())).items():
        if len(group) > 1:
            raise SystemExit(f"{metro}: sub-areas {sorted(group)} all slug to {slug!r}")
    return {v['slug']: assigned[town]
            for town, t in towns.items() for v in t['venues']}


def _regions_for(metro, assigned):
    """`assigned` with each district label replaced by its [[SUB_AREA_REGIONS]]
    region, or unchanged for a metro the table does not list.

    Exhaustive by construction: a listed metro that clusters a district the
    table has no region for kills the generator, because half a metro folded is
    worse than none — the picker would show four regions next to a stray town.
    """
    if not any(m == metro for m, _ in SUB_AREA_REGIONS):
        return assigned
    unmapped = sorted({d for d in assigned.values() if (metro, d) not in SUB_AREA_REGIONS})
    if unmapped:
        raise SystemExit(f"{metro}: districts {unmapped} have no SUB_AREA_REGIONS region "
                         f"— add them to cluster_metros.SUB_AREA_REGIONS")
    return {town: SUB_AREA_REGIONS[(metro, district)] for town, district in assigned.items()}


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
    sizes, per_state, big = [], {}, []
    for state in sorted(by_state):
        metros = metros_for_state(state, by_state[state], labels[state])
        counts = Counter(metros.values())
        per_state[state] = counts
        if len(by_state[state]) >= 30:   # UsRoster.MinCinemasToSplit
            sizes.extend(counts.values())
        for metro, held in counts.items():
            if held >= MIN_VENUES_TO_SUBDIVIDE:
                big.append((held, metro, [v for v in by_state[state]
                                          if metros[v['slug']] == metro]))
    print(f"areas in split states: {len(sizes)}  biggest {max(sizes)}  "
          f"one-venue {sum(1 for s in sizes if s == 1)}  "
          f"two-or-fewer {sum(1 for s in sizes if s <= 2)}  "
          f"median {sorted(sizes)[len(sizes) // 2]}")
    for state, counts in sorted(per_state.items(), key=lambda kv: -sum(kv[1].values()))[:8]:
        top = ', '.join(f"{n} {c}" for n, c in counts.most_common(4))
        print(f"  {state:14s} {sum(counts.values()):4d} venues -> {len(counts):3d} metros ({top})")

    print(f"\nsub-areas: radius={SUB_RADIUS_KM}km min={MIN_SUB_CLUSTER_VENUES} "
          f"fold={SUB_FOLD_RADIUS_KM}km, in metros of {MIN_VENUES_TO_SUBDIVIDE}+ venues")
    for held, metro, vs in sorted(big, reverse=True, key=lambda b: (b[0], b[1])):
        areas = Counter(sub_areas_for_metro(metro, vs).values())
        print(f"  {metro} ({held} venues -> {len(areas)} sub-areas)")
        print('    ' + ', '.join(f"{name} {n}" for name, n in
                                 sorted(areas.items(), key=lambda kv: (-kv[1], kv[0]))))


if __name__ == '__main__':
    _report()
