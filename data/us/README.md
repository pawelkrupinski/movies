# US cinema roster (Flicks, `www.flicks.us`)

`venues.json` is the checked-in input the US roster is generated from. It is the
loader's source of truth: nothing in `common/` reaches the network, and
`UsRosterData.scala` is a pure function of this file.

## Shape

One object per venue:

```json
{"slug":"amc-classic-albuquerque-12","title":"AMC CLASSIC Albuquerque 12",
 "address":"3810 Las Estancias Way, Albuquerque, New Mexico 87121",
 "city":"Albuquerque","state":"New Mexico","postCode":"87121",
 "lat":"35.0214740","lon":"-106.7111172","region_slug":"albuquerque"}
```

`slug` is the Flicks `/cinema/<slug>/` id the scraper fetches — the one field
that must stay exact. `region_slug` is the Flicks METRO the venue was found
under; the roster neither groups nor sub-groups by it directly (see below) — it
survives only as the source of each clustered metro's display LABEL. It is
`null` for the 788 venues recovered from their own pages rather than from a
metro sweep (pass 2 below), which is one of the reasons the sub-grouping is
distance-clustered instead.

## Grouping: the metro is the place, the state is how you find it

The addressable place is the **metro** — `/los-angeles/`, a `City` of its own —
because "films in Los Angeles" is a screen somebody wants and "films in
California" is not. 448 of them, over the 46 states with 30+ venues; a state
under that is one place in its own right and keeps the slug `states.py` gave it
(Alaska, Hawaii, DC, Delaware, Rhode Island, Vermont, Guam, American Samoa and
the Virgin Islands — nine of the 55), so the US serves **457 cities**.

The **state** survives as a `CityGroup`: the landing lists metros under their
state's heading, which is how a visitor gets from "California" to "Los Angeles".
It is not a URL — `/california/` 404s. The cut lives in `UsRoster.places`, which
reads the metro each venue was clustered into and the metro centroids this
generator emits beside them.

Metro names are not unique across state lines (see the clustering note below),
so `City.usCities` qualifies a slug with its state where the bare one is taken —
by another US metro (`/new-york/` is the New York side, `/new-york-new-jersey/`
the Jersey one) or by another country (`/birmingham/` is the UK's, so Alabama's
is `/birmingham-alabama/`).

## Sub-grouping: distance-clustered metros

The metros a state's picker groups by are **clustered from the venues' own
coordinates** by `scripts/cluster_metros.py`, the US counterpart of
`data/germany/scripts/cluster_regions.py`. Grouping on the raw `region_slug`
instead produced 607 areas over the 46 split states, 159 of them holding two
cinemas or fewer, plus a per-state "Other areas" catch-all for the 788 venues
carrying no slug at all. The clustering:

1. Collapses each state's venues to **towns** (a town's cinemas can never end up
   in two metros), positioned at the centroid of their own venues.
2. Ranks towns by cinema count, then by how many cinemas sit within a radius of
   them, then by name, and greedily picks hubs: each town joins the nearest hub
   within **75 km**, or becomes a hub itself.
3. **Folds** any cluster left holding fewer than 3 venues into its nearest
   neighbouring cluster, if one is within 150 km (twice the radius). Only a
   venue with no metro nearer than that keeps an area of its own — 11 of them.
4. Names each cluster after the Flicks metro most of its venues came from
   (whose display label `scripts/metros.py` derives from its slug), the
   hub town's own metro breaking a tie, and the hub town's name where the
   cluster has no metro or its metro just repeats the state's name. Labels are
   unique within a state; the picker's persisted key is `Slugify.stable` of the
   label, re-derived in `UsRoster` rather than carried in the generated data.

**Why 75 km.** Tuned against the outcome, not in the abstract. It is the
smallest radius that still merges the travel-sheds a resident treats as one
place — Dallas + Fort Worth, the whole SF Bay — while keeping apart the ones
they don't: greater LA stays separate from the Inland Empire (~85 km out),
Philadelphia from the Lehigh Valley. At 90 km both of those over-merge (LA
swallows the Inland Empire into a 175-cinema area); at 60 km the Dallas
metroplex splits in two. The resulting spread over the split states is a median
of 6 venues per metro, biggest Los Angeles at 133, only 18 holding two or fewer.

**A cluster never crosses a state line**, because the clustering runs per state
and a state is what the harvest is keyed by. So a metro that really does span one
arrives as one city per state: the New York travel-shed is `/new-york/` (102
venues, the NY side) and `/new-york-new-jersey/` (52, the Jersey side), and the
same goes for Philadelphia, Kansas City and St. Louis. Where the Flicks label for
the out-of-state metro is the honest name for what is left, it is kept — New
Jersey's biggest cluster really is called "New York".

`python3 data/us/scripts/cluster_metros.py` prints the venue-count distribution
the current constants produce; it is the tool the radius was tuned with.

Time zones are resolved PER METRO, from the coordinates of the metro's own
venues (`cluster_metros.zone_for`), not per state. Fifteen states straddle a
boundary, and a state-level zone reached every city cut out of one: Knoxville and
Chattanooga were served on Central time, El Paso on Eastern — 138 venues in all.
That is not only a "today" boundary. `City.zoneId` also decides when a showtime
counts as started (so shows vanished an hour early in El Paso and lingered an
hour in Knoxville) and the UTC offset the schema.org `ScreeningEvent`s carry to
Google. The clock text itself was always right: showtimes are stored as
`LocalDateTime` and printed verbatim.

Eighteen metros still straddle a real boundary, and their minority venues — 30 of
5,031 — keep the majority's clock. Splitting a metro at a zone line instead would
cut travel-sheds people drive across daily, which is the worse answer; the
generator prints the count so it cannot drift unnoticed.

## Re-harvesting

Two passes, because neither source alone is complete:

1. **Region sweep** — for each of the 577 metro slugs (the distinct
   `/now-playing/<slug>/` paths in `https://www.flicks.us/sitemap-main.xml/`):

   ```
   GET https://www.flicks.us/cinemas/geo/?lat=&lng=
   is-ajax-call: yes
   Cookie: geo={"gps":null,"region":"<metro-slug>","choice":"region","ip_detect":false}
   ```

   returns `{"count":N,"items":[…]}` with every field above already clean. The
   `lat`/`lng` query parameters are IGNORED — the cookie is what selects the
   region, and without it the response is IP-geolocated.

2. **Sitemap gap fill** — the region sweep misses a few hundred venues that
   appear in `https://www.flicks.us/sitemap-cinemas.xml/` (5,017 slugs) but sit
   in no metro result. Fetch those `/cinema/<slug>/` pages individually and read
   the venue's own coordinates and address off the page.

**Pace both passes at ~2 req/s, 3 workers maximum.** This origin does not answer
429 — it THROTTLES BY STALLING CONNECTIONS, and its throughput plateaus at ~3-5
req/s no matter how much concurrency you point at it, so going wider makes the
harvest slower and degrades the host. Measured 2026-08-30; see the `flicks.us`
row in `RealHttpFetch.HostPolicies`.

State parsing needs tolerance beyond "full name before the ZIP": some addresses
use two-letter abbreviations, some New England ZIPs are missing their leading
zero, `Delaware` is misspelled `Deleware` throughout, and the Virgin Islands
render as `US Virgin Islands`. Normalize all of those to the full state name.

## Regenerating the Scala roster

```
python3 data/us/scripts/generate_roster.py data/us/venues.json \
        common/src/main/scala/models/UsRosterData.scala
```

It needs `timezonefinder` (`python3 -m pip install timezonefinder`), the only
dependency outside the standard library and only ever at regeneration time — the
roster is checked in, so nothing at build or test time imports it.

The generator refuses to emit a roster whose COORDINATES cannot be what they
claim, because a venue's coordinates are the only thing deciding which metro —
which city, which URL — it lands in, and a wrong one does not fail to cluster: it
clusters somewhere plausible and wrong. Two checks, both in
`cluster_metros.check_coordinates`, and each caught a record that had been
shipping, every one contradicted by its own postcode:

- a venue more than 1,500 km from its state's other venues — `Grand Theatre
  Perry` (Iowa 50220) had a flipped longitude sign and sat in Mongolia, so it
  became a one-venue metro instead of joining Des Moines 65 km away; `Newport
  Performing Arts` is in Newport, **Oregon** (97365) and was filed under Newport
  News, Virginia;
- a town whose venues are more than 75 km apart — clustering collapses a town to
  one point, so `Regal Largo Mall` (Largo, 33771) arriving as city "Key Largo"
  put itself *and* the real Key Largo cinema in the Naples metro, 380 km apart;
  `Sky Vu Drive In Monroe` (53566) arriving as "Tomah" did the same to the real
  Tomah cinema.

`python3 data/us/scripts/test_cluster_metros.py` covers both, and the zone
majority.

It also refuses to emit a roster with an unresolved duplicate display
name: `displayName` is the wire key every per-cinema slot is stored under, so two
venues sharing one silently rebind the loser's showtimes to the winner. It
qualifies a repeated title with its town, then its state, and dies if that is
still not unique. Cross-country collisions (a US venue against a Polish, UK or
German one) are caught separately by `UsRoster.claimedElsewhere`, which the
generator cannot see.

It runs the clustering above as part of the same pass, and dies the same way on
a metro it cannot name uniquely within its state. It then runs it a SECOND time
inside each metro of 75+ venues — Los Angeles, New York, San Francisco, Chicago
and Dallas Fort Worth — at a twelfth of the radius, and carries that district's
name on the venue too (`sub_areas_for_metro`, grouped by `UsMetroSubAreas`).
Those five are too big to browse as one list, and their districts are the places
a local names: Manhattan, Brooklyn, The Bronx and Staten Island fall out of the
venues' own `city` values, as do Santa Monica, Pasadena and Long Beach.

Three of the five then fold those districts onto REGIONS (`SUB_AREA_REGIONS`).
A town name is right only while the district really is inside the metro's
namesake city — true of Chicago, whose suburbs are all Chicagoland, and of
Dallas Fort Worth, already named for both its anchors, so neither folds. It is
false for the three that sprawl across places with identities of their own,
where the raw list asserts a containment no resident would say: San Jose is not
inside San Francisco, Long Island is not inside New York, Orange County is not
inside Los Angeles. Those fold onto the regions a local browses by:

    San Francisco  18 districts ->  5   San Francisco, East Bay, South Bay,
                                        North Bay, Peninsula
    New York       18 districts ->  8   Manhattan, Long Island, Brooklyn,
                                        Westchester, Queens, Staten Island,
                                        Rockland, The Bronx
    Los Angeles    28 districts -> 12   Los Angeles, San Fernando Valley, San
                                        Gabriel Valley, Gateway Cities, Orange
                                        County, South Bay, Ventura County,
                                        Westside, Long Beach, Antelope Valley,
                                        Santa Clarita, Catalina Island

The fold runs over the districts the clustering already found, so the geography
is unchanged and only the buckets are renamed — which also caps its precision at
the district: the 6 km sub-pass pulls two Queens venues across the river into
the Manhattan cluster, so both land in Manhattan. The table must cover a listed
metro exhaustively: a re-harvest that clusters a district it has no region for
kills the generator rather than showing a stray town beside "East Bay".

Everything it does is a pure
function of `venues.json` — same input, byte-identical output — so re-running it
on an unchanged file is a no-op and the diff after a re-harvest is only what the
harvest moved.
