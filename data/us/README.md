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

## Grouping: states, not metros

Flicks lists **577 US metros**. That is far past the ~200 a city picker stays
usable at (Germany ships 158, the UK 79), so the roster groups by **state or
territory instead — 55 regions**, which is also the unit a US visitor
recognises. Metro detail is not lost: each state with 30+ venues is split into
metro `CinemaAreaGroup`s the way London is split by compass — 46 of the 55
states, 448 metros in all, California's 486 venues into 21. The split rule lives
in `UsRoster.metroAreas`, which reads the metro each venue was clustered into.

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

**A cluster never crosses a state line**, because a state is the `City` whose
cinemas the areas partition. So a metro that really does span one is split at
the border: New York's areas hold only the New York side of the NY metro and New
Jersey's only the Jersey side, and the same goes for Kansas City and St. Louis.
Where the Flicks label for the out-of-state metro is the honest name for what is
left, it is kept — New Jersey's biggest area is "New York" (52 venues) and
Delaware's is "Philadelphia".

`python3 data/us/scripts/cluster_metros.py` prints the venue-count distribution
the current constants produce; it is the tool the radius was tuned with.

Per-state time zones live in `scripts/states.py`, not here — they are code, not
harvested data. A handful of states straddle two zones; the predominant one is
used, and the field only decides where a listing's "today" boundary falls, never
a showtime.

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

The generator refuses to emit a roster with an unresolved duplicate display
name: `displayName` is the wire key every per-cinema slot is stored under, so two
venues sharing one silently rebind the loser's showtimes to the winner. It
qualifies a repeated title with its town, then its state, and dies if that is
still not unique. Cross-country collisions (a US venue against a Polish, UK or
German one) are caught separately by `UsRoster.claimedElsewhere`, which the
generator cannot see.

It runs the clustering above as part of the same pass, and dies the same way on
a metro it cannot name uniquely within its state. Each venue's `lat`/`lon` rides
along into the roster (rounded to 5 decimals, ~1 m) as well as being clustered
on: `UsMetroSubAreas` splits the five metros too big to browse — Los Angeles,
New York, San Francisco, Chicago, Dallas Fort Worth — into London's five compass
areas from those coordinates, so the sub-division is regenerated with the roster
rather than hand-maintained. Everything it does is a pure
function of `venues.json` — same input, byte-identical output — so re-running it
on an unchanged file is a no-op and the diff after a re-harvest is only what the
harvest moved.
