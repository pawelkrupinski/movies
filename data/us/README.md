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
under; it is kept for provenance and for a future area split, but it is not what
the roster groups by (see below).

## Grouping: states, not metros

Flicks lists **577 US metros**. That is far past the ~200 a city picker stays
usable at (Germany ships 158, the UK 79), so the roster groups by **state or
territory instead — 55 regions**, which is also the unit a US visitor
recognises. Metro detail is not lost: every venue keeps its `region_slug`, and a
large state (California is ~430 venues) can later be split into
`CinemaAreaGroup`s the way London is, without touching this dataset.

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
