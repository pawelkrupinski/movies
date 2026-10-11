# US cinema roster (Flicks, `www.flicks.us`)

`venues.json` is the checked-in harvest the US roster is generated from:

    venues.json + towns.json → scripts/build_pages.py → pages.json
                             → scripts/generate_roster.py → common/src/main/scala/models/UsRosterData.scala

Nothing in `common/` reaches the network; `UsRosterData.scala` is a pure function
of these files.

## Shape

One object per venue:

```json
{"slug":"amc-classic-albuquerque-12","title":"AMC CLASSIC Albuquerque 12",
 "address":"3810 Las Estancias Way, Albuquerque, New Mexico 87121",
 "city":"Albuquerque","state":"New Mexico","postCode":"87121",
 "lat":"35.0214740","lon":"-106.7111172"}
```

`slug` is the Flicks `/cinema/<slug>/` id the scraper fetches — the one field
that must stay exact. The harvest's `region_slug` (the Flicks metro a venue was
found under) is dropped: it named the 75 km metros the pages were until 2026-10,
and nothing reads it now. A venue with no `state` is not rostered.

## Pages: towns and clusters, by the shared rule

A US page is built by the rule Poland, Spain and Germany share
(`data/scripts/town_pages.py`), because people do not drive far just to see a
film:

1. A **major city** — a place of 100,000 people or more — lists only the venues
   inside it (`/los-angeles/` is Los Angeles; Pasadena is `/pasadena-california/`).
2. Any other town with **3+ venues** is a page of its own.
3. A **2-venue town** anchors a cluster of the 1–2-venue towns within **20 km**.
4. The 1-venue towns left cluster within **30 km** of the most populous of them.
5. A 1-venue town still alone joins the nearest multi-town cluster whose anchor
   is within **40 km**.

Only 1–2-venue towns ever cluster. A cluster is named after its anchor — most
venues, then most people — and reads "Bend area" ("in the Bend area"). Wider
radii than Germany's 10/20/30: American small towns are further apart. The 2026-10
build: 2,003 pages (302 major, 1,105 single towns, 596 clusters), 865 with one
cinema; 95% of cinemas within 29 km of the town their page is named after, the
furthest 76 km, 15 pages reaching past 50 km. The metros it replaced: 468 pages,
315 of them reaching past 50 km.

**A cluster may cross a state line** — the per-state metros split one travel-shed
into two pages (`/philadelphia-new-jersey/`, `/kansas-city-kansas/`); Hudson,
Wisconsin is on the Woodbury, Minnesota page now. The picker files a page under
its **anchor town's state**; `/texas/` is no page (`/california/` is California,
Maryland — a town).

**Towns** (`towns.json`, written by `scripts/geocode_towns.py` from GeoNames'
`cities1000.txt`): a venue is in the town of its `city` within 30 km in its own
state (50 km for a city of 500,000+ — Houston sprawls), else the state's place
nearest it within 6 km, else a town of its own centred on its venues. New York's
five boroughs are New York City, whatever Flicks calls them (Brooklyn, Bayside),
and Los Angeles' neighbourhoods are Los Angeles. The geocoder refuses a venue
harvested at a namesake's coordinates (`Grand Theater Madison`, Minnesota, sat at
Madison, Wisconsin) and one too far from its own town's gazetteer centre to be
matched to it; `build_pages.py` refuses a venue 1,500 km from the rest of its
state (a flipped longitude sign) and a town more than 60 km from every venue it
holds. The fix for each is the venue's coordinates in `venues.json`.

**Slugs.** A page is slugged after its name; where two US pages share it, the
bigger is bare and the other qualified with its state (`springfield-missouri`) —
unless the bare slug already meant a town: a slug a page had, or one retired
since, keeps meaning the town it was named after while that town has a venue, so
`/springfield/` is still Springfield, Illinois. Where another country serves the
slug, `City.usCities` qualifies it (`/birmingham/` is the UK's, so Alabama's is
`/birmingham-alabama/`; `/potsdam/` is Brandenburg's). Spain yields only
`/toledo/` and `/laredo/`, which the US held first.

**Retired slugs** (`pages.json` → `retired`): a page that stops existing 301s to
the page holding the town it was named after (`/philadelphia-new-jersey/` →
Philadelphia, `/portland-maine/` → the Westbrook cluster that holds Portland), or,
when no town of that name has a venue, to the page holding most of its venues
(`/inland-empire/` → Chino). The first build retired 149 of the 475 slugs the US
ever published, the four the venue-coordinate fixes had retired and the two
split states among them; retirements are kept and re-pointed on every build.

```
python3 data/us/scripts/build_pages.py
python3 data/us/scripts/test_build_pages.py
python3 data/us/scripts/generate_roster.py      # needs timezonefinder
```

**Time zones** are resolved per PAGE from its venues' coordinates (the majority
clock, `generate_roster.zone_for`); a venue across a zone line from the rest of its
page carries its own (`UsRosterData.venueZones`, 4 of 5,029), which `VenueClock`
reads before its page's.

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
row in `HostPolicies`.

State parsing needs tolerance beyond "full name before the ZIP": some addresses
use two-letter abbreviations, some New England ZIPs are missing their leading
zero, `Delaware` is misspelled `Deleware` throughout, and the Virgin Islands
render as `US Virgin Islands`. Normalize all of those to the full state name.

## Regenerating the Scala roster

```
python3 data/us/scripts/geocode_towns.py cities1000.txt   # only after a re-harvest adds venues
python3 data/us/scripts/build_pages.py
python3 data/us/scripts/generate_roster.py
```

`generate_roster.py` needs `timezonefinder` (`python3 -m pip install
timezonefinder`), the only dependency outside the standard library and only ever
at regeneration time — the roster is checked in, so nothing at build or test time
imports it.

It refuses to emit a roster with an unresolved duplicate display name:
`displayName` is the wire key every per-cinema slot is stored under, so two venues
sharing one silently rebind the loser's showtimes to the winner. It qualifies a
repeated title with its town, then its state, and dies if that is still not
unique. Cross-country collisions (a US venue against a Polish, UK or German one)
are caught separately by `UsRoster.claimedElsewhere`, which the generator cannot
see. It also refuses a page listing a venue the roster lacks, or a venue on no
page.

A wrong coordinate never fails quietly into a wrong page: the town match is by
name within 30 km of the venue's own point, so `Regal Largo Mall` (Largo, 33771)
arriving as city "Key Largo" is matched to Largo, and `Sky Vu Drive In Monroe`
(53566) arriving as "Tomah" to Monroe. `test_geocode_towns.py`,
`test_build_pages.py` and `test_generate_roster.py` cover the matching, the page
rule, the redirects and the clocks.

Everything is a pure function of the committed files — same input,
byte-identical output — so re-running on an unchanged tree is a no-op and the diff
after a re-harvest is only what the harvest moved.

## Retired venues

- **`retired.json`** — venues this country has retired (closed, duplicate or
  feedless), keyed by flicks.us slug, each with its reason, date and evidence. The
  generator drops them, so a re-harvest cannot bring one back, and `CountrySpec`
  fails if one is rostered anyway. To retire a venue, add it here and regenerate;
  to bring one back, delete its entry. Entries also arrive by PR: the worker's daily
  closure sweep (`ClosureSweep`) starts `.github/workflows/retire-venues.yml` for a
  venue confirmed closed, which re-checks it live and opens the PR adding it here.
