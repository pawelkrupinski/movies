# Germany (Filmstarts) — full cinema roster

Harvested + geocoded roster of **all German cinemas** on filmstarts.de, and the
source the shipped German model is generated FROM. Germany has been live at
`showtimes.cc/de` for some time: this is no longer a staging area for a future
phase, it is the input to `GermanRosterData.scala` (see "Regenerating" below).

## Contents

- **`regions.json`** — the roster the app will load: **158 regions** covering
  **1,529 cinemas** across all 16 Bundesländer. Each region:
  `{ slug, name, lat, lon, bundesland, cities:[…], cinemas:[{theaterId, name, city, displayName}] }`.
  Regions cap the city dropdown (901 raw cities → 158 regions, each aggregating
  cinemas within ~35 km of a hub city). Every cinema `displayName` is globally
  unique (4 chain-name collisions disambiguated with `(City)`).
- `theaters-raw.json` — the raw flat harvest (1,534 theaters, pre-clustering).
- `city-coords.json` — the 900 distinct cities → lat/lon (GeoNames), for reference.
- `scripts/` — the reproducible pipeline.

## How it was produced

1. **Crawl** (`scripts/crawl_de_full.py`, reusing `crawl_filmstarts.py`) — the
   filmstarts.de `/kinoprogramm/` directory: all 16 Bundesländer → cities →
   theaters, extracting each `theaterId` (the id `WebediaShowtimesClient` scrapes
   via `theater-<id>`). filmstarts hard rate-limits (429) a direct bulk crawl, so
   this routes through the **Decodo residential proxy** (`KINOWO_PROXY_*`) — the
   same proxy the prod worker already uses for filmstarts. NRW needs special
   handling (its lander page 1 is a link-less shell; real content starts at
   `?page=2`, and its big cities — Köln/Düsseldorf/Dortmund — are only on the
   homepage-featured list, not the lander sub-pages).
2. **Geocode** (`scripts/geocode_cities.py`) — 900/900 cities matched via the bulk
   GeoNames `DE.txt` dump (no live Nominatim needed); Bundesland backfilled from
   GeoNames admin1.
3. **Cluster** (`scripts/cluster_regions.py`) — greedy hub-assignment at a 35 km
   radius → 158 regions (13 single-city, 145 merged), each named after its
   largest constituent city.

## Regenerating

`scripts/generate_roster.py` turns `regions.json` into
`common/src/main/scala/models/GermanRosterData.scala` — the tuples
`models.GermanRoster` materialises into `GermanRegion` cities and `GermanCinema`
venues. Re-run it after any re-harvest, and commit the generated file with the
data.

It carries each region's `bundesland` through, because that is what
`City.germanStates` groups the `/` picker by: 16 headings over 158 regions, since
158 in one A-to-Z is not a list anybody reads. So a re-harvest re-groups the
picker on its own rather than drifting from a hand-kept list.

Two regions are corrected on the way through (`MISFILED` in the generator):
Münster and Dorsten are filed by the crawl under Hessen and Berlin, and both are
in Nordrhein-Westfalen by their own coordinates — the Land comes off whichever
Filmstarts lander page a region's hub venues were harvested under, and those two
pages are wrong. The generator FAILS if a correction stops correcting anything,
or names a slug the roster no longer has, so a stale override cannot sit there
quietly.

## What consumes this

All of it, as of the German launch: the roster model (`GermanRegion` /
`GermanCinema`), the scrape catalog's filmstarts entries, `Country.Germany`
(`showtimes.cc/de`, switchable, in the deploy matrix), the apps via
`/api/catalog` — which carries each city's Bundesland as its `region`, so the
two-step pick works there too — and the page + read-model snapshots.
