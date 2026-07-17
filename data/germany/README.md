# Germany (Filmstarts) — full cinema roster

Harvested + geocoded roster of **all German cinemas** on filmstarts.de, ready to
drive the data-driven German model. This directory is the validated *data
foundation*; the model/loader/web/mobile wiring that consumes it is the next
phase (see "Remaining work" below).

## Contents

- **`regions.json`** — the roster the app will load: **158 regions** covering
  **1,533 cinemas** across all 16 Bundesländer. Each region:
  `{ slug, name, lat, lon, bundesland, cities:[…], cinemas:[{theaterId, name, city, displayName}] }`.
  Regions cap the city dropdown (901 raw cities → 158 regions, each aggregating
  cinemas within ~35 km of a hub city). Every cinema `displayName` is globally
  unique (4 chain-name collisions disambiguated with `(City)`).
- `theaters-raw.json` — the raw flat harvest (1,533 theaters, pre-clustering).
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

## Remaining work (the model/product wiring)

1. **Data-driven roster model** — generate the German cities+cinemas from
   `regions.json` into `case class` data subtypes of the sealed `City`/`Cinema`
   (a `GermanRegion` City + `GermanCinema` Cinema), fed into `Cinema.byCity` so
   `Cinema.all`/`Source.all`/`byDisplayName` pick them up, plus the
   `CinemaScraperCatalog` filmstarts entries (theaterId → cinema). Replaces the 3
   hand-authored German cities (Berlin/Munich/Wurzburg) — they're a subset of this.
2. **Worker capacity (operational)** — 1,533 German cinemas is a large scrape
   load. `kinowo-worker` currently handles `pl,de` on one 2x/1gb machine sized for
   pl + 10 DE cinemas. Enabling the full roster needs Germany on its **own worker**
   (like UK's `kinowo-worker-uk`) or a resize — otherwise it OOMs/throttles.
3. **Web** — `Country.Germany.webUrl` + `fly.de.toml` (`showtimes-de`), make DE
   `switchable`, deploy matrix leg.
4. **Mobile** — iOS + Android German support. If DE serves via the `/api/catalog`
   endpoint, the apps can consume the regions dynamically instead of hardcoding.
5. **Snapshots/tests** — page + read-model snapshots regenerate; add coverage.

`Source.all` gains 1,533 entries — verify `byDisplayName`/priority scale is fine.
