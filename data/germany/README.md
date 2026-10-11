# Germany (Filmstarts) — full cinema roster

Harvested + geocoded roster of **all German cinemas** on filmstarts.de, and the
source the shipped German model is generated FROM. Germany has been live at
`showtimes.cc/de` for some time: this is the input to `GermanRosterData.scala`
(see "Adding venues and regenerating" below).

## Contents

- **`venues.json`** — every Filmstarts venue harvested, one per line:
  `{ theaterId, name, city, displayName }`. `city` is the town Filmstarts files
  it under; `displayName` is the wire key and globally unique (chain-name
  collisions disambiguated with `(City)`). The roster is this less `retired.json`.
- **`pages.json`** — which listing page each venue is on (`build_pages.py`, below):
  **436 pages** over 1,515 cinemas — 74 major cities, 42 towns of three or more
  venues, 203 clusters of small towns, and 117 one- or two-venue towns with no
  neighbour to cluster with. Each page:
  `{ slug, kind, name, anchor, bundesland, qualifiedSlug, lat, lon, towns, cinemas }`.
  `retired` maps every slug that was a page and is not any more to the page
  holding the town it was named after — or, when that town has no venue left, the
  page now holding most of its venues (the 19 regions of 2026-10 that left no
  town page under their own slug: `/vechta/` → Cloppenburg, whose cluster holds
  Vechta; `/sigmaringen/`, whose cinema closed, → Tuttlingen).
- `city-coords.json` — each Filmstarts city → GeoNames lat/lon, population and
  admin1 (`scripts/geocode_cities.py`). The gazetteer `build_pages.py` reads,
  corrected by its `TOWN_COORDS`.
- `theaters-raw.json` — the raw flat harvest (1,534 theaters).
- **`retired.json`** — venues this country has retired (closed, duplicate or
  feedless), keyed by Filmstarts `theaterId`, each with its reason, date and evidence. The
  build drops them, so a re-harvest cannot bring one back, and `CountrySpec`
  fails if one is rostered anyway. To retire a venue, add it here and regenerate;
  to bring one back, delete its entry. Entries also arrive by PR: the worker's daily
  closure sweep (`ClosureSweep`) starts `.github/workflows/retire-venues.yml` for a
  venue confirmed closed, which re-checks it live, rebuilds the pages and roster,
  and opens the PR adding it here. A page left with no venue redirects to the
  page nearest it.
- **`kinoprogramm.json`** — each venue's kinoprogramm.com page, `{ theaterId: path }`:
  its showtime FALLBACK behind Filmstarts (`KinoprogrammClient`, served once a
  venue's Filmstarts scrape has failed three separate runs). 1,132 of 1,515 venues
  (2026-09-27); `kinoprogramm-unmatched.json` lists the rest with their best
  candidate. Produced by `scripts/harvest_kinoprogramm.py` (precision first: a
  candidate must name the roster city in its address AND sit within 25 km of it,
  so a same-named town elsewhere cannot match; open-air screens never match a
  regular cinema). Rerun it after a re-harvest, then `generate_roster.py`, which
  carries the path into `GermanRosterData`. The fallback is keyed by venue, not by
  page, so a re-cluster leaves it alone.
- `scripts/` — the reproducible pipeline.

## The pages

A German page used to be a REGION — every venue within ~35 km of a hub city, so
`/koeln/` listed Düsseldorf and Bonn and `/berlin/` listed Potsdam. Since 2026-10
it is the rule Poland and Spain use (`data/scripts/town_pages.py`), with Germany's
parameters in `scripts/build_pages.py`:

1. A **major city** — a municipality of 100,000 people or more — lists only the
   venues inside it.
2. Any other town with **3+ venues** is a page of its own.
3. The rest — 1- and 2-venue towns only — **cluster**: a 2-venue town takes the
   1- and 2-venue towns within **10 km**; the 1-venue towns left cluster within
   **20 km** of the biggest of them; a town still alone joins the nearest cluster
   within **30 km**. Tighter than Poland's 10/25/35: at those radii Germany's dense
   villages chain into clusters 66 km across. At 10/20/30 no cinema is more than
   46 km from the town its page is named after (Werder, on Oranienburg's page),
   and 95% are within 22 km.

A cluster is named after its anchor — most venues, then most people — as
"Bayreuth und Umgebung" ("in Bayreuth und Umgebung"; the phrase is `GermanPage`'s).
Slugs spell umlauts out (`muenster`); a page whose slug another country already
serves or wants is qualified with its Bundesland (`City.germanSlug`). The picker
groups pages by the anchor's Bundesland, read off its coordinates.

Towns are Filmstarts' cities, with two corrections in `build_pages.py`:

- **`TOWN_ALIASES`** — second spellings of one town (`Frankfurt` / `Frankfurt am
  Main`, `Freiburg im Breisgau` / `Freiburg`) and districts Filmstarts names on
  their own (`Kalk` is Köln, `Altona` Hamburg, `Regionalverband Saarbrücken` the
  Residenz's Völklingen). An alias between two points more than 10 km apart is
  refused. Lohne/Löhne and Sulzbach/Sulzbach-Rosenberg are NOT aliases — they are
  four towns.
- **`TOWN_COORDS`** — towns `city-coords.json` matched to a GeoNames namesake,
  each re-placed from the venue's own Filmstarts address: Lohne (Oldenburg), not
  Löhne; Sulzbach (Taunus), not Sulzbach-Rosenberg; Landau an der Isar, not
  Landau in der Pfalz; Gronau (Leine), Grafenberg, Ebersbach, Hirschberg, Walheim,
  Buckow and Taufkirchen (Vils) likewise. Found by comparing each town's GeoNames
  Land with the Filmstarts lander page its venue was crawled under, and reading
  the postal code of every mismatch.

The build is a **drift guard** too: it refuses a town placed more than 60 km from
the page its venues were on before — the shape a gazetteer namesake takes — unless
the town is pinned in `TOWN_COORDS`.

## Adding venues and regenerating

1. Append the venues to `venues.json` (theaterId, name, Filmstarts city,
   displayName). A city not yet in `city-coords.json` stops the build: geocode it
   (`scripts/geocode_cities.py`, or add the GeoNames row by hand) — and check it is
   the right namesake.
2. Rebuild and regenerate:

   ```
   python3 data/germany/scripts/build_pages.py
   python3 data/germany/scripts/test_build_pages.py
   python3 data/germany/scripts/generate_roster.py
   ```

   then run `CatalogSeedSpec` twice (it rewrites the apps' seeds) and the
   `OgCardAssetsSpec`, which lists any new page as awaiting its first share card.

`generate_roster.py` emits `common/src/main/scala/models/GermanRosterData.scala` —
the tuples `models.GermanRoster` materialises into `GermanPage` cities and
`GermanCinema` venues. It refuses a duplicate `displayName` and a venue on no page
or two. `scripts/ci/generated-artefacts-drift.sh` re-runs both scripts in CI, so a
`pages.json` or roster that disagrees with its inputs fails the build.

## How the harvest was produced

1. **Crawl** (`scripts/crawl_de_full.py`, reusing `crawl_filmstarts.py`) — the
   filmstarts.de `/kinoprogramm/` directory: all 16 Bundesländer → cities →
   theaters, extracting each `theaterId` (the id `WebediaShowtimesClient` scrapes
   via `theater-<id>`). filmstarts hard rate-limits (429) a direct bulk crawl, so
   this routes through the **Decodo residential proxy** (`DECODO_PROXY_*`) — the
   same proxy the prod worker already uses for filmstarts. NRW needs special
   handling (its lander page 1 is a link-less shell; real content starts at
   `?page=2`, and its big cities — Köln/Düsseldorf/Dortmund — are only on the
   homepage-featured list, not the lander sub-pages).
2. **Geocode** (`scripts/geocode_cities.py`) — 900/900 cities matched via the bulk
   GeoNames `DE.txt` dump (no live Nominatim needed).
3. **Pages** (`scripts/build_pages.py`) — above. Until 2026-10 a greedy 35 km
   hub clustering made 158 regions instead; its venues are `venues.json` now.

## What consumes this

All of it: the roster model (`GermanPage` / `GermanCinema`), the scrape catalog's
filmstarts entries (keyed by page slug) and kinoprogramm.com fallbacks (keyed by
venue), `Country.Germany` (`showtimes.cc/de`), the apps via `/api/catalog` — which
carries each page's Bundesland as its `region`, so the two-step pick works there
too — and the page snapshots.
