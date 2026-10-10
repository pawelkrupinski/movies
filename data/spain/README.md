# Spain (sensacine.com) — full cinema roster

Harvested + geocoded roster of **all Spanish cinemas** on sensacine.com (the
Webedia/AlloCiné platform's Spanish deployment), mirroring
[`data/germany`](../germany/README.md)'s roster shape and pipeline.

## Pages

Since 2026-10 a Spanish page is not a province but Poland's shape of page
(`data/pl/README.md`), built by the same rule — `data/scripts/town_pages.py`,
called by `scripts/build_pages.py` into **`pages.json`** and generated into
`common/.../models/SpanishRosterData.scala`:

1. **A major city** — one of the **43 municipalities of 150,000 people or
   more** (GeoNames municipal population; Badajoz, 149,746, is the first one
   out) — lists only the venues inside that municipality. `/barcelona/` is
   Barcelona's cinemas, not Berga's or Sitges'.
2. **Any other town with 3+ venues** is a page of its own (Cornellà de
   Llobregat, Arrecife…).
3. **Every other town is clustered:** a 2-venue town takes the 1- and 2-venue
   towns within 10 km; the 1-venue towns left cluster within 25 km of the
   biggest of them; a town still alone joins the nearest cluster within 35 km.
   A cluster is named after its biggest town — "Sitges y alrededores", "en
   Sitges y alrededores". Never across the sea: every town in the island
   provinces, Ceuta and Melilla is on an island in `build_pages.py`'s
   `ISLANDS`, and towns on different landmasses share no page (Ceuta is 28 km
   from Algeciras).
4. The picker groups every page by **province**, majors and clusters side by
   side; the apps get the province as each city's `region`.

240 pages (43 major, 111 towns, 86 clusters) for 602 venues: median 2 venues a
page, at most 11; 85 one-venue pages, mostly towns with nothing within 35 km.
Barcelona's province went from one page of 55 venues to 15, Madrid's 51 to 15.

A page that disappears is kept in `pages.json`'s `retired` map, pointing at the
page now holding MOST of its venues (ties: a major city's, then the one nearest
the old page's centre); `City.renamedSlugs` 301s it. The first build retired
the 13 province slugs that are not also a town's — `/asturias/` → Gijón,
`/islas-baleares/` → Palma de Mallorca, `/vizcaya/` → Barakaldo,
`/soria/` → Golmayo (Soria's cinema, a municipality over the line). The other
39 province slugs are a town's page now and answer as that town. Each retired
entry also carries the slug its page was qualified to where another country
had the plain one (`City.spanishSlug`), so the redirect leaves from the URL the
page really had.

### Towns: `town-coords.json`

A town is a **municipality**, and each venue's comes from its own address, not
from SensaCine: the section header SensaCine files a venue under is usually its
town and sometimes not — "Estacion De Espiel" (Córdoba) heads Huércal-Overa's
cinema (Almería), "Fraile" (Jaén) the Autocinema Tenerife, "Grau I Platja" is
Gandia's beach district.

- `scripts/fetch_venue_addresses.py` → **`venue-addresses.json`**: every
  SensaCine venue page's schema.org address (street, postal code, locality);
  595 pages, 5 workers, ~80 s.
- `scripts/build_venue_towns.py` → **`town-coords.json`**: each venue's
  municipality — the locality or header matched inside the province the postal
  code names, else the postal code's own municipality (GeoNames' postal dump),
  else a hand-checked pin in `PINNED` — and each municipality's centre (its most
  populous place), population and province. 602 venues in 420 towns. Shown the
  way the header writes it when that is the whole name ("Alcoy"), GeoNames'
  name otherwise ("El Masnou", not SensaCine's "Masnou").

Re-cluster (after a re-harvest, or to change the rule):

```
python3 data/spain/scripts/fetch_venue_addresses.py   # after a re-harvest only
D=$(mktemp -d)
curl -sL https://download.geonames.org/export/dump/ES.zip -o $D/ES.zip && unzip -oq $D/ES.zip -d $D
mkdir $D/zip && curl -sL https://download.geonames.org/export/zip/ES.zip -o $D/zip/ES.zip && unzip -oq $D/zip/ES.zip -d $D/zip
python3 data/spain/scripts/build_venue_towns.py $D/ES.txt $D/zip/ES.txt   # -> town-coords.json
python3 data/spain/scripts/test_build_venue_towns.py
python3 data/spain/scripts/build_pages.py                                 # -> pages.json
python3 data/spain/scripts/test_build_pages.py
python3 data/spain/scripts/generate_roster.py                             # -> SpanishRosterData.scala
rm -rf $D
```

A new page has no share card until it has deployed; list it in
`OgCardAssetsSpec`'s `awaitingFirstDeploy`, and delete a retired page's card.

## Contents

- **`pages.json`**, **`town-coords.json`**, **`venue-addresses.json`** — see
  "Pages" above.
- **`provinces.json`** — the harvested roster: **52 provinces**
  (Spain's 50 provinces + the autonomous cities Ceuta and Melilla, which
  sensacine.com's own `/cines/` index treats as provinces) covering **595
  cinemas**. Each province:
  `{ slug, name, lat, lon, zoneId, towns:[…], cinemas:[{theaterId, name, town, displayName}] }`.
  - `lat`/`lon` are the province's **capital city**'s coordinates (not a
    province centroid).
  - `zoneId` is `"Atlantic/Canary"` for the two Canary Islands provinces
    (Las Palmas, Santa Cruz de Tenerife) and `"Europe/Madrid"` for the other
    50 — the Canaries run an hour behind the mainland.
  - Every cinema `displayName` is globally unique across Spain (the wire key
    every stored showtime is filed under). The actual 2026-09-01 harvest has
    **zero** raw name collisions, so all 595 `displayName`s equal `name`
    unchanged — see "Deduplication" below for how a collision would be
    handled and how that logic is tested.
- `theaters-raw.json` — the raw flat harvest (595 theaters), one object per
  venue: `{theaterId, name, town, provinceId, provinceName}`.
- `province-coords.json` — the 52 provinces → capital city name + lat/lon +
  zoneId (GeoNames), the direct input to `provinces.json`'s geo fields. No
  page is placed by these any more (a page is placed at its town); the first
  page build used them only to break a tie over where a retired province
  redirects. Salamanca's is wrong — GeoNames' biggest "Salamanca" is the Madrid
  district — and harmlessly so, since `/salamanca/` is a town page now.
- **`communities.json`** — province → autonomous community. Reference data
  (the Spanish state's own administrative division), NOT harvested, which is
  why it sits apart from `provinces.json` and survives a re-harvest. It has
  exactly one job: qualifying a page slug that another country already
  claims in `City.bySlug`'s single global namespace, the way a state qualifies
  a US metro's. Two pages need it today — Toledo and Laredo, which the US
  roster already serves (Ohio, Texas), so Spain's are
  `/toledo-castilla-la-mancha/` and `/laredo-cantabria/`.
- `scripts/` — the reproducible pipeline.

## How it was produced

1. **Crawl** (`scripts/crawl_sensacine.py`) — sensacine.com's `/cines/`
   directory: province index → all 52 `/cines/provincias-<id>/` pages,
   paginated (`?page=N`) until a page adds no new theater id → every venue's
   `theaterId` + name, recovered from the `data-theater="{&quot;id&quot;:...}"`
   JSON attribute on each venue card (a plain `href="/cines/cine/E\d+/"`
   regex undercounts badly — most cards only carry the id in this attribute).
   Venues are attributed to the town named by the nearest preceding `<h2
   class="titlebar-title...">` section header. No proxy needed — plain
   `curl`/`urllib` with a realistic desktop Chrome User-Agent hit no
   403/429 against this host; the crawl paces itself at ~400ms/request,
   sequential, retrying once on any fetch failure.
   ```
   python3 data/spain/scripts/crawl_sensacine.py
   ```
2. **Geocode** (`scripts/geocode_provinces.py`) — matches each province's
   capital city (an explicit, hand-verified `PROVINCE_CAPITAL` map in the
   script — the capital isn't always the province's namesake, e.g. Álava's
   capital is Vitoria-Gasteiz, Vizcaya's is Bilbao) against the free
   GeoNames bulk dump (`ES.txt`, tab-separated; feature class `P`, highest
   population match wins). All 52/52 resolved on the first pass — no manual
   fixes were needed.
   ```
   mkdir -p data/spain/geonames
   curl -sL https://download.geonames.org/export/dump/ES.zip -o data/spain/geonames/ES.zip
   unzip -o data/spain/geonames/ES.zip -d data/spain/geonames
   python3 data/spain/scripts/geocode_provinces.py
   rm -rf data/spain/geonames   # ~11MB uncompressed dump, not checked in
   ```
3. **Accents** (`scripts/build_town_names.py`) — SensaCine writes its town
   headers unaccented and title-cased ("Alcala De Henares"), and only 28 of
   the 423 towns keep their accents. Those names now go on the page — the
   `<h1>`, the meta description, the schema.org `containsPlace` — so they are
   corrected against the same GeoNames dump, into `town-names.json`. The
   correction can only ever re-spell a town, never substitute one: a GeoNames
   name is accepted only when it folds to the same ASCII as the harvested one.
   The 60 towns GeoNames does not know under that name are left as harvested,
   with only the particle-casing rule applied. Since the move to town pages
   these spellings are read by `build_venue_towns.py`, which shows a
   municipality the way its header writes it.
   ```
   mkdir -p data/spain/geonames
   curl -sL https://download.geonames.org/export/dump/ES.zip -o data/spain/geonames/ES.zip
   unzip -o data/spain/geonames/ES.zip -d data/spain/geonames
   python3 data/spain/scripts/build_town_names.py
   python3 data/spain/scripts/test_build_town_names.py
   rm -rf data/spain/geonames
   ```
4. **Build** (`scripts/build_provinces.py`) — joins the crawl + geocode
   output into `provinces.json`, assigning each province a slug (lowercase,
   ASCII-folded, spaces/punctuation → hyphens — e.g. `Álava` → `alava`,
   `A Coruña` → `a-coruna`) and computing each cinema's `displayName`.
   ```
   python3 data/spain/scripts/build_provinces.py
   ```
5. **Pages** — `build_venue_towns.py` and `build_pages.py`, see "Pages" above.
6. **Generate the Scala** (`scripts/generate_roster.py`) — turns
   `pages.json` + `provinces.json` + `ocine.json` into
   `common/src/main/scala/models/SpanishRosterData.scala`, the flat tuple data
   `models.SpanishRoster` materialises into `City`/`Cinema` objects. It refuses
   to emit a duplicate `displayName` across the whole country, or a venue on no
   page or two — both silent downstream, the first as two cinemas sharing one
   wire key, the second as a venue never scraped.
   ```
   python3 data/spain/scripts/generate_roster.py
   ```

### Deduplication

`build_display_names` in `build_provinces.py` starts every `displayName`
from the raw venue `name`; if a name repeats within Spain it qualifies with
the town (`"Cinesa Diagonal (Barcelona)"`); if name+town also repeats, it
qualifies with the province too. If a collision survives both passes, the
script refuses to emit — non-zero exit naming the offending displayName(s)
— rather than silently colliding two cinemas onto one wire key.

The real 595-venue harvest has zero duplicate raw names, so none of the
qualification branches fire on real data. `scripts/test_build_provinces.py`
exercises all three cases (pass-through, town-qualified, town+province
-qualified) plus the refusal path directly against synthetic data:
```
python3 data/spain/scripts/test_build_provinces.py
```

## Counts

- **52** provinces, **0** with zero venues.
- **595** unique cinemas (theaterIds are unique across the whole harvest), to
  which `ocine.json` adds the **9** Ocine venues SensaCine does not list, less
  two venues since closed (E0778, E1013): **602** in the roster.
- Verified-facts expectation of ~594 held: the crawl found 595, one more
  than the pre-reconnoitred estimate — consistent with normal roster churn
  between the manual recon and this run, not a pagination miss (every
  province's last page added 0 new ids before the crawler stopped it).
- Biggest provinces by cinema count: Barcelona (55), Madrid (51), Valencia
  (39), Alicante (35), Murcia (24).
- **0** `displayName`s needed qualifying (see "Deduplication" above).

## What consumes this

`scripts/generate_roster.py` → `common/src/main/scala/models/SpanishRosterData.scala`
→ `models.SpanishRoster`, which materialises the `SpanishPage` cities and
`SpanishCinema` venues once and hands them to `City.spanishCities`,
`Cinema.byCity` and `CinemaScraperCatalog.spanishBaseByCity` (one
`WebediaShowtimesClient` on `WebediaMarket.Spain` per venue, keyed by its
`theaterId` — except the Ocine venues, scraped by `OcineClient` off the
chain's own per-venue ticketing server instead; see below).

### Ocine: `ocine.json`

The one roster input that is neither harvested nor generated. SensaCine
carries no programme for most Ocine venues and does not list some of them at
all, so `ocine.json` — hand-kept like `communities.json`, and so untouched by
a re-harvest — names each venue's own ticketing server
(`tickets.ocine<slug>.es`) in two lists: `listed`, the venues SensaCine has,
by `theaterId`; and `unlisted`, the chain's venues SensaCine does not list,
which `generate_roster.py` ADDS to their province with no `theaterId`, so their
own server is their only source. The file's `_comment` says which venues are
on it, which are not, and why. `generate_roster.py` refuses a row that has
stopped lining up with the harvest (a dropped `theaterId`, an unknown
province, one server named twice), and every host it names needs a pace row
in `tools.HostPolicies` — `CinemaScraperCatalogSpec` fails on one without.

**A re-harvest is not free.** `displayName` is the wire key every stored
showtime is filed under, so a venue whose name changes upstream arrives as a
NEW venue and its history stays filed under the old name. Re-run the pipeline
when the roster has genuinely moved, diff `provinces.json` before regenerating,
and expect the `expected-schedules.txt` / read-model snapshots to shift with
it.

## Retired venues

- **`retired.json`** — venues this country has retired (closed, duplicate or
  feedless), keyed by SensaCine `theaterId`, each with its reason, date and evidence. The
  generator drops them, so a re-harvest cannot bring one back, and `CountrySpec`
  fails if one is rostered anyway. To retire a venue, add it here and regenerate;
  to bring one back, delete its entry. Entries also arrive by PR: the worker's daily
  closure sweep (`ClosureSweep`) starts `.github/workflows/retire-venues.yml` for a
  venue confirmed closed, which re-checks it live and opens the PR adding it here.
