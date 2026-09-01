# Spain (sensacine.com) — full cinema roster

Harvested + geocoded roster of **all Spanish cinemas** on sensacine.com (the
Webedia/AlloCiné platform's Spanish deployment), mirroring
[`data/germany`](../germany/README.md)'s roster shape and pipeline.

## Contents

- **`provinces.json`** — the roster the app will load: **52 provinces**
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
  zoneId (GeoNames), the direct input to `provinces.json`'s geo fields.
- **`communities.json`** — province → autonomous community. Reference data
  (the Spanish state's own administrative division), NOT harvested, which is
  why it sits apart from `provinces.json` and survives a re-harvest. It has
  exactly one job: qualifying a province slug that another country already
  claims in `City.bySlug`'s single global namespace, the way a state qualifies
  a US metro's. One province needs it today — Toledo, which the US roster
  already serves as the Ohio metro at `/toledo/`, so Spain's becomes
  `/toledo-castilla-la-mancha/`.
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
3. **Build** (`scripts/build_provinces.py`) — joins the crawl + geocode
   output into `provinces.json`, assigning each province a slug (lowercase,
   ASCII-folded, spaces/punctuation → hyphens — e.g. `Álava` → `alava`,
   `A Coruña` → `a-coruna`) and computing each cinema's `displayName`.
   ```
   python3 data/spain/scripts/build_provinces.py
   ```
4. **Generate the Scala** (`scripts/generate_roster.py`) — turns
   `provinces.json` + `communities.json` into
   `common/src/main/scala/models/SpanishRosterData.scala`, the flat tuple data
   `models.SpanishRoster` materialises into `City`/`Cinema` objects. It refuses
   to emit a province with no community, or a duplicate `displayName` across
   the whole country — both are silent downstream, the first as an
   unqualifiable slug collision and the second as two cinemas sharing one wire
   key.
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
- **595** unique cinemas (theaterIds are unique across the whole harvest).
- Verified-facts expectation of ~594 held: the crawl found 595, one more
  than the pre-reconnoitred estimate — consistent with normal roster churn
  between the manual recon and this run, not a pagination miss (every
  province's last page added 0 new ids before the crawler stopped it).
- Biggest provinces by cinema count: Barcelona (55), Madrid (51), Valencia
  (39), Alicante (35), Murcia (24).
- **0** `displayName`s needed qualifying (see "Deduplication" above).

## What consumes this

`scripts/generate_roster.py` → `common/src/main/scala/models/SpanishRosterData.scala`
→ `models.SpanishRoster`, which materialises the `SpanishProvince` cities and
`SpanishCinema` venues once and hands them to `City.spanishCities`,
`Cinema.byCity` and `CinemaScraperCatalog.spanishBaseByCity` (one
`WebediaShowtimesClient` on `WebediaMarket.Spain` per venue, keyed by its
`theaterId`).

**A re-harvest is not free.** `displayName` is the wire key every stored
showtime is filed under, so a venue whose name changes upstream arrives as a
NEW venue and its history stays filed under the old name. Re-run the pipeline
when the roster has genuinely moved, diff `provinces.json` before regenerating,
and expect the `expected-schedules.txt` / read-model snapshots to shift with
it.
