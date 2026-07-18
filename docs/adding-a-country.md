# Adding a new country

A repeatable runbook for bringing a new country online — model → data → worker →
web → localization → mobile → store. Distilled from the UK (`uk`, Flicks) and
Germany (`de`, Filmstarts) rollouts. Each country is a **fully isolated pipeline**:
its own Mongo db, worker machine, web app, and locale — nothing is shared but the
Mongo cluster and the Docker image.

Use `<cc>` for the country code (`de`), `<lang>` for the BCP-47 language (`de`),
`<Db>` for the database (`kinowo_de`). Work in a git worktree per the repo's
standing rule; commit each phase.

## 1. Model (`common/`)

1. **`Country`** (`common/src/main/scala/models/Country.scala`) — add a
   `case object` to the `Country` enum + `all`: `code`, `displayName`,
   `language = Locale.forLanguageTag("<lang>-<REGION>")`, `mongoDb = "kinowo_<cc>"`,
   `filmwebEnabled` (Filmweb is Polish-only → false elsewhere), `brandName`
   ("Showtimes" outside PL), and `webUrl` — **start `None`** (not yet deployed),
   flip to `Some("https://showtimes-<cc>.fly.dev")` in phase 4. `cities` reads the
   city list from phase 2. `webUrl = Some(...)` is what makes it `switchable`
   (appears in the navbar country switcher) and self-serve its OG origin.
2. **City roster** (`City.scala`) — the sealed `City` model. Two shapes:
   - **Hand-authored case objects** (like PL/UK): `case object X extends City(slug,
     CityLabels(...), lat, lon, ZoneId.of(...))` with `cinemas = Cinema.<city>`.
     Keep an `all<CC>Cities` full roster + an `active<CC>Cities` filter so
     enabling/disabling a city is a one-line edit (see UK). Add the list to
     `City.all`, `City.allModelled`.
   - **Data-driven** (like DE at scale — hundreds of cinemas): generate the roster
     from a checked-in data file (see `data/germany/`), grouped into **regions**
     (≤~200, to cap the dropdown). This needs a `case class` data subtype of the
     sealed `City`/`Cinema` fed into `Cinema.byCity`.
3. **Cinemas** (`Cinema.scala`) — each venue is a `Cinema(displayName, pillName)`
   **and a `Source`** (merge/priority). `Cinema.all` = `byCity.flatMap(_._2)`, and
   `Source.all`/`priority`/`byDisplayName` derive from it — so just add the country
   to `byCity` and it flows through. **Display names must be globally unique** (the
   `Source` key) — disambiguate chain collisions with `(City)`.
4. **Scrape catalog** (`worker/.../services/cinemas/CinemaScraperCatalog.scala`) —
   wire each cinema to its source client, keyed into `baseByCity`. Reuse an existing
   client where the source repeats (UK→`FlicksClient`, DE→`WebediaShowtimesClient`
   via `filmstarts(theaterId, cinema)`); a genuinely new source is a new
   `CinemaScraper` fitting the existing contract (no reaper change — open/closed).

**Tests:** `CountrySpec`, `CatalogSpec`, and any city-count spec. `Source.all`
grows — sanity-check `byDisplayName` uniqueness.

## 2. Data harvest (only if the roster isn't hand-authored)

For a full-country sweep (DE), see `data/germany/README.md` + `scripts/`: crawl the
source directory for every venue + its scraper id, geocode the cities
(GeoNames bulk `DE.txt`, 100% match — no live Nominatim needed), cluster into
≤~200 regions. **Source sites rate-limit bulk crawls (429)** — route through the
Decodo residential proxy (`KINOWO_PROXY_*`, `isp.decodo.com:10001`), the same proxy
the prod worker uses. Persist the dataset into the repo (`data/<country>/`) as the
loader's input.

## 3. Worker (`kinowo-worker-<cc>`)

The worker is split **per country** — each machine watches only its own db's change
stream (a per-country split halves per-machine cost; same-db replicas don't — see
`fly.worker.toml`). A large roster on the shared `kinowo-worker` will OOM/throttle.

1. **`fly.worker.<cc>.toml`** — clone `fly.worker.de.toml`: `app =
   'kinowo-worker-<cc>'`, `KINOWO_COUNTRIES = '<cc>'`, its own `worker_heapdumps`
   volume, `primary_region = 'arn'` (Mongo colocation), shared-cpu-2x/1gb (re-size
   for a big roster). Drop `<cc>` from the sibling worker's `KINOWO_COUNTRIES`.
2. **Provision BEFORE the first deploy** (or it crash-loops — the fresh app boots
   with no `MONGODB_URI`, the required-Mongo probe hits the stale `127.0.0.1` URI,
   and `MongoConnection.init` refuses to start):
   - `flyctl apps create kinowo-worker-<cc> --org personal`
   - `flyctl volume create worker_heapdumps --size 1 --region arn --app kinowo-worker-<cc>`
   - Secrets: copy **`MONGODB_URI` from the running `kinowo-worker`** (the real
     `kinowo-mongo.internal` URI, NOT the local `.env.local` `127.0.0.1` one),
     piped so the value never prints; the env-agnostic ones (TMDB/ZYTE/SENTRY/
     TELEGRAM_*/`KINOWO_PROXY_*`/OMDB/TRAKT) from `.env.local`. `flyctl secrets
     import` reads `KEY=VALUE` from stdin.
3. Add a matrix leg to `.github/workflows/deploy.yml` (the no-op guard is keyed on
   `bin=worker` + `matrix.toml`, so each worker app tracks its own toml).

## 4. Web frontend (`showtimes-<cc>`)

1. **`fly.<cc>.toml`** — clone `fly.de.toml`: `app = 'showtimes-<cc>'`,
   `KINOWO_COUNTRY = '<cc>'`, `primary_region` near the users (DE→`fra`, UK→`lhr`),
   G1 web JVM tuning. Add a matrix leg to `deploy.yml`.
2. **Flip `Country.<Cc>.webUrl` → `Some("https://showtimes-<cc>.fly.dev")`** — now
   `switchable`. This one flag AUTO-adds the country to (a) the navbar country
   `<select>`, (b) the debug `?country=` switcher + the dev per-country `DebugStack`
   wiring, and (c) the `/api/catalog` mobile endpoint — all three iterate
   `Country.switchable`, so no separate edits. Then update the CountrySpec /
   CatalogSpec / PageSnapshot assertions that asserted the country was excluded, and
   **regenerate the page snapshots** (the switcher gains an option) + the **mobile
   catalog seeds** (`CatalogSeedSpec` rewrites `ios/…/catalog-seed.json` +
   `android/…/catalog-seed.json` — mobile then picks up the country + cities
   automatically).
3. Provision: `flyctl apps create showtimes-<cc>` + its 7 web secrets — `MONGODB_URI`
   (prod, same as the worker), OAuth (`GOOGLE_*`/`FACEBOOK_*`), `ADMIN_ALLOWLIST`
   from `.env.local`, and a **fresh** `APPLICATION_SECRET` (per-app; `secrets.token_urlsafe(48)`).
   **OAuth login needs the new `showtimes-<cc>.fly.dev` redirect registered in the
   Google/Facebook consoles** (manual, provider-side) — the repertoire works
   without it.

## 5. Localization

- **Web** (`web/src/main/resources/`): `messages.<lang>` mirroring `messages.en`'s
  keys; add `<lang>` to `play.i18n.langs` in `application.conf` (else the deployment
  silently falls back to Polish). Fix any hardcoded literals to `messages(...)`.
  Generate `og-home-<cc>.png` (the share card).
- **iOS** (`ios/`): add a `<lang>` localization to every key in
  `Localizable.xcstrings` + `InfoPlist.xcstrings`; add `<lang>` to `knownRegions`
  in `project.pbxproj`; add a `Country(code:"<cc>", languageCode:"<lang>")`
  fallback-seed entry (locale is country-forced). Migrate hardcoded SwiftUI
  literals into the catalog first if you want them localized.
- **Android** (`android/`): `res/values-<lang>/strings.xml` mirroring the base
  keys; a `Country(code="<cc>", languageTag="<lang>")` seed entry.
- **Google Play** (`android/app/src/main/play/listings/<locale>/`): `title.txt`
  (≤30), `short-description.txt` (≤80), `full-description.txt` (≤4000). Publish with
  `./gradlew :app:bootstrapReleaseListing` then `:app:publishReleaseListing` (see
  `android/PLAY_PUBLISHING.md`).

## 6. Observability

`fly/grafana/victoria/scrape.yml` — add a `kinowo-worker-<cc>` target (its
`kinowo_worker_*` series carry `country="<cc>"`) and a `showtimes-<cc>-web` target.
That is the ONLY per-country Grafana step: the dashboards' worker panels map the
selected `$country` to its worker app via a hidden `$worker_app` variable
(`label_values(kinowo_worker_throttled{country=~"$country"}, app)`, defaulted to
**All** so it always follows `$country` — a stale single selection would pin the
wrong worker), so once the new worker is scraped, selecting `<cc>` shows its worker
with no dashboard edit.

## 7. Ship

Provision everything (phases 3–4) **before** merging, so the deploy legs have live
targets. Merge to `main` → CI builds once and deploys every leg. Verify each new
machine is `started` + `/health` passing, the worker connected to `kinowo_<cc>`
(hydrate logs), and the web app serves the repertoire. Watch the worker's
credit/steal for the first warm window.
