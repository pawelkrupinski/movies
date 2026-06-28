# White-cinema investigations

Durable cross-run log for the recurring (~every 3 days) "white cinema"
investigation. A **white** uptime bar = the scrape *fetch succeeded* but the
*parser returned zero showtimes* (parsed-but-empty), as opposed to red/yellow
(the fetch itself threw — 5xx/timeout/TLS/403). Each run targets cinemas whose
**last 3 consecutive active scrape buckets are all white** and records, per
venue, the root cause + action: `fixed` (SHA) / `unfixable: <reason>` /
`intentionally-dormant` / `needs-human: <reason>` / `recovered`.

Read this before investigating so you don't re-diagnose a venue already settled,
and so you can re-check whether a previously-broken venue has recovered.

## How to find the white list (methodology)

`/uptime` (kinowo.fly.dev) is now **auth-gated** — OAuth login + `ADMIN_ALLOWLIST`
(checked in `web/.../controllers/AdminAction.scala`); an anonymous `curl` returns
`401 "Not logged in."`. So query prod Mongo directly instead:

1. A `flyctl proxy 27017:27017 --app kinowo-mongo` is usually already running;
   start one if not (see the `prod-mongo-access` memory).
2. Connect with `MONGODB_URI` from the root checkout `.env.local`, host swapped to
   `127.0.0.1:27017`.
3. Collection `uptimeBuckets`, docs `{service, bucket, successes, failures,
   zeroes, errors, ...}`. Replicate `UptimeController`'s predicate
   (`web/.../controllers/UptimeController.scala`): per service take the last
   `RecentScrapes = 3` non-empty buckets; status `zero` (white) =
   `zeroes>0 && failures==0 && successes==0`. A venue is 3-scrape-white when those
   3 are all `zero`. Skip `*|enrichment` services. Service name = the cinema's
   `displayName` (`common/.../models/Cinema.scala`); map it to its client in
   `worker/.../services/cinemas/CinemaScraperCatalog.scala`.

---

## 2026-06-28

8 cinemas were 3-scrape-white. **Seven are genuinely film-dormant (parsers
verified working live); one (Kino Zamek) is `needs-human`.** No code change
shipped this run — the two that *looked* like fixable bugs (Studio's URL,
Zamek's allow-list) turned out to be a seasonal break and a subtle-risky
allow-list mismatch respectively.

Recoveries since 2026-06-25: **Kino za Rogiem (Płock)** and **Kino PCA
(Polkowice)** are no longer 3-scrape-white (recovered / fell off the active
white set). Kino PDK is still dormant (below).

### Kino MOK Nowa Ruda (Nowa Ruda) — `intentionally-dormant`
- Client: `MsiClient` @ `https://bilety.nowaruda.pl`.
- Was green until ~17:00Z today, flipped white. The MSI month page (`/MSI/mvc/pl?
  date=2026-06|07`) returns the JS-shell (0 `movies-movie__single`) because there
  are no June/July screenings. The authoritative AJAX endpoint
  `/MSI/mvc/pl/Repertoire/GetShortEventsWithFilters` returns exactly **one**
  upcoming event: *"Piotr Bałtroczyk - Stand Up 2026"* on **2026-10-11** — a
  stand-up (non-film) AND out of the client's 2-month fetch window. Parser
  correct; venue film-dormant near-term.
- Action: none. Re-check next run.

### Kino Warszawa (Przeworsk) — `intentionally-dormant`
- Client: `MsiClient` @ `https://bilety-kino.przeworsk.um.gov.pl`.
- Was green until ~17:00Z today. `GetShortEventsWithFilters` returns
  `{"filtersForEvent":[],"repertoireEvents":[],"dates":[]}` — **zero** upcoming
  events of any kind. June screenings ended; nothing future loaded. Parser
  correct.
- Action: none. Re-check next run.

### DKF Politechnika (Wrocław) — `intentionally-dormant` (summer/academic break)
- Client: `FilmwebShowtimesClient` (Filmweb cinemaId **1645**).
- Filmweb `/api/v1/cinema/1645/seances?date=…` returns **0 seances** for every
  probed date (2026-06-28 … 2026-07-05). It's a university discussion film club
  (DKF) in late June — these pause over the summer. Filmweb genuinely empty; not
  a parser bug. If it's still white in September with films visibly on Filmweb,
  re-probe for a real break / own-site migration.
- Action: none. Re-check next run (expect recovery in the autumn term).

### Kino nad Wartą (Koło) — `intentionally-dormant`
- Client: `Bilety24OrganizerClient` @
  `https://www.bilety24.pl/kino/organizator/koninskie-centrum-kultury-1626`.
- Organizer page is live (200, 118 KB) but has **0 `Film:` anchors** — only
  `Spektakl:` (×8, theatre) and `Wydarzenie:` (×4). The parser keys on `Film:`
  so it correctly returns empty. Venue currently programming only theatre/events.
- Action: none. Re-check next run.

### Studio (Opole) — `intentionally-dormant` (confirmed summer break to 3 Sept)
- Client: `KinoStudioClient` @ `https://mdk.opole.pl/kino-studio.html`.
- The scraped URL now soft-404s (HTTP 200 but body is *"404 - Młodzieżowy Dom
  Kultury"*, no `div.ckeditor`). The homepage instead links to
  `https://mdk.opole.pl/kino-studio-przerwa.html` ("przerwa" = break), whose text
  reads: *"W czasie wakacji nasze kino jest nieczynne… Startujemy już 3 września"*
  — **closed for the summer, reopening 3 September**. So the parser's empty result
  is correct; this is a seasonal hiatus, not a scraper bug.
- Action: none this run. **Re-check after ~3 Sept**: confirm the repertoire URL is
  back (likely `kino-studio.html` again; if they keep a new slug, update
  `KinoStudioClient.RepertoireUrl` + re-record the fixture then).

### Żuławski Ośrodek Kultury (Nowy Dwór Gdański) — `intentionally-dormant`
- Client: `BiletynaClient` @ `https://biletyna.pl/Nowy-Dwor-Gdanski/Zulawski-Osrodek-Kultury`.
- Live page is 200 with ld+json but **0 `ScreeningEvent`** — only non-film items
  (a `ComedyEvent` among 10 `@type` entries). Parser correctly drops non-movies
  and returns empty. No films programmed.
- Action: none. Re-check next run.

### Kino PDK (Pyrzyce) — `intentionally-dormant` (still, since 2026-06-25)
- Client: `BiletynaClient` @ `https://biletyna.pl/Pyrzyce/Pyrzycki-Dom-Kultury`.
- Re-verified live: still **0 `ScreeningEvent`** (17 non-film `@type` entries).
  Unchanged from last run. Parser working; venue still film-dormant.
- Action: none. Re-check next run.

### Kino Zamek (Szczecin) — `needs-human`
- Client: `KinoZamekClient`. Strategy: intersect the MSI ticketing portal
  (`https://bilety.zamek.szczecin.pl/MSI/mvc/pl?date=…`) with a film **allow-list**
  scraped from the castle website's kino listing
  (`https://zamek.szczecin.pl/wydarzenia/kino/`, links `/wydarzenie/kino/<slug>/`),
  matching MSI titles to listing slugs by a derived-slug prefix.
- **Both sides return data** — this is NOT dormant. MSI has 66 (June) + 266 (July)
  `movies-movie__single` blocks; the listing yields 7 film slugs
  (`faraon`, `czytajac-lolite-w-teheranie`, `90-urodziny-pavarottiego`,
  `szczecinskie-swieto-klasyki-filmowej-w-kinie-zamek`, the `zamkowe-noce-filmowe`
  banners, …). **But the current MSI titles and the listing slugs don't overlap**,
  so `isFilm` filters everything out → empty → white. It flipped white only
  ~16:00Z today after being green earlier, so it partly self-recovers day to day.
- Two real problems hide here:
  1. Most current MSI titles are non-films ("LATO NA TARASACH 2026" summer-terrace
     concerts / yoga / orchestras) — correctly excluded.
  2. But genuine classic films ARE on MSI now (`BRZEZINA`, `PAN TADEUSZ`,
     `MOJA DROGA B.`, `NIEZWYKŁA PODRÓŻ KOZIOŁKA MATOŁKA`) that almost certainly
     belong to the festival listed under the single banner slug
     `szczecinskie-swieto-klasyki-filmowej-w-kinie-zamek`. The per-title→slug
     prefix match can't bridge an individual film title to a festival-banner slug,
     so these are **under-reported**.
- Why no fix shipped: a robust fix means either abandoning the website allow-list
  for a film/non-film classifier (the MSI feed is concert-heavy, so this risks
  letting concerts through), or special-casing festival banners — both are
  speculative and the repo gate forbids a fix I can't back with a confident
  fail-before/pass-after test of the *intended* behaviour. The "what should show"
  ground truth (do the festival classics count?) is a product call.
- Action: **needs-human** — decide whether Kino Zamek's allow-list should be
  hardened to catch festival-banner films (and whether to mix in
  `NonMovieEventClassifier`/`OnlyMovieEventsFilter` to drop the LATO NA TARASACH
  concerts directly). All evidence above is reproducible via the two live URLs.

---

## 2026-06-25

3 cinemas were 3-scrape-white. **All three are genuinely film-dormant — parsers
verified working; no fixable scraper bug.** No code change shipped this run.

### Kino PDK (Pyrzyce) — `intentionally-dormant`
- Client: `BiletynaClient` @ `https://biletyna.pl/Pyrzyce/Pyrzycki-Dom-Kultury`.
- Never green in the 24h record. Live ld+json page has **0 `ScreeningEvent`**;
  the only upcoming item is a November `ComedyEvent` (kabaret), which the parser
  correctly drops as non-movie. Parser working; venue has no films programmed.
- Action: none. Re-check next run — will go green when it lists films again.

### Kino za Rogiem (Płock) — `intentionally-dormant` (transient between repertoires)
- Client: `MsiClient` @ `https://bilety.pokis.pl` (fetches current + next month
  `MSI/mvc/pl?date=YYYY-MM`).
- Was **green 2026-06-24 13:00Z**, flipped white ~same day. Recorded as white
  (zero), not red — so the worker's own fetch succeeds and parses zero films
  (the site times out from a local/residential IP, but the worker reaches it).
  June Jim-Jarmusch cycle ended; July repertoire not yet loaded (July fixture
  already empty). Parser shape matches fixtures.
- Action: none. **Re-check next run — expect recovery when July films load.** If
  still white after ~2 more runs with films visibly on bilety.pokis.pl, suspect a
  real break and re-probe.

### Kino PCA (Polkowice) — `intentionally-dormant`
- Client: `Bilety24OrganizerClient` @
  `https://www.bilety24.pl/kino/organizator/centrum-kultury-w-polkowicach-1689`.
- Never green in the 24h record. Organizer page is live and functional but lists
  only theatre + stand-up (`Spektakl:` / `Wydarzenie:` anchors) and **zero
  `Film:` anchors**; the parser keys on `Film:` so it correctly returns empty.
  Had films on 2026-06-08; venue has since stopped programming films.
- Action: none. Re-check next run.
