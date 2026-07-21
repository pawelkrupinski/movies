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

## 2026-07-21

**18 cinemas were 3-scrape-white** (real buckets ~08:15–11:15 UTC / 10:15–13:15
Warsaw, newest bucket 11:15 UTC across services — actively scraping, all three
white buckets per venue within ~1h of the newest, not a boot artifact). **ONE
real bug fixed (Centrum 3D Przemyśl — Filmweb went silently empty); the other 17
are film-dormant or a standing needs-human.** Discovery method unchanged
(`/uptime` auth-gated): a mongosh replay of `UptimeController`'s predicate over
prod `uptimeBuckets` via the running `flyctl proxy` on `127.0.0.1:27017` — per
service, last 3 non-empty buckets all `status==zero`
(`successes==0 && failures==0 && zeroes>0`), excluding `|enrichment` / the 6
enrichment sources / `img:*`.

**Set changes vs 2026-07-18 (was 16):**
- **NEW this run:** **Centrum 3D Przemyśl** — never diagnosed before; a
  Filmweb-backed venue whose Filmweb source went empty. **`fixed` @0f74f76d0.**
- **RETURNED:** **Kino Sfinks** — was `needs-human` on 2026-07-11 (film-dormant +
  markup drift), fell off the white set for 07-14/07-18, now 3-white again.
  Re-probed live this run — **unchanged, still `needs-human`.**
- **Carried over (16, all still white):** the same 16 as 2026-07-18. All 16
  live-probed this run (batched pass) — **every one still genuinely film-dormant
  or on a known break; no parser recovered-but-broken.**

**Out-of-scope heads-up (RED, not white — fetch failure, different mode):**
**Wybrzeże** was 3-scrape-**failing** (red): `CircuitOpenException: circuit open
for bilety.rck.kolobrzeg.pl`. The breaker tripped after repeated fetch failures,
so it correctly surfaces red (not white); the system is handling it. Not a white
target; flagged for the next run / a human. (Last run's Kinomax red has cleared.)

### Centrum 3D Przemyśl — `fixed` @0f74f76d0
- Old client: `FilmwebShowtimesClient(1786)`. Live Filmweb `/api/v1/cinema/1786/
  seances?date=…` returns `[]` for every date 2026-07-21…07-28, and
  `/cinema/1786/info` confirms the venue (Centrum 3D, Przemyśl, Konarskiego 9) —
  it's just no longer maintained on Filmweb. White for all 24 buckets, never
  green in the window.
- But the venue's OWN site is live and programming films: `ck.przemysl.pl`
  (Centrum Kulturalne w Przemyślu) → `/kino-centrum/repertuar` renders an
  **IcAgenda** (Joomla) event list, **50 upcoming showtimes across 8 real films**
  (Minionki i straszydła ×14, Zaproszenie ×12, Robin Hood: Koniec Legendy ×6, Toy
  Story 5 ×6, O czym sobie nie mówimy ×6, Takie jest życie ×3, Drzewo Magii ×2,
  Niesamowita historia Mumbo Jumbo ×1), dated 2026-07-28 → 08-16. Classic
  "Filmweb went silently empty for a small venue" root cause.
- Fix: new `KinoCentrum3DPrzemyslClient` reads that listing directly. Each
  `div.ic-event-div` is one screening; `div.iceventlist-title a`'s href
  (`/component/icagenda/<id>-<slug>/YYYY-MM-DD-HH-MM`) carries the title + the
  screening's own date+time, so one anchor = one showtime, folded to films via
  `SlotsToMovies.fold`. Titles left verbatim (ALL-CAPS + glued "dubbing") — the
  ingest choke point (`MovieCache.recordCinemaScrape`) recases + strips format
  tokens centrally. Catalog swapped from Filmweb 1786 → own-site client; the dead
  1786 corpus fixtures removed. Fail-before/pass-after
  `KinoCentrum3DPrzemyslClientSpec` (recorded 2026-07-21 fixture, 8 films / 50
  showtimes, first slot ROBIN HOOD 2026-07-28 15:00). Corpus fixture recorded and
  read-model-snapshot.json + expected-schedules.txt + all four expected-*.html
  regenerated (Centrum 3D joins the source set of the shared films Minionki /
  Zaproszenie / Toy Story 5 / Takie jest życie, reordering their poster-fallback
  tail; primary posters unchanged). All layers green: the client spec, worker
  unit (2457), e2e (both snapshots stable), PageSnapshot (all 5).

### Kino Sfinks (Kraków, Nowa Huta) — `needs-human` (unchanged since 2026-07-11)
- Client: `KinoSfinksClient` @ `kinosfinks.okn.edu.pl/wydarzenia-harmonogram.html`.
  Live this run: **0** `table.widok_listy`, **0** `tr[onclick]`, page renders
  `empty-results` / "Brak wydarzeń" — still film-dormant AND still on the drifted
  markup (the old harmonogram table is gone site-wide, exactly as 2026-07-11).
  With zero screening rows rendered anywhere there's no film-row markup to sample,
  so a new parser still can't be written or test-backed blind. **needs-human —
  re-check once the venue repopulates its calendar**, then rebuild the parser
  against the new (populated) row shape (and treat `.empty-results` as zero
  screenings, not a parse failure).

### Kino Zamek (Szczecin) — `needs-human` (unchanged festival filter-gap)
- Listing `zamek.szczecin.pl/wydarzenia/kino/` still yields only 2 festival/banner
  slugs (`44-45-pomorskie-spotkania-z-diaporama`, `zamkowe-noce-filmowe-2026`), no
  individual film-title slugs, so the per-title→slug prefix match filters every
  MSI film out. Same standing product call as 2026-06-28/07-07 — likely
  self-resolves when normal repertoire resumes and individual film slugs return.

**Carried-over dormant (15, live-probed this run — all still `intentionally-dormant`):**
ADA Kino Studyjne (ld+json `events:[]`), DKF Politechnika (Filmweb 1645 `[]`
2026-07-21/25 — summer/academic break), Kino CK Lublin (0 `Film:`, Jazz-festival
programme), Kino Chatka Żaka ("Brak wydarzeń"), Kino Krapkowice (0
`latest-kino-item`, summer break → 31 Jul, still in window), Kino Kuźnica
(header-only `tbl_repertoire`), Kino PDK (0 `ScreeningEvent`, only Comedy/Theater),
Kino Warszawa Przeworsk (0 `movies-movie__single` 07/08), Kino Wisła Brzeszcze (0
`Film:`), Kino nad Wartą (0 `Film:`), Kino Świt ("Brak nadchodzących seansów
filmowych"), Kozienicki Dom Kultury (MSI `repertoireEvents:[]` 07/08), Patria
("Brak filmu" every slot), Studio Opole (`kino-studio.html` 404s,
`kino-studio-przerwa.html` live → break to 3 Sept), Teatr Ziemi Rybnickiej
(`?type[]=film` returns only Koncert/Festiwal/Warsztaty, 0 film tiles). Each parser
correct; each venue genuinely un-programmed or within its known break window.

---

## 2026-07-18

**16 cinemas were 3-scrape-white** (real buckets ~02:15–03:15 UTC / 04:15–05:15
Warsaw, newest bucket 03:15 UTC across services — actively scraping, all three
white buckets per venue within ~1h of the newest, not a boot artifact). **All 16
are genuinely film-dormant or the standing needs-human festival gap — every one
live-probed this run and confirmed the parser is correct (zero films actually
listed).** **No code change shipped** — an accurate "why each white venue is
white" run.

**Set changes vs 2026-07-14:**
- **Fell off the white set** (recovered / no longer 3-white): **Kino Sfinks**
  (was `needs-human` — film-dormant + markup drift) and **Kino Jaworzyna**
  (was `intentionally-dormant`). Both gone from the white set = no longer 3-white.
- **Returned to white this run:** **Kino PDK (Pyrzyce)** — dormant in the
  June/early-July runs, fell off for 07-07…07-14, now 3-white again. Re-probed
  live (below) — still dormant.
- **Carried over unchanged (14):** ADA Kino Studyjne, DKF Politechnika, Kino
  Chatka Żaka, Kino CK Lublin, Kino Krapkowice, Kino Kuźnica, Kino nad Wartą,
  Kino Świt, Kino Warszawa (Przeworsk), Kino Wisła Brzeszcze, Kino Zamek
  (needs-human), Kozienicki Dom Kultury, Patria, Studio (Opole), Teatr Ziemi
  Rybnickiej. (That's 15 carried + PDK returned = 16.)

**Out-of-scope heads-up (RED, not white — a fetch failure, different mode):**
**Kinomax** was 3-scrape-**failing** (red) this run — `CircuitOpenException:
circuit open for bilety.kinomax.info.pl`. The circuit breaker tripped after
repeated fetch failures, so it correctly surfaces red (not white); the system is
handling it. Not a white target; flagged here for the next run / a human.

Discovery method unchanged (`/uptime` is auth-gated): a mongosh replay of
`UptimeController`'s predicate against prod `uptimeBuckets` via the running
`flyctl proxy` on `127.0.0.1:27017` — per service, last 3 non-empty buckets all
`status==zero` (`successes==0 && failures==0 && zeroes>0`), excluding
`|enrichment` / the 6 enrichment sources / `img:*`. All 16 venues then live-HTTP
probed (one batched pass) to distinguish genuinely-dormant from a
recovered-but-broken parser. **Result: none recovered** — every white bar is the
correct output of a healthy parser against a venue with no current film programme.

**Live-probe verdicts (all DORMANT unless noted):**
- **Kino PDK** — `BiletynaClient` @ `biletyna.pl/Pyrzyce/Pyrzycki-Dom-Kultury`.
  ld+json `Place.events` has **2** events, both non-film (`ComedyEvent` 06.11,
  `TheaterEvent` 20.11); **0** `ScreeningEvent`. Parser correct — `intentionally-dormant`.
- **DKF Politechnika** — `FilmwebShowtimesClient` (Filmweb 1645). API `[]` for
  2026-07-18/19/25 — still the summer/academic break. `intentionally-dormant`.
- **ADA Kino Studyjne** — `BiletynaClient`. `<h2>Brak wydarzeń</h2>`, ld+json
  `events:[]`. `intentionally-dormant`.
- **Kino CK Lublin** — `Bilety24Client`. 0 `Kup bilet - Film:` anchors; only a
  Jazz-festival concert/workshop programme live. `intentionally-dormant`.
- **Kino nad Wartą (Koło)** — `Bilety24OrganizerClient` (KCK 1626). 0 `Film:`
  anchors, 30 non-film (Koncert/Spektakl). `intentionally-dormant`.
- **Kino Wisła Brzeszcze** — `Bilety24OrganizerClient` (1539). 0 `Film:`, 18
  concert + 4 show anchors. `intentionally-dormant`.
- **Kino Świt (DK Świt, Warszawa)** — `SwitClient` @ `dkswit.com.pl/kino/`.
  "Brak nadchodzących seansów filmowych", 0 `div.cks-movie-card`. `intentionally-dormant`.
- **Patria (Ruda Śląska)** — `KinoPatriaClient` @ `kinopatria.com/repertuar/`.
  Every slot "Brak filmu". `intentionally-dormant`.
- **Kino Warszawa (Przeworsk)** — `MsiClient`. July+August pages 0
  `div.movies-movie__single`; data endpoint `GetShortEventsWithFilters?date=2026-07`
  = `{"repertoireEvents":[],"dates":[]}`. Genuinely empty at the data layer.
  `intentionally-dormant`.
- **Kozienicki Dom Kultury** — `MsiClient` @ `bilety.dkkozienice.pl`. Data
  endpoint `repertoireEvents:[]`. `intentionally-dormant`.
- **Kino Kuźnica (SOK Suchedniów)** — `SystemBiletowyClient` @
  `shd.systembiletowy.pl`. `table.tbl_repertoire` header-only, 0 booking rows.
  `intentionally-dormant`.
- **Kino Chatka Żaka (UMCS)** — `KinoChatkaZakaClient`. Calendar empty (no
  `h3.header-light` / `div.box-row`). `intentionally-dormant`.
- **Kino Krapkowice** — `KdkKrapkowiceClient`. `view-kino`/`latest-kino-item`
  empty; known summer break → **31 Jul 2026** (still in window). `intentionally-dormant`.
- **Studio (Opole)** — `KinoStudioClient`. `kino-studio.html` 404s;
  `kino-studio-przerwa.html` still says "…nieczynne… Startujemy już 3 września"
  → break confirmed to **3 Sept 2026**. `intentionally-dormant`.
- **Teatr Ziemi Rybnickiej** — `TeatrZiemiRybnickiejClient`. `?type[]=film`
  returns the same 8 tiles as `?type[]=all` — all non-film (Festiwal/Koncert/Jam
  Session), **0 film**. `intentionally-dormant`. **Minor drift note (NOT
  actionable this run):** the site appears to IGNORE the `type[]=film` query
  param (returns unfiltered), so the client's film filter is a no-op — but the
  venue has zero film tiles regardless, so nothing is being hidden and there's no
  fail-before/pass-after test to write (can't assert "should show film X" when no
  film exists). Re-check when the venue programmes films: if a real film tile is
  then present but still filtered out, THAT is the test-backable bug — rebuild the
  film filter against tile markup instead of the ignored query param.
- **Kino Zamek (Szczecin)** — `KinoZamekClient`. Castle listing still yields only
  2 festival-banner slugs (`44-45-pomorskie-spotkania-z-diaporama`,
  `zamkowe-noce-filmowe-2026`), no individual film-title slugs. Unchanged standing
  `needs-human` festival filter-gap (see 2026-07-07). Likely self-resolves when
  normal repertoire resumes and individual film slugs return to the listing.

---

## 2026-07-14

**17 cinemas were 3-scrape-white** (real buckets ~17:15–18:00 Warsaw, newest
bucket 18:15 — actively scraping, within ~15 min–1h of the newest, not a boot
artifact). **Zero failing.** **TWO are new this run and were probed live —
BOTH `intentionally-dormant`.** **Fifteen are carried-over** venues, each still
within its known dormancy / needs-human window. **No code change shipped** — an
accurate "why each white venue is white" run.

**Prior fixes confirmed RECOVERED** (both gone from the white set, so the
2026-07-11 fixes worked end-to-end): **Kino Bajka** (fixed @010be6a82 — the
`data-dane` JSON parser) and **Kino Centrum Skarżysko-Kamienna** (fixed
@c8f656417 — MSI total-outage guard). Also fell off: **Kino Malta Charlie
Monroe** (its 3–16 Jul hiatus ends 16 Jul; no longer 3-white this run).

Discovery method unchanged (`/uptime` is auth-gated): a mongosh replay of
`UptimeController`'s predicate against prod `uptimeBuckets` via a fresh
`flyctl proxy` — per service, last 3 non-empty buckets all `status==zero`
(`successes==0 && failures==0 && zeroes>0`), excluding `|enrichment` / the 6
enrichment sources / `img:*`. (Note: the environment had a ~1-min network egress
outage at run start — Fly API + all HTTP down — which cleared on retry; the old
proxy had gone half-dead and was replaced with a fresh one on `127.0.0.1:27117`.)

**New white this run (probed live — both dormant):**
- **Jaworzyna (Kino Jaworzyna, Krynica-Zdrój)** — `intentionally-dormant`.
- **Kino Kuźnica (SOK Suchedniów)** — `intentionally-dormant`.

**Carried-over (15, unchanged — still white, within known windows, not
re-probed live this run):** ADA Kino Studyjne, Dyskusyjny Klub Filmowy
Politechnika (= "DKF Politechnika"), Kino Chatka Żaka, Kino CK Lublin, Kino
Krapkowice (summer break → 31 Jul), Kino nad Wartą, Kino Sfinks (needs-human —
film-dormant + markup drift), Kino Świt, Kino Warszawa (Przeworsk), Kino Wisła
Brzeszcze, Kino Zamek (needs-human — festival filter-gap), Kozienicki Dom
Kultury, Patria (Kino Patria, Ruda Śląska), Studio (Opole — break → 3 Sept),
Teatr Ziemi Rybnickiej. Each display name is unique in `Cinema.scala` and maps
to its prior diagnosis; the dated-window ones (Krapkowice → 31 Jul, Studio →
3 Sept) are all still inside their windows.

### Jaworzyna (Kino Jaworzyna, Krynica-Zdrój) — `intentionally-dormant`
- Client: `EkobiletClient` @ `https://ekobilet.pl/kino-jaworzyna` (slug
  `kino-jaworzyna`; previously scraped from Filmweb 561). Live: HTTP **200** (45 KB,
  no redirect / no Cloudflare), landing shows **"Brak wydarzeń na dzisiaj,
  sprawdź w innym dniu…"** with **0** `div.event-card` / **0** `p.overme`.
- The date strip has **9** `div.card-date[data-date]` days (12.07 → 20.07.2026)
  but **0** carry `available-color` — every one is `pointer-events-none` (today
  14.07 is `active-color` but still non-clickable). So there are **zero bookable
  days** to sweep via `?date=`, and no detail page to read. Markup is NOT drifted:
  the class-slot contract (`div.card-date` + `data-date` + `available-color` /
  `pointer-events-none`) is exactly what the parser expects — the `available-color`
  class is simply absent because nothing is programmed. Parser correct; small
  seasonal venue, no current schedule. Re-check next run.

### Kino Kuźnica (SOK Suchedniów) — `intentionally-dormant`
- Client: `SystemBiletowyClient` @ `https://shd.systembiletowy.pl` (VisualSoft
  ticketing; previously scraped from Filmweb 1713, which had gone silently empty).
  Live: `GET /index.php` → HTTP **200** (21.9 KB, no redirect / no Cloudflare).
- Skin-1 markup IS present but the repertoire table is **header-only**:
  `table.tbl_repertoire` = 1 element, but **0** `repertoire.html` booking links,
  **0** data `<tr>`/`<td>` rows (only the `Tytuł | Lokalizacja | Data | Kup bilet`
  header). All other skins absent (`div.event-item`=0, `data-date`=0,
  `h3.event-title`=0, `kup-bilet`=0). No film title/date anywhere. Not markup
  drift — the skin-1 selector matches the table correctly and finds zero rows.
  Parser correct; venue live but currently un-programmed (mid-July). Re-check
  next run.

---

## 2026-07-11

**18 cinemas were 3-scrape-white** (real buckets ~21:15–23:45 local, all within
~15 min–2h45 of the newest bucket — actively scraping, not stale). **Fourteen
are carried-over venues already diagnosed** (all still within their known
dormancy / needs-human windows); **FOUR are new this run** and were probed live —
**two fixed, one dormant, one needs-human.**

Discovery: `/uptime` is auth-gated, so a mongosh query against prod
`uptimeBuckets` (via the running `flyctl proxy` on `127.0.0.1:27017`) replicated
`UptimeController`'s predicate (last 3 recorded buckets all `status==zero`,
excluding `|enrichment` / the 6 enrichment sources / `img:*`). Newest bucket =
2026-07-11 00:00 Warsaw.

**New white this run (probed live):**
- **Kino Bajka (Lublin)** — `fixed` @010be6a82.
- **Kino Sfinks (Kraków, Nowa Huta)** — `needs-human` (film-dormant + markup drift).
- **Kozienicki Dom Kultury (Kozienice)** — `intentionally-dormant`.
- **Kino Centrum Skarżysko-Kamienna** — `fixed` @c8f656417 (swallowed 503).

**Fell off the white set since 2026-07-07** (recovered / no longer 3-white):
Kino Awangarda 2, Kino Paradox.

**Carried-over (14, unchanged — still white, within known windows, not
re-probed this run):** Kino CK Lublin, Kino Malta Charlie Monroe (hiatus →16 Jul
+ redesign needs-human), Kino Wisła Brzeszcze, Kino Świt, Patria, DKF Politechnika
(= "Dyskusyjny Klub Filmowy Politechnika"), Kino Krapkowice (break →31 Jul), Kino
Zamek (needs-human festival gap), Studio (break →3 Sept), ADA Kino Studyjne, Kino
Chatka Żaka, Kino Warszawa (Przeworsk), Kino nad Wartą, Teatr Ziemi Rybnickiej.
Each display name was confirmed unique against `Cinema.scala` and maps to its
prior diagnosis; the three with dated windows are all still inside them.

### Kino Bajka (Lublin) — `fixed` @010be6a82
- Client: `KinoBajkaClient` @ `kinobajka.pl/repertuar/`. Root cause: the WordPress
  page **stopped server-rendering** the schedule as HTML. The old parser keyed on
  `div.screening-day[id]` / `div.screening-item` — both now 0 in the server HTML.
  The whole advance window (35 days, 2026-07-11 → 09-27, real films: Vaiana,
  Minionki i straszydła, Zaproszenie, Toy Story 5, …) instead ships as an
  HTML-entity-encoded JSON blob in the `data-dane` attribute of `<div id="rep2">`,
  which the site's `rep2` widget `JSON.parse`s client-side. Blob shape:
  `{buy:<booking-host>, dni:{"YYYY-MM-DD":[{t,u,p,m,w,tag,s:[{g,h,x}]}]}}`
  (`m` = "genres · format · NNN min").
- Fix: rewrote the parser to read the `data-dane` attribute (jsoup entity-decodes
  it) and parse the JSON — title (via `kino-bajka` title rules), showtimes
  (time `g` + past flag `x`, paired with the day key), runtime off the `· NNN min`
  caption, poster, film URL, and the shared `buy` booking host. Fail-before /
  pass-after `KinoBajkaClientSpec` re-recorded against an 11-07-2026 capture (pins
  "Minionki i straszydła" 2026-07-11 13:30, runtime 90, the `buy` URL). Corpus
  fixture re-recorded and read-model + expected-schedules + all four rendered HTML
  snapshots regenerated (Bajka's real films — Vaiana/Minionki/Zaproszenie — are
  shared with the snapshot cities, so their poster/source fallbacks shifted). All
  layers green: `KinoBajkaClientSpec`, `FilmScheduleEndToEndSpec` (both e2e
  snapshots stable), `PageSnapshotSpec` (all 4). `ev:1` festival blocks do not
  leak as junk movie rows.

### Kino Centrum Skarżysko-Kamienna — `fixed` @c8f656417
- Client: `MsiClient` @ `https://bilet-mck.skarzysko.pl`. Root cause: the MSI
  portal returned **HTTP 503** (bare IIS/Microsoft-HTTPAPI error page, a real
  backend outage — no Cloudflare challenge) to BOTH month fetches. `MsiClient`
  wrapped each month in `Try(http.get(url)).getOrElse("")`, so the 503 was
  swallowed into an empty month and recorded as a successful "0 showtimes" —
  white, indistinguishable from a dormant venue. This is the same
  swallow-misclassification pattern fixed for `KinoAwangarda2Client` /
  `KinoPatriaClient` on 2026-07-07, but in the shared `MsiClient`.
- Fix: fetch both months, tolerate a *partial* failure (one month reachable still
  yields its screenings — the existing per-venue spec rows prove this), but if
  **every** month fetch fails, propagate the error so a dead portal surfaces red,
  not white. Fail-before / pass-after test in `MsiClientSpec` (`FailingHttpFetch`
  503 → `intercept[HttpStatusException].code shouldBe 503`); all 27 MSI spec rows
  + the e2e read-model guard stay green. Re-check the underlying repertoire once
  the host is reachable (couldn't judge dormant-vs-live while it's 503ing).
  NOTE: this hardens ALL MSI venues (Cinema1, GOK Tychowo, Nowa Ruda, Przeworsk,
  Sztum, Kozienice, …) against total-outage misclassification.

### Kozienicki Dom Kultury (Kozienice) — `intentionally-dormant`
- Client: `MsiClient` @ `https://bilety.dkkozienice.pl`. Both month pages
  (2026-07, 2026-08) return HTTP **200** with **0** `div.movies-movie__single`.
  Verified past the render layer: the portal's own data endpoint
  `/MSI/mvc/pl/Repertoire/GetShortEventsWithFilters?date=2026-07` returns
  `{"repertoireEvents":[],"dates":[]}` — genuinely empty at the data layer, not a
  fetch failure (so the new total-outage guard correctly leaves it white). No
  test-backable fix; re-check next run.

### Kino Sfinks (Kraków, Nowa Huta) — `needs-human` (film-dormant + markup drift, both true)
- Client: `KinoSfinksClient` @ `kinosfinks.okn.edu.pl/wydarzenia-harmonogram.html`.
  The parser targets `table.widok_listy tbody tr[onclick]` with a `Seanse`
  category label — that table is **gone site-wide** (the site moved to per-day
  URLs + `table_1/2/3.sekcja-paneli` CMS panels + a `table.icalendar` date-strip;
  `kategoria-189.html` → 404, now `wydarzenia-kategoria-189.html`). AND the venue
  is currently **film-dormant**: every per-day page 2026-07-11 → 27 (plus spot
  checks into Aug/Sep/Oct) and the Seanse category page all server-render
  `<div class="empty-results"><span>Brak wydarzeń</span></div>` — nothing
  scheduled in any category through October.
- Why no fix: with zero screening rows rendered anywhere, there is no film-row
  markup to sample, so a new parser can't be written or test-backed blind. A
  future parser must also treat `.empty-results` as zero screenings (not a parse
  failure). **needs-human — re-check once the venue repopulates its calendar**;
  then rebuild the parser against the new (populated) row shape.

---

## 2026-07-07

**16 cinemas were 3-scrape-white** (real overnight buckets ~00:30–03:30 local, not
a boot artifact). **Twelve are carried-over film-dormant venues; one new venue
(Kino Paradox) is dormant; one (Kino Zamek) is the same festival filter-gap still
`needs-human`; and TWO — Kino Awangarda 2 and Kino Patria — shared a real bug that
was `fixed` this run.**

The fix: `KinoAwangarda2Client` and `KinoPatriaClient` wrapped their PRIMARY
repertoire fetch inside `Try(parse(http.get(...))).getOrElse(Seq.empty)`, so a
5xx/timeout was swallowed into an empty list and recorded as a successful "0
showtimes" scrape — **white**, indistinguishable from a genuinely dormant venue,
when the fetch was actually FAILING (should be red). Moved the fetch outside the
Try so the HTTP exception propagates (the guard `KinoZamekClient` already
documents). Awangarda 2 is *live-proven* to hit this: its host (cyberfolks.pl
shared hosting) returned HTTP 503 "Serwer tymczasowo niedostępny / Script
execution exceeded allocated limits" to 6 consecutive retries — that 503 was being
mis-painted white. **Fixed @a4a2c149a** (+ shared `FailingHttpFetch` testkit fake;
fail-before/pass-after unit tests in both client specs). Patria was fixed in the
same commit for consistency (identical anti-pattern) though it is *currently*
dormant, not failing.

**Audit-only heads-up (not fixed):** three more clients swallow their PRIMARY
fetch the looser way — `Try(http.get(...)).getOrElse("")` feeding a parser:
`KinoDianaClient`, `KinoTatryClient`, `VisualTicketClient`. None is white today
for that reason (Tatry is intentionally-dormant per memory), so left alone;
worth a follow-up sweep to make fetch-failure→red uniform across all scrapers.

**Set changes vs 2026-07-03:**
- **New white:** Kino Awangarda 2 (fixed), Kino Paradox (dormant), Kino Zamek
  (back — was needs-human, off the set on 07-03).
- **Fell off (recovered / no longer 3-white):** ADA Kino Studyjne, Żuławski
  Ośrodek Kultury.
- **Carried dormant (unchanged, still white):** DKF Politechnika, Kino Chatka
  Żaka, Kino CK Lublin, Kino Krapkowice, Kino nad Wartą, Kino PDK, Kino Świt,
  Kino Warszawa (Przeworsk), Kino Wisła Brzeszcze, Teatr Ziemi Rybnickiej,
  Studio (Opole), Kino Malta Charlie Monroe.

Scope note: the twelve carried-over dormant venues were each diagnosed with live
evidence in prior runs and remain within their known dormancy/break windows
(Krapkowice → 31 Jul, Studio → 3 Sept, Charlie Monroe hiatus → 16 Jul). They stay
white; not deep-re-probed this run. The four changed venues below were probed live.

### Kino Awangarda 2 (Olsztyn) — `fixed` @a4a2c149a
- Client: `KinoAwangarda2Client` @ `awangarda.olsztyn.pl` (Joomla article id=77).
- Live: the host returned **HTTP 503** ("Serwer tymczasowo niedostępny … Script
  execution exceeded allocated limits") on every one of 6 retries — a genuine
  fetch failure, not an empty repertoire. `RealHttpFetch` throws
  `HttpStatusException` on a 503, but the client's `Try(parse(http.get)).getOrElse`
  swallowed it → white. Fix moves the fetch outside the Try (propagates → red).
  Cannot judge dormant-vs-parser-drift for THIS venue right now because the page
  is unfetchable; but the misclassification itself is fixed and regression-tested.
  Re-check the underlying repertoire once the host is reachable.

### Kino Patria (Ruda Śląska) — `intentionally-dormant` (+ swallow hardened @a4a2c149a)
- Client: `KinoPatriaClient` @ `kinopatria.com/repertuar/`. Live: HTTP 200 (36 KB),
  markup intact (`amy-movie-showtimews-daily-1` + weekly grid present, July date
  tabs present) but every movie item reads **"Brak filmu"** — no films programmed
  (typical mid-July single-screen closure). Parser correct. Its identical
  fetch-swallow anti-pattern was fixed in the same commit (not the cause of
  today's white, but corrected so a future 503 shows red not white).

### Kino Paradox (Kraków) — `intentionally-dormant`
- Client: `KinoParadoxClient` @ `kinoparadox.pl/repertuar/`. Live: HTTP 200 but the
  schedule now loads client-side via the WordPress `visualnet-importer` plugin,
  which shows `Błąd przy pobieraniu kategorii`; the old server-rendered selector
  `div.list-item__content__row[data-date]` finds **0** rows. That error is a red
  herring, though: the underlying VisualNet ticketing backend
  `bilety.kinoparadox.pl/index.php/repertoire` IS server-fetchable and returns
  `data-events-count="0"` for **every** day Jul–Dec 2026 (`"messages":"empty"`).
  So there is genuinely nothing to parse — venue film-dormant for the summer, no
  test-backable fix possible. **Re-check in autumn:** if VisualNet fills with
  events but our `/repertuar/` selector still finds nothing, THEN rebuild the
  scraper against the VisualNet `repertoire` HTML (structure is present + parseable)
  instead of the JS-injected WordPress page.

### Kino Zamek (Szczecin) — `needs-human` (same festival filter-gap as 2026-06-28)
- Client: `KinoZamekClient`. Strategy unchanged: intersect MSI ticketing titles
  with a film allow-list scraped from `zamek.szczecin.pl/wydarzenia/kino/`
  (`/wydarzenie/kino/<slug>/` links), keeping an MSI title only if its derived slug
  prefix-matches a listing slug.
- Live: MSI has genuine films for Jul–Aug (MOJA DROGA B., PANI Z TELEWIZJI, plus
  animated shorts CZERWONY KAPTUREK / OPOWIEŚĆ O ZŁOTEJ RYBCE / WIEŻA DZWONÓW …)
  mixed with non-film events (yoga, concerts, "LATO NA TARASACH"). But the castle
  listing now yields only **2 slugs**, both banners:
  `zamkowe-noce-filmowe-2026` (festival) and `44-45-pomorskie-spotkania-z-diaporama`
  (slideshow). The per-title→slug prefix match can't bridge an individual film to
  a festival-banner slug, so every genuine film is filtered out → white. This is
  the identical under-reporting escalated on 2026-06-28, now concretely the
  "Zamkowe Noce Filmowe 2026" summer festival.
- Why still no fix: unchanged product call — either follow the festival-banner page
  to enumerate its films and add them to the allow-list (concrete but adds
  banner-page parsing + uncertain whether those festival classics are what we want
  to surface), or drop the allow-list for a `NonMovieEventClassifier` (risks
  letting the MSI concerts/yoga through). Both are speculative without a confident
  ground-truth of "what should Kino Zamek show." **needs-human** — decide the
  policy; evidence reproducible via the two live URLs. Likely self-resolves when
  normal (non-festival) repertoire resumes and individual film slugs return to the
  listing.

---

## 2026-07-03

**14 cinemas were 3-scrape-white. Thirteen are genuinely film-dormant (parsers
verified working live — summer breaks or non-film programming); one (Kino Malta
Charlie Monroe) is `needs-human` — its site is mid-redesign and serves NO
fetchable showtime data anywhere.** No code change shipped this run.

Window caveat: the worker had booted ~06:45Z, so the 3 white buckets for every
venue span only this morning (07:45–08:45 local). The list still matches
`UptimeController`'s predicate exactly, but a couple of the *new* whites below
(vs 2026-06-28) could be early-morning "next repertoire not loaded yet" states —
re-check them next run before treating as settled.

**Still-white since 2026-06-28** (re-verified dormant, no recovery): DKF
Politechnika, Kino PDK, Kino Warszawa (Przeworsk), Studio (Opole), Żuławski
Ośrodek Kultury, Kino nad Wartą, Kino Chatka Żaka.
**New white this run** (not white on 2026-06-28): ADA Kino Studyjne, Kino CK
Lublin, Kino Krapkowice, Kino Malta Charlie Monroe, Kino Świt, Kino Wisła
Brzeszcze, Teatr Ziemi Rybnickiej.
**Fell off the white set since 2026-06-28** (recovered or no longer 3-white):
Kino MOK Nowa Ruda, Kino Zamek (Szczecin — was `needs-human`; not 3-white now).

### Kino Malta Charlie Monroe (Poznań) — `needs-human`
- Client: `CharlieMonroeClient` @ `https://kinomalta.pl/seanse`. Parser keys on
  `article.movie-card` + `application/ld+json` `ScreeningEvent` blocks — **both
  are gone**. The redesigned page now renders an **empty** `<div
  class="movie-list"></div>` with no server-side data behind it.
- Deep-probed the new site: **no HTTP-fetchable showtime source exists.** The
  schedule is filled client-side from *hardcoded demo JS* (the "Kup Bilet" modal
  literally reads `btn.dataset.id === "101" ? "La Grazia" : "Flow"` and
  `parseInt(btn.dataset.mockSeats)` — mock data, not a backend). `wp-json/` is
  blanket `401 rest_login_required`; every plausible `admin-ajax.php` action
  (`wpmoly_get_grid`, `get_showtimes`, …) returns `0` (unregistered); individual
  `/movies/<slug>` pages have an always-empty `div.screening-times`;
  `/baza-filmow` + the movie sitemap give titles/slugs only, no dates/times/booking
  links. Even a real browser would get an empty schedule + a fake ticket modal.
- Filmweb fallback can't rescue it either: **Charlie Monroe is not on Filmweb.**
  It's absent from the `/showtimes/Poznań` listing (so `FilmwebCinemaIdResolver`
  correctly leaves it UNMATCHED — no fallback id), Filmweb live-search returns no
  cinema entity (only films/people named "Charlie"), and the address-listed
  `Bułgarska 19` id 1618 returns `[]` seances for today. So the white bar is the
  *correct* output — own site broken AND Filmweb has nothing.
- Why no fix: there is literally no data to parse and no test-backable change to
  ship — the repo gate forbids a speculative parser against a nonexistent source.
- **Hiatus note (from Paweł, 2026-07-03): the cinema is on a break 3–16 Jul
  2026.** So during that window the empty schedule is *expected* — even a working
  scraper would show nothing. The redesign breakage above is a *separate*,
  still-real problem that only becomes observable once films should return.
- Action: **needs-human — re-check AFTER 16 Jul 2026.** If films are back on
  `kinomalta.pl/seanse` but the bar is still white, the redesign breakage is
  confirmed live: look for whatever backend now fills `.movie-list` (re-probe for
  `fetch(`/`admin-ajax`/`wp-json` wiring) and build a parser against it. If the
  site still serves only the hardcoded demo JS with no real feed, it stays
  needs-human until the owner finishes it (Filmweb won't help — the venue isn't
  listed there). Nothing actionable in our code today.

### ADA Kino Studyjne (Warszawa) — `intentionally-dormant`
- Client: `BiletynaClient` @ `https://www.biletyna.pl/Warszawa/ADA-Kino-Studyjne`.
- Live ld+json `Place.events` is `[]`; the page renders "Brak wydarzeń" (no
  events) and only lists unrelated concerts/kabaret as suggestions. Parser
  correct; venue has no screenings listed right now. (New white this run — could
  be a between-repertoires gap; re-check next run.)

### Kino Świt (DK Świt, Warszawa) — `intentionally-dormant`
- Client: `SwitClient` @ `https://dkswit.com.pl/kino/`. Parser keys on
  `div.cks-movie-card` — **0** in the live page, which itself reads "Brak
  nadchodzących seansów filmowych" (an admin empty-state). No films programmed.

### Kino Krapkowice — `intentionally-dormant` (summer break to 31 Jul 2026)
- Client: `KdkKrapkowiceClient` @ `https://kdk.krapkowice.pl/kino`. The
  `div.view-kino` container is present but has **0** `li.latest-kino-item`;
  selectors unchanged. The venue posted "Przerwa Wakacyjna w Kinie Krapkowice"
  (29.06.2026), reopening **31 Jul 2026**. Re-check after then.

### Teatr Ziemi Rybnickiej (Rybnik) — `intentionally-dormant`
- Client: `TeatrZiemiRybnickiejClient` @ `https://www.teatrziemirybnickiej.pl`.
  Parser requests `?type[]=film` → 0 tiles; the same `div.events-list a.item`
  markup returns 27 tiles for `?type[]=all`, all non-film
  (Koncert/Spektakl/Festiwal/Warsztaty/Wystawa/Kabaret). Parser healthy; no film
  programme.

### Kino CK Lublin — `intentionally-dormant`
- Client: `Bilety24Client` @ `https://ck-lublin.bilety24.pl`. `planChunks()`
  finds 20 event links on `/repertuar/` (pattern intact) but `parseEvent`'s
  `a.b24-button[title^="Kup bilet - Film:"]` matches none — all 20 live events are
  `Spektakl:` (14) or `Koncert:` (6). Cultural centre running only theatre/concert
  programming. (New white — re-check next run.)

### Kino Wisła Brzeszcze — `intentionally-dormant`
- Client: `Bilety24OrganizerClient` @
  `https://www.bilety24.pl/kino/organizator/kino-wisla-w-brzeszczach-1539`.
  200 (117 KB), 203 event anchors: **0** `Film:`, but 18 `Koncert:`, 4
  `Spektakl:`, etc. Parser keys on `Film:` → correctly empty.

### Kino nad Wartą (Koło) — `intentionally-dormant` (still, since 2026-06-28)
- Client: `Bilety24OrganizerClient` @
  `https://www.bilety24.pl/kino/organizator/koninskie-centrum-kultury-1626`.
  200 (134 KB), 239 anchors: **0** `Film:` (22 `Koncert:`, 6 `Spektakl:`, …).
  Unchanged from last run.

### Kino Chatka Żaka (Lublin/UMCS) — `intentionally-dormant`
- Client: `KinoChatkaZakaClient`, fetches
  `https://www.umcs.pl/pl/kalendarz-wydarzen,9469,1.lhtm`. Parser targets
  `h3.header-light` + `div.box-row`: **0** each; the calendar renders "Brak
  wydarzeń". Genuinely empty. (Consistent with the standing memory that Chatka
  Żaka is often film-dormant.)

### DKF Politechnika (Wrocław) — `intentionally-dormant` (still; summer break)
- Client: `FilmwebShowtimesClient` (Filmweb cinemaId **1645**).
  `/api/v1/cinema/1645/seances?date=…` returns `[]` for every date
  2026-07-03…07-10. University film club still on the summer break diagnosed
  2026-06-28. Expect recovery in the autumn term.

### Kino PDK (Pyrzyce) — `intentionally-dormant` (still, since 2026-06-25)
- Client: `BiletynaClient` @ `https://biletyna.pl/Pyrzyce/Pyrzycki-Dom-Kultury`.
  ld+json has 2 events, both non-film (`ComedyEvent`, `TheaterEvent`); **0**
  `ScreeningEvent`. Unchanged.

### Kino Warszawa (Przeworsk) — `intentionally-dormant` (still, since 2026-06-28)
- Client: `MsiClient` @ `https://bilety-kino.przeworsk.um.gov.pl` (NOTE: the
  2026-06-28 entry mentioned a `GetShortEventsWithFilters` endpoint — that string
  is **not** in the codebase; `MsiClient` GETs `<base>/MSI/mvc/pl?sort=Name&date=YYYY-MM`).
  July + August 2026 pages both 200 with **0** `div.movies-movie__single`. Empty
  repertoire.

### Studio (Opole) — `intentionally-dormant` (still on summer break to 3 Sept)
- Client: `KinoStudioClient` @ `https://mdk.opole.pl/kino-studio.html`.
  `kino-studio.html` still soft-404s (no `div.ckeditor`);
  `kino-studio-przerwa.html` carries "…nasze kino jest nieczynne… Startujemy już
  3 września". Re-check after ~3 Sept (confirm the repertoire URL/slug is back).
- Incidental (no scraping impact): `mdk.opole.pl` is now serving injected
  Russian-language casino spam in its `<head>`/meta — the host looks compromised.
  Our parser reads only `div.ckeditor`, which is unaffected, so no action, but
  worth knowing if their pages start misbehaving.

### Żuławski Ośrodek Kultury (Nowy Dwór Gdański) — `intentionally-dormant` (still)
- Client: `BiletynaClient` @
  `https://biletyna.pl/Nowy-Dwor-Gdanski/Zulawski-Osrodek-Kultury`. ld+json has 1
  event, `ComedyEvent` (kabaret); **0** `ScreeningEvent`. Unchanged from 2026-06-28.

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
