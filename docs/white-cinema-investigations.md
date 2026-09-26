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

`/uptime` is **auth-gated** at every one of its addresses — `https://kinowo.net/uptime`
(Poland), `https://showtimes.cc/<cc>/uptime` for `uk` / `de` / `us` / `es` — behind
OAuth + `ADMIN_ALLOWLIST` (`web/.../controllers/AdminAction.scala`); an anonymous
`curl` gets `401 "Not logged in."`. So query prod Mongo directly instead.

1. Mongo lives on the Hetzner host **`mongo-1`**, reached over an ssh forward
   (`scripts/local-mirror/prod-tunnel.sh`), usually already running — check with
   `ps aux | grep 'ssh -N -L 27017'`. The old `flyctl proxy` route died with the
   Hetzner migration; do not reach for it. Connect with `MONGODB_URI` from the
   root checkout's `.env.local` (it already points at `127.0.0.1:27017`).
2. **Sweep ALL FIVE countries.** Each country's uptime lives in its own database —
   `kinowo` (PL), `kinowo_uk`, `kinowo_de`, `kinowo_us`, `kinowo_es`
   (`Country.mongoDb`, `common/.../models/Country.scala`) — on the SAME
   connection, so iterate with `db.getSiblingDB(name)`. (Runs before 2026-07-28
   were PL-only; runs between 2026-08-02 and 2026-08-29 were PL-only because the
   other workers were stopped. Both are history — all five run now.)
3. Collection `uptimeBuckets`, docs `{service, bucket, successes, failures,
   zeroes, errors, fallback, ...}` — note the timestamp field is **`bucket`**, an
   ISODate, not `bucketTimestamp`. Replicate `UptimeController`'s predicate
   (`web/.../controllers/UptimeController.scala`): a bucket's status is
   `failures>0 ? (successes+zeroes>0 ? yellow : red) : successes>0 ? green : zero`;
   per service take the last `RecentScrapes = 3` non-`empty` buckets; the service
   is white when all 3 are `zero`. Skip `*|enrichment` and `img:` services.
   Service name = the cinema's `displayName` (`common/.../models/Cinema.scala`);
   map it to its client in `worker/.../services/cinemas/CinemaScraperCatalog.scala`.
4. **`uptimeBuckets` only retains ~24 h**, so "has this venue EVER been green?" is
   only answerable inside that window — and a venue white across ALL its retained
   buckets is long-dormant, not newly broken.

### Triage, because the set is ~1,400 venues

PL is ~325 services and its white list is small enough to probe venue by venue.
The other four are not: UK ~850 (`FlicksClient` → flicks.co.uk), DE ~1,540
(`WebediaShowtimesClient` → filmstarts.de), ES ~600 (same client → sensacine.com),
US ~5,000 (flicks.us). Cut them down with signals, and hand-probe only survivors:

- **A green→white transition inside the retained window is the sharpest cut** —
  a venue that WAS working and stopped. Everything else is a venue that has been
  white for as long as we can see.
- **Join `cinema_scrapes`** (`_id` = displayName; `scrapedAt` + `films` = the last
  CONTENT-BEARING scrape) to separate "had a programme days ago" from "never had
  one in this window".
- **Ask the AGGREGATOR whether it advertises a programme** before probing a venue
  site. DE/ES: the venue page's `data-showtimes-dates` attribute is the exact day
  list the scraper walks — `[]` means legitimately empty; the per-day JSON says
  `{"error":true,"message":"no.showtime.error","nextDate":null,"results":[]}`.
  UK/US: an empty venue renders Flicks' own `no-streaming-sessions` block ("we
  haven't received movie times for this cinema yet") and no `data-date` day tabs.
- **A whole client breaking shows up as a RATIO**, not as one venue. Compare each
  country's white share against the others and against the previous run here.

### Two ways a broken scraper hides from this sweep

Both were caught on 2026-09-04 and neither shows as a white bar:

- **The Filmweb fallback masks an empty own-site parse.** A bucket with
  `fallback: true` means the venue's own scraper returned nothing and Filmweb is
  covering for it — green bar, dead scraper. (Kino Agrafka and Kino Bułgarska 19
  were both in that state for four days.)
- **A partial parse stays green on whatever it still reads.** Kino Pod Baranami
  kept the five November/December special screenings its date regex could still
  match and dropped its ~200-showtime current week, reporting success throughout.

So whenever you diagnose a root cause, immediately ask which OTHER venues share
that code path and check them too, whatever colour their bar is.

### A fourth trap: cadence vs. retention window, at the low-cadence countries

Found 2026-09-08. `classify()` doesn't require 3 non-empty buckets — it takes
`.takeRight(3)` of whatever's there, so a service with only 1 bucket in the 24h
window still classifies white/green off that ONE bucket. At PL/UK/ES cadence
(60/420/420 min) that's rarely a problem — most services fill 3 buckets inside
24h. At DE (600 min) and especially US (840 min) it's structural: **no US
service had 3 non-empty buckets in this run's 24h window at all — max was 2,
and 336 of the 1,143 white US services had exactly 1.** Two consequences:

- **The green→white transition signal goes blind exactly where it's most
  needed.** DE and US both showed **zero** green→white transitions this run,
  not because nothing broke, but because a transition older than ~1-2 scrapes
  back (which at 840 min is not very far back at all) has already scrolled out
  of the 24h window.
- **The "archive age ≤10d + filmCount>0" cut is dominated by publication lag,
  not breakage**, at these cadences. Spot-checked DE's Film-Eck (age 1.1d,
  1 film) and UK's Electric Palace Harwich (a genuine green→white transition,
  not just an archive hit): BOTH have real, correctly-formatted upcoming
  screenings live on the aggregator right now that our last 1-2 scrapes simply
  missed because the venue hadn't published them yet when we asked. This is
  the exact shape 2026-09-04's Kinowerkstatt false-positive — replaying live
  captures through the real client is still the only way to tell a lag from a
  break, and at DE/US cadence, most single-film "recently broke" candidates
  will be lag. Weight the "recently broke" list accordingly; treat it as a
  probe queue, not a bug list.

### A fifth trap: the DE/ES per-day `.json` verification endpoint is dead

Found 2026-09-19. Prior runs verified a lag-vs-bug call by hitting
`https://www.filmstarts.de/kinoprogramm/kino/<id>/tag/<date>.json` (or the
ES `sensacine.com` equivalent) and reading `{"error":true,"message":
"next.showtime.on",…}`. That path now 404s unconditionally — including
against a known-busy control cinema — so it's gone, not venue-specific.
The real verification signal was always the venue page's own
`data-showtimes-dates="[…]"` attribute (`[]` = genuinely empty right now,
far-future-only dates = lag); the `.json` endpoint was only ever a second
opinion. Drop it from the checklist — lean on `data-showtimes-dates` alone,
cross-checked against a known-busy control cinema (confirms the fetch
method itself isn't blocked) rather than the per-day JSON.

### A second target: every venue on the Filmweb fallback

Added 2026-09-26. The Mikro screens sat on the fallback for three days, green
throughout, because kinomikro.pl's rebuild 404'd their feed. **Filmweb is
Poland-only**, so this target is `kinowo.filmwebFallback` only. The same-named
collection in the other four DBs holds the chain→Flicks fallback
(`fallbackSource: "Flicks"`, UK 235 / US 63 docs) and stale docs, never Filmweb
cover. The UK's 107-venue Cineworld/Odeon ENTER→RECOVER burst of 09-17/18 was
the Cineworld relaunch, not a Filmweb event. Every run lists docs with
`active: true`, plus any whose `history` shows an ENTER or RECOVERED since the
last run (a flapper is a broken scraper too), and diagnoses each one like a
white venue. `lastReason` and the newest-first `history` lines
(`<epochMs>\t<ENTER|PROBE_FAILED|RECOVERED>\t<reason>`, one per hourly
re-probe of the primary) usually name the failure outright:
- "primary returned no screenings": parse drift, a moved programme, or a dark venue.
- `HTTP 404/410`: the site was rebuilt or the slug renamed.
- `403`: an IP block.
- `5xx`/`CircuitOpenException`: the upstream is down.
- A TLS error.

Watch how the reason evolved and when it started. Log every active fallback
venue in a table (venue, since, last reason, verdict). A venue whose own site
is genuinely gone while Filmweb is complete is
`needs-human: move permanently onto Filmweb?`. It should not sit on the
fallback run after run. Baseline 2026-09-26: 2 active (both Mikro screens,
fixed the same day). See that entry's fallback table.

### A third target: `thin` buckets (screenings, but none in the next 72h)

Added 2026-09-26 (@650ad3211). The partial-parse mask above now has a flag.
A scrape that returns screenings but none from now (on the venue's own clock)
until 72h ahead marks its `uptimeBuckets` document `thin: true`
(`NearTermProgramme` in the worker, recorded by `UptimeRecordingScraper` and
`SourceFallbackScraper`). The bar stays green with a dashed violet outline. A
venue whose last 3 active buckets are ALL green-and-thin is pulled into its own
"Nothing in the next 72h" triage section on `/uptime`, so it shows even where a
large roster hides healthy rows.

Add it to the sweep: per service, take the last 3 non-`empty` buckets as
usual. A service is **thin** when all 3 are green (`successes>0`, no failures)
**and** have `thin: true`. The same mongosh pass that builds the white list can
emit this list.

**It is a probe queue, not a bug list.** On 2026-09-26 the rule flagged 53 of
517 green PL venues. Most are cultural centres whose next film really is days
or weeks away (Bielański OK: 29 days; MCK Tkacz; Karolinka). The high-yield
subset is **thin + a short span**: every screening falls inside the scrape's
own short window (Filmweb's ~7 days), with the front of the window empty.
All 9 PL Filmweb venues in that shape that day had `/info` `"multiplex":
"BILETYNA"`. For **7 of them**, biletyna sold screenings in the next 3 days
that Filmweb didn't list: Polonez (50), Giewont (15), Wołomin (8), Wadowice (7),
Len (7), Działdowo (9) and Chmielno (2). Filmweb only carried "Lalka"/"Obcy"
pre-sales from 09-30. The other 2 (MOK Police, Kongres Węgrów) genuinely had
nothing near on biletyna either. The check for each thin Filmweb venue: find
its biletyna place page via a Filmweb `orderLinks` URL (`biletyna.pl/film/<Title>/<City>`
→ the `/<City>/<Place>` link), then count `ScreeningEvent`s in the next 3 days.
Thin counts for other countries on 09-26, for comparison: UK 31/789, DE 52/1113,
US 60/4254, ES 6/414 (72h+ lead on the latest content-bearing scrape).

### A new capability: reading production logs without a browser, via the mongo-1 SSH hop

Found 2026-09-19, while running down what first looked like a real bug
(Grand Makwa Cinema Onamia, below). `logs.kinowo.net`/Grafana both need an
interactive Google-SSO browser session this environment doesn't have — but
`reference_logs_web_ui_victorialogs` also notes VictoriaLogs' OWN port
(`10.20.0.11:9428`) answers with **no credential** to anything on the
private Hetzner subnet. `mongo-1` (`root@178.105.221.61`, the same host
this whole methodology already SSHes into for the Mongo tunnel) sits on
that same subnet, so a plain `ssh root@178.105.221.61 curl …` reaches
VictoriaLogs' LogsQL HTTP API directly — no tunnel setup, no browser, no
SSO:

```
ssh root@178.105.221.61 \
  "curl -sG http://10.20.0.11:9428/select/logsql/query \
     --data-urlencode 'query=<LogsQL query>' -d 'limit=50'"
```

Each hit is one JSON object per line (NOT a JSON array — parse line by
line). This turns "is this venue's scrape actually broken, or does it
just look that way from the outside" from a guess into a fact: grep the
service's own `CinemaScrapeRunner`/`ChunkScrapePlanner` log lines and read
literally what the worker did on its last several attempts, instead of
inferring it from `cinema_scrapes` + a live re-fetch. Use this BEFORE
concluding a "real bug" on any white/red venue investigation, not just
this one — it would have shortened several past false-positive chases
(Astoria-Filmtheater, Cine Cortés, 09-15) from "live page has content, must
be broken" straight to "the worker's own log already shows it fetched fine
and correctly found nothing, so this is a gap, not a bug." A time-range
filter needs the bracket form, `_time:[<start>, <end>]` — the slash form
from VictoriaLogs' own docs 404s against this deployment's query parser.

---

## 2026-09-26

**Ninth all-five-country sweep.** Newest bucket 2026-09-25 23:15 UTC. There is
no 09-22 entry: that run never logged. PL was fully hand-probed. UK/DE/ES/US were
triaged with the archive-join + seasonal-name cut (the regex from the 09-19 note).
Probes of every non-seasonal archive≤10d candidate (US: the top 20 by archive
film count) went to background subagents. Every BREAK claim was re-verified
by hand before acting.

| DB | services | white | white % | red | green→white (in-window) | non-seasonal archive≤10d |
|---|---|---|---|---|---|---|
| `kinowo` (PL) | 537 | **6** | 1.1% | 1 | 0 | 2 |
| `kinowo_uk` | 852 | **52** | 6.1% | 0 | 0 | 11 |
| `kinowo_de` | 1,538 | **383** | 24.9% | 32 | 0 | 19 |
| `kinowo_us` | 5,039 | **783** | 15.5% | 4 | 0 | 111 |
| `kinowo_es` | 612 | **187** | 30.6% | 2 | 0 | 14 |

vs. 2026-09-19: PL 5/310→6/537. Services grew by 227 from the nearby-towns
roster sweep, and the white share fell. UK 49→52 (flat). DE 389→383 (flat;
seasonal 162/383 = 42.3%). US 741→783 (seasonal 129). ES 202→187, with 612
services now after the Ocine re-roster. No client shows a ratio jump. DE red
rose 21→32 (out of brief, not investigated). Max retained non-empty buckets per
white service: PL 40, UK 4, ES 5, DE 3, US 2. The cadence-vs-retention blind
spot is unchanged.

### Poland: 6 white + 3 masked (2 by the Filmweb fallback, 1 by a partial Filmweb parse): 5 venues fixed across 3 commits, 4 dormant

**Kino Mikro + Mikro Bronowice (Kraków): fixed, @867342024.** Neither was
white; both were **masked by the Filmweb fallback** (`fallback: true` on every
retained bucket; `filmwebFallback` active since 2026-09-23 ~13:00 UTC, 44/49
consecutive probe failures). The fallback history reads "primary returned no
screenings" from 09-23, then `HTTP 404 for GET kinomikro.pl/api.php/v1/repertoires`
from 09-24. kinomikro.pl was rebuilt from Joomla onto WordPress, and the old
Joomla JSON path died. The old feed was always a front for the venue's
VisualSoft ticketing instance: the old records even carried `system_url:
bilety.kinomikro.pl`. That instance's own
`service.php/repertoire/list.json?limit=1000&advanced=1` returns the whole
programme of both screens in one request (95 screenings to 10-29; `limit`
honoured; `meta.nbResults` = total), so the per-day walk is gone.
`KinoMikroClient` now reads it, keyed on `location.institution_name`. Dub tags
("Marsupilami- dubbing") are peeled into the showtime format. The director
regex gained Występują/Muzyka/`|` terminators, closed by `(?!\p{L})` because
Java's ASCII `\b` sees no boundary after "…ą". Spec replays a live 09-26
capture. The old client never requests that URL, so the spec fails before the
fix. The frozen 08-06 corpus only held the dead feed, so Mikro's rows left the
snapshots and its 7 dead corpus fixtures were deleted (same precedent as CSW
Toruń on 09-19).

  *Snapshot side-effect worth knowing:* `main` was already red on
  `e2e (rest)` (runs for c30cce89e and badf48c1d) because c30cce89e ("Drop a
  stored RT or Metacritic url whose page credits another director") shipped
  without regenerating `expected-schedules.txt`. This run's regen folded
  those RT drops in, and bumped `DerivationVersion` to f36edf955e2e6880.

**Regis (Bochnia): fixed, @17b810024.** White since 09-21 19:08 UTC (last
archive 09-21, 11 films). `bilety.kino.bochnia.pl/index.php` still carries 66
live `div.event-item[data-date][data-time]` rows on the visual9 skin, but the
title moved from `h3.event-title` to `h2.event-title`. The h3-pinned selector
matched nothing. The selector is now `.event-title`. Spec replays a live 09-26
capture (fails before, 22/22 after). **Shared code path checked:** all 15
VisualSoft venues were fetched live. Every other instance still renders h3
(title-count = event-item count) and all 14 were GGGG with same-day archives.
Only Bochnia had drifted. (Every VisualSoft instance also exposes
`service.php/repertoire/list.json`; bochnia, kgl, kck, shd, sta, bck, bdk, ckp,
chck, pckul, mok.zory, ock, kht and oks all answered with counts matching
their HTML. *Follow-up, same day: done.* `SystemBiletowyClient` now reads that
feed for every VisualSoft venue, Kino Mikro included (`KinoMikroClient`
deleted), and all four HTML skin parsers are gone. Farys and Kino Orzeł lack
the advanced template, so they get the plain feed. Nasze Kino is scoped by
`location.institution_name`, which also stops Oświęcim's concerts leaking in.
See the "VisualSoft venues read one JSON feed" commit.)

**Kino CKiS Skierniewice + Kino Polonez (Skierniewice): fixed, @c4d4ca925.**
CKiS had been white since 2026-09-22 22:39 UTC (last archive: 1 film, "Kura").
Filmweb 3149 returns `[]` for 09-26..10-03. `/info` shows 3149 is CKiS's
**concert hall at ul. Reymonta 33, not the cinema.** The cinema is Kinoteatr
Polonez (ul. Wita Stwosza 2/4), rostered separately as `KinoPolonez` on
Filmweb 320. **Polonez was the masked one.** Its bars read GGGGGG, yet Filmweb
320 also returns `[]` for 09-26..28 and its archive held a single "Lalka"
slot. Meanwhile biletyna's place page lists **50 screenings over 2 halls**
(Mistyczka, Tedi, Hot Spot, W sercu dziczy, Resident Evil, …). This is the
partial-parse mask again, only the partial source was Filmweb. Both venues
sell every ticket through biletyna: kino.cekis.pl's "kup bilet" goes through
bilety.io to biletyna. (The venue site's Mojeekino link is a VOD catalogue,
NOT a ticketing system. A subagent mapped its API,
`api-mojeekino.app.insysgo.pl/v1/InsysGoPage/GetPageContent`, and it holds
no dated screenings. Don't chase it again.) So both went onto the existing
`BiletynaClient`: `/Skierniewice/Kinoteatr-Polonez` and
`/Skierniewice/Centrum-Kultury-i-Sztuki-Sala-koncertowa`. The concert-hall
page mixes 9 ScreeningEvents with 19 concerts and plays, and the client's
`@type` + title filters keep only the films. Two `CinemaScraperCatalogSpec`
routing tests replay live 09-26 captures. The CKiS test was run against the
unmodified catalog first and failed on the missing Filmweb 3149 fixture, so
it fails before and passes after. `testUnit` is green, and the e2e snapshot is
unaffected (Skierniewice isn't in the frozen corpus).

  *Shared-path note:* Filmweb-backed PL venues can go "green on a stray slot"
  while their real programme is elsewhere. The white predicate cannot see it.
  *Follow-up, same day:* now flagged. See "A third target: `thin` buckets" in
  the methodology. The first measured thin list found 6 more Filmweb venues in
  the Polonez state (Giewont, Wołomin, Wadowice, Len, Działdowo, Chmielno), all
  with a biletyna place page to move onto. **Fixed, same day, @a54a375df**:
  each moved onto its biletyna place page after matching the address to
  Filmweb's `/info`. That work surfaced a platform bug: **a biletyna place page
  lists at most 50 events**, and 13 catalogued venues sat at exactly 50
  (Kameralne sells 132, Wadowice 110, Polonez 95). **Fixed @bedfb0c3f**: a full
  page is topped up from `/ajax/events?params[h]=<hall id from get_filter>`
  (paged `ipp=100`), and the feed's `category_id` maps 1:1 to the page's
  `@type`. A future sweep can treat a biletyna venue with exactly 50 events
  and no feed request in its logs as a regression of this.

**Still dormant, re-probed live (subagent, spot-checked):**

| Venue | Source | Verdict |
|---|---|---|
| Kino na Szekspirowskim (Gdańsk) | biletyna | **intentionally-dormant**: seasonal rooftop open-air, the 11th season (May–Sept 2026) is over |
| Kino Wisła Brzeszcze | bilety24 organiser 1539 | **intentionally-dormant**: "Brak wydarzeń"; ok.brzeszcze.pl says the cinema is closed indefinitely for thermo-modernisation. New fact; it is no longer a mystery |
| Dyskusyjny Klub Filmowy Politechnika (Wrocław) | Filmweb 1645 | **intentionally-dormant**: `[]` for 09-26..10-03; dkf.pwr.edu.pl last posted a programme in May. The October checkpoint from 08-24 comes due next run |
| Kino Chatka Żaka (Lublin) | UMCS calendar 9469 | **intentionally-dormant**: "Brak wydarzeń" for all categories |

Red (1): the same `Cineworld Enrichment` non-cinema row as 09-19. Out of scope.

### Filmweb-fallback sweep (PL only): 2 active, 5 recent flappers, all resolved or transient

Swept `kinowo.filmwebFallback` (219 docs) for `active: true` or any ENTER/RECOVERED in the last 14 days.

| Venue | Episode (UTC) | Reason (from `history`) | Verdict |
|---|---|---|---|
| Kino Mikro | since 09-23 13:48, active | "no screenings" from 09-23, then `HTTP 404` on `kinomikro.pl/api.php/v1/repertoires` from 09-24 (site rebuilt) | **fixed** @867342024, then moved onto the shared VisualSoft feed @34310a5f0. Deployed; awaiting its first post-deploy probe to release the fallback |
| Mikro Bronowice | since 09-23 12:39, active | same | **fixed**, same commits |
| Kino Wisła, Kino Atlantic (novekino.pl) | 09-12 11:14→18:14, 7 h | `CircuitOpenException` / timeouts on www.novekino.pl | **recovered**: one upstream outage, green since |
| Kino Astra (Oborniki) | 09-22 13:05→23:05, 10 h | `TimeoutException` on its biletyna host | **recovered**: transient, green since |
| KINOkawiarnia Stacja Falenica, Cinema1 | entered before the window, RECOVERED 09-13 | "primary returned no screenings" | **recovered**: back on their own-site sources (archives 09-26: 24 and 19 films) |

### Production check after the day's fixes (2026-09-26 ~05:40–07:40 UTC)

Checked prod Mongo after the fixes deployed:
- **Biletyna, 50-event cap:** every capped venue now carries its full programme — Kameralne 130 screenings (to 12-13), Wadowice 103, Polonez 91, Sztuka 92, Teatr Elektryczny 86, Rondo 76, Len 58.
- **VisualSoft JSON feed:** all green, including both plain-feed instances (Farys, Kino Orzeł) and the scoped venues (Nasze Kino 30, Mikro 74, Bronowice 21).
- **The eight venues moved to biletyna:** all green. The four whose oldest retained bucket was flagged `thin` show it only on that pre-switch bucket.
- **Filmweb fallback:** none active in PL. Kino Mikro recovered at 01:49 UTC.
- **Ocine:** Urban X-Madrid, Premium Estepark, Vila-seca and Mendibil each read 146–276 screenings from their own servers. **Ocine Blanes** timed out three times in a row around 00:36 UTC (`POST tickets.ocineblanes.es/api/v1/sessions` hit the worker's 30 s timeout), then was parked until its next 7-hour window. At 07:30 it read 19 films / 230 screenings, and the same server answered a local probe in 4–5 s. Verdict: **transient upstream outage, recovered**. No code change.

### UK: 52 white, 0 transitions, 11 archive≤10d candidates probed, 0 bugs

All 11 (Castlemorton, West Side Stromness, Phoenix Blyth, Lichfield Garrick,
Hythe Moviola, Wetherby Film Theatre, Theatr Colwyn, Coliseum Aberdare, Plaza
Stockport, Watersmeet, Ilkley) render Flicks' `no-streaming-sessions` block
with no `data-date` tabs. **EMPTY.** Controls (Finsbury Park Picturehouse, Odeon
Norwich) parse normally.

### DE: 383 white, 0 transitions, 19 non-seasonal archive≤10d probed, 0 bugs

16 read `data-showtimes-dates="[]"`. Windlicht (A2757, 16 films in its 6-day-old
archive) listed only 09-25, now past, so it is effectively empty. **Kulisse** and
**CLUB PASSAGE** (A0410) show near-term dates whose day content matches the
selectors, so both are **publication lag**. Control CinemaxX Kiel (A0279) is
populated.

### US: 783 white, 0 transitions, top-20 (by archive film count) of 111 probed, 0 bugs

14 EMPTY (`no-streaming-sessions`): Marquee Pullman Square 16, Cinemark
Century Mountain View 16, Geneseo, Heritage Park Altus 7, Southtowne Twin,
Moody Gardens, Angela Triplex, Kentucky Science Center, both Living Room
Theatres, T&T Twin, Trinity Weaverville, Illinois Theatre, Lights Of Liberty.
6 were **LAG**: day tabs present with `cinema-times__article` markup that
matches the selectors (Green River Campbellsville, Heritage Park Duncan,
Cinestar Huntsville, Cottonwood Havre, Huish Reel, National Graham 3). Control
AMC Empire 25 parses. That makes 34/34 US checks in two runs with zero
parser breaks.

### ES: 187 white, 0 transitions, 14 non-seasonal archive≤10d probed, 0 bugs

**The 5 Ocine venues** (Urban X-Madrid, Premium Estepark, Blanes, Vila-seca,
Mendibil) show an archive age of ~8.0 d because it predates the 09-25 evening
switch to `OcineClient`. Each chain server's `POST /api/v1/sessions` returns
10–16 live films in the shape `OcineParser` expects. SensaCine now shows `[]`
for them. **Lag across the switchover, not a break.** Re-check next run: they
should be green. (The subagent also claimed "worker-es has logged nothing on
09-26". That was an artefact: the run happened at ~23:30 UTC on 09-25, so
there was no 09-26 yet. Discarded.) **NovoCine Leiro 3D** (E0847) is
**LAG**. The remaining 8 are `[]`. Control Cinesa Marineda City is populated.

---

### Mephisto Augsburg (`A1560`) — `fixed` (retired)

Paged as UNCOVERED ("down 1656h and Filmweb has nothing to serve either"). The
venue is gone: Filmstarts 404s the id (its sibling Mephisto Ulm, `A1559`, still
answers 200), mephisto-augsburg.de now serves a betting-affiliate page, and the
cinema closed at the end of January 2026 after ARB Kino GmbH went insolvent. The
Staatstheater Augsburg is turning the hall into a stage. Removed from
`data/germany/regions.json` and `GermanRosterData` (1,529 → **1,528**), and
added to `CountrySpec`'s delisted-id guard.

The page itself was wrong too. A German venue has no Filmweb fallback, but the
wiring wrapped every eligible non-chain venue in a "Filmweb" `SourceFallbackScraper`
in every country. The Filmweb wrapper is now gated on `Country.filmwebEnabled`
(Poland only), so non-PL single venues get the plain uptime recorder.

### The other long-404ing venues (DE 30, US 3, ES 2) — 12 retired, the rest settled

Every venue whose archive row had answered 404/410 since 2026-08-31 was checked for
closure (own site, local press) and for a Filmstarts re-listing under a new id.

- **Retired as closed** (`CountrySpec` guards each id/name): DE `A0908` Kino Deutsches
  Haus, `A0680` Kino Idstein, `A2708` Kino im Badehaus Masserberg, `A1688` Lichtwerk
  Schmallenberg, `A0613` Hof-Theater Sigmaringen, `A0119` Burgtheater Gummersbach,
  `A0100` Kino Center Friedberg, `A0726` Kulturhaus Gotha, `A1547` Löwen-Lichtspiele
  Kenzingen (re-add if the town's planned reopening happens); ES `E0778` Yelmo Vialia
  Albacete, `E1013` Cine Mota del Cuervo; US Alamo Drafthouse Naples (the metro keeps
  `/naples/` through a `MetroDisplayNames` pin).
- **Duplicate**: `A1824` Kinomobil Besigheim — Filmstarts re-listed it as `A0793`,
  already rostered.
- **Operating, dropped by Filmstarts — `needs-human` only if a source is wanted**:
  Heimgarten Oberammergau, Holi Öhringen, Kinett Kusel, Filmpalast Lingen (`G02Q9`
  redirects to a dead `A0390`), KUKI Schlüchtern, Instituto Cervantes Hamburg + Berlin,
  Luxor Schwetzingen, Kino-Center Rendsburg, Loßburg, Wathlingen, Maxhütte-Haidhof,
  Lichtwerk Schwandorf, AJZ Chemnitz, Kurtheater Bad Sooden-Allendorf. Eight of these
  have a kinoprogramm.com page and are served by the German fallback once their
  Filmstarts scrape has failed three separate runs; the rest have no source.
- **Operating, dropped by flicks.us**: ACME Screening Room Lambertville, Acme Theatre
  Riverton — both still screening on their own sites.
- **Unclear, left rostered**: LOTTO Hamburg Autokino, Kulturkirche St. Stephani,
  Lu-Li Sommernachtskino, Kino am Seffersbach, Kino in den Felsengängen (seasonal).

Do not re-diagnose these; re-check only the unclear ones next season.

## 2026-09-19

**Eighth all-five-country sweep.** Newest bucket ~2026-09-19 08:00 UTC. Same
sweep script shape as prior runs. PL fully hand-probed; UK/DE/ES/US triaged
with the archive-join + seasonal-name cut, spot-checks delegated to
background subagents (per the standing delegate-probe-work convention) with
every "real bug" claim personally re-verified live before trusting it.

| DB | services | white | white % | red | green→white (in-window) |
|---|---|---|---|---|---|
| `kinowo` (PL) | 310 | **5** | 1.6% | 1 | 0 |
| `kinowo_uk` | 853 | **49** | 5.7% | 0 | 0 |
| `kinowo_de` | 1,538 | **389** | 25.3% | 21 | 0 |
| `kinowo_us` | 5,041 | **741** | 14.7% | 3 | 1 |
| `kinowo_es` | 600 | **202** | 33.7% | 2 | 0 |

vs. 2026-09-15: PL 5/312 (1.6%, flat — but a different 5: see below), UK
64/853→49/853 (7.5%→5.7%, down), DE 401/1538→389/1538 (26.1%→25.3%, flat),
US 1111/5040→741/5041 (22.0%→14.7%, down — the seasonal sawtooth swinging
back down again, consistent with the pattern documented since 09-04), ES
218/601→202/600 (36.3%→33.7%, down). PL's red count also dropped sharply
(3→1): Kino Roma and Kino MOK Nowa Ruda, both red on 09-15, are fully green
again this run with fresh same-day archives — no action needed, self-healed.

**Seasonal-name regex note:** re-derived the correct historical pattern
(`terraza|verano|open.?air|autokino|freibad|sommer|drive.?in`, from the
09-08 entry) after this run's first pass used an ad-hoc one that badly
undercounted DE (60 vs. the corrected 164/389 = 42.2% of the white list).
UK 0%, US 108/741 = 14.6%, ES 23/202 = 11.4% — all in line with prior runs'
shape. Use the exact regex above going forward; don't reinvent it.

### Poland — all 5 hand-probed live, 1 fixed, 1 recovered off the list, 3 confirmed still dormant

**Kino Centrum CSW Toruń — fixed, @900ee3607 (+ snapshot regen @d7bca6c5f).**
NEW to the white list this run (was RED on 09-15, not investigated then).
`cinema_scrapes` showed a real scrape 2026-09-17 11:13 (3 films),
`lastBarren.since` 2026-09-17 12:13 — a break roughly 2 days before this
run's cutoff. Root cause: the venue's site (csw.torun.pl) was rebuilt on
Nuxt sometime between the 09-15 and 09-17 scrapes. The old client's target,
the static `/repertuar/` page (dpProEventCalendar `div.box` markup), now
renders a client-side-loading Vue widget with none of that markup — the
fetch still 200s, so the client kept "succeeding" at zero. The new page
loads its schedule from a small WordPress REST route on the site's API
subdomain (`https://api.csw.torun.pl/wp-json/csw/v1/cinema-repertoire`,
found via the page's own asset requests) — a flat JSON array of
`{id, filmId, title, date, time, ticketUrl, href, bilety24}`, much simpler
than the old HTML. `KinoCentrumCswClient` now calls that endpoint directly
(API-over-browser, per standing preference) instead of scraping HTML;
`today`-based year inference is gone too since the API returns absolute
dates, so the `today` constructor param was dropped end-to-end (client +
catalog call site).

**Test:** `KinoCentrumCswClientSpec` rewritten to replay a fresh 2026-09-19
capture of the new endpoint (`test/resources/fixtures/csw-torun/api.csw.torun.pl/…`)
— the pre-fix client doesn't even request that URL, so this is a clean
fail-before/pass-after (`FileNotFoundException` under `FakeHttpFetch`
pre-fix, 5/5 green post-fix). `sbt worker/Test/test` green (3,499 tests,
after `./infra/bin/fetch-gitops` cleared the usual missing-local-checkout
`deploy.*` failures — same one-time setup step every run since 2026-09-04
has needed). `FilmScheduleEndToEndSpec` required a snapshot regen (the
frozen 08-06-2026 corpus only has the OLD `/repertuar/` fixture, not the
new API URL, so replaying the real client against it now correctly
produces zero for this one venue) — diffed clean (only Kino Centrum CSW
Toruń's contributions moved, no other film/cinema touched), reran to
confirm stability, both green. `web/PageTest/testOnly views.PageSnapshotSpec`
green with no HTML regen needed (Toruń isn't one of the three snapshotted
cities). Pushed to `main` at the end of this run (see below).

**Kino Lewart (Lubartów) — recovered, off the white list.** White on every
run since at least 09-12; this run shows 8/8 green buckets with a real
same-day scrape (6 films, 2026-09-19 07:46). No action — bilety24 must have
simply resumed publishing.

**The other 3 white PL venues — all re-probed live, all still confirmed
dormant, no change:**

| Venue | Source | What the source says |
|---|---|---|
| Kino Chatka Żaka (Lublin) | `umcs.pl` now 301-redirects to `www.umcs.pl/pl/kalendarz-wydarzen,9469,1.lhtm` | 0 `box-row`, page renders but empty |
| Kino Wisła Brzeszcze | bilety24 `…/kino/organizator/osrodek-kultury-w-brzeszczach-1539` (NOTE: the bare `/osrodek-kultury-w-brzeszczach-1539` URL without the `/kino/organizator/` prefix now 404s — "Bilety24 - napotkaliśmy problem" — that's just the wrong URL, not a break; the actual configured URL still works) | "Brak wydarze" (no events) |
| Kino na Szekspirowskim (Gdańsk) | biletyna.pl via Zyte (`httpResponseBody: true`, Cloudflare still blocks plain curl — see `reference_biletyna_cloudflare_blocks_all_ips`) | "Brak wydarz…" |

**DKF Politechnika (Wrocław) — still white, 11th consecutive run, academic-break checkpoint not yet due.** `Dyskusyjny Klub Filmowy Politechnika` (its full service name — the log's shorthand "DKF Politechnika" is the same venue). Filmweb 1645 `/api/v1/cinema/1645/seances?date=…` returns `[]` for 09-19/20/21; `/api/v1/cinema/1645/info` still resolves fine. The 08-24 checkpoint ("escalate if still `[]` in October") has not yet triggered — today is 2026-09-19. Next run (~09-22) should watch for the October rollover.

**Red (1, down from 3):** `Cineworld Enrichment` — an enrichment-pipeline service name that slipped past the `*|enrichment` filter (no pipe in the name) but isn't a cinema scrape; out of scope, not investigated. Kino Roma and Kino MOK Nowa Ruda (both red on 09-15) are fully green again — self-healed, no action.

### UK — 49 white (down from 64), 0 in-window transitions, all 6 archive≤10d candidates hand-checked, 0 bugs

43 of the 49 have no `cinema_scrapes` archive within 10 days (long-tail,
not individually probed this run). The 6 with a recent archive — Castlemorton
Cinema (Morton Majestic), Chiddingfold Village Hall Cinema, Heart Centre
Headingley, Ilkley Cinema, Pavilion Cinema Whitby, Regent Blackpool — were
all fetched live (`flicks.co.uk/cinema/<slug>/`): every one renders Flicks'
own `no-streaming-sessions` block with **zero `data-date` tabs at all** (not
even far-future ones) — genuinely empty right now, not a lag case, not a
bug. Cross-checked two of them against the AJAX sessions endpoint directly
(200, 0 bytes) to confirm it's Flicks itself with nothing listed, not a
parse failure. No fixes, nothing new vs. 09-15's shape besides the drop in
raw count (64→49).

### DE — 389 white (down slightly from 401), 0 in-window transitions, all 14 archive≤10d candidates checked, 0 bugs

164/389 (42.2%) seasonal-named (open-air/Freiluftkino/autokino — see the
regex note above). Of the 14 non-seasonal-or-borderline candidates with a
≤10-day archive (Central-Kino Borgentreich, 4× Freiluftkino-named venues
that the imperfect first-pass regex missed, Hafenkino Wilhelmshaven,
Hansafilmpalast, Kino Neu Anspach, Kino Wesenberg, Kino-Center Forchheim,
2× Kommunales Kino venues, Magic Cinema im Europa Park, Neues Theater Sankt
Wendel) — every one's `data-showtimes-dates` on filmstarts.de reads exactly
`[]` (not even a far-future gap-list). Verified against a live control
cinema (CinemaxX Kiel, A0279) returning 20+ real dates including today, so
the fetch method itself isn't blocked. **0 bugs, 0 lag cases** — this
batch is a clean bill of health. Red (21) unchanged in shape, out of brief.

### US — 741 white (down from 1,111, seasonal sawtooth swinging back down), 1 in-window transition (resolved, not a bug), 0 real bugs after log-verified correction (see below)

108/741 (14.6%) seasonal-named. The one in-window green→white transition,
**Montalbán Theatre** (Hollywood), is a one-off special-screening venue:
its last archive entry is a SINGLE showing of "Project Hail Mary" on
2026-09-18 22:00 UTC with a `bookingUrl` pointing at the venue's own site
(a special-event booking, not the venue's regular repertoire calendar). The
live page now renders Flicks' `no-streaming-sessions` block with no day
tabs — the one-off screening already happened and nothing is scheduled
next. Not a bug.

Sampled the higher-value candidates from the 88 non-seasonal, archive≤10d
list (prioritizing multi-film venues, since a venue with several films
going to zero at once is a stronger signal than a single-film one):

- **Fox 5 Theatre Sterling, Three Star Cinema McMinnville, Palace Theatre
  Lake Placid, Reel Time Cinema Brookfield, Welden Theatre St Albans, Capri
  Theatre Shelbyville, Kentucky Science Center Louisville, McCurtain Cinema
  Idabel, Moody Gardens Theater Galveston, Princess Theatre Rushville
  Indiana, The Grand Theatre Oelwein, The Showhouse Othello** — all
  genuinely empty right now (Flicks' `no-streaming-sessions` block, no near
  or far day tabs). Not bugs.
- **Canal Place Philadelphia 7** (archive: 7 films 1.7d ago) and **Marquee
  Pullman Square 16** (archive: 16 films 1.7d ago) both looked alarming at
  first — high film counts suddenly at zero — but each has only **ONE**
  scrape bucket in the 24h window (US's 840-min cadence means most venues
  get 1-2 buckets, not 3), and live-checking both found real, current
  content: Canal Place's page shows "6 movies / 67 showtimes" this week and
  its AJAX sessions endpoint (`/cinema/sessions/canal-place-philadelphia-7/2026-09-19/`)
  returns 6 real `cinema-times__article` blocks right now. This is the
  documented cadence-vs-retention blind spot (a single stale zero-reading,
  not a confirmed break) — the site is fine, the classification is just
  noise from n=1. No action; will read green on the next scrape.
- **Grand Makwa Cinema Onamia (`grand-makwa-cinema-onamia`) — corrected
  to self-healing gap, NOT a bug (see below for how the first pass got
  this wrong).** `cinema_scrapes` showed `lastBarren.since: 2026-09-14`
  (5.6 days barren) with the last real archive entry 2026-09-13 (3 films,
  a single screening day). The live page carried real `data-date` tabs for
  2026-09-19/20/22 when checked, and its AJAX sessions fragment returned 3
  real films with valid markup — which read at first like a live parser
  bug, since replaying that exact captured HTML through the real
  `FlicksClient.parseDay`/`parseProgrammeDates`/`hasTimetable` (a throwaway
  spec, deleted, never committed) parsed it correctly. That was genuine
  evidence the CODE isn't broken, but not by itself evidence there's no
  bug — the same "live page has content, our archive doesn't" shape the
  09-15 entry already flagged as a false-positive trap for Astoria-Filmtheater
  and Cine Cortés, just not recognised as the same trap at first.

  **Resolved conclusively via production logs**, reached over an SSH hop
  through `mongo-1` (same private Hetzner subnet as the `monitoring-1`
  VictoriaLogs host, `10.20.0.11:9428` — no Google SSO needed, see the new
  `reference_victorialogs_via_mongo1_ssh` methodology note below). Every
  `ChunkScrapePlanner` log line for this venue since 2026-08-30 shows
  `enqueued N/N chunk task(s)` (a real day-tab list found) — **except every
  attempt from 2026-09-14 onward, which logs NO planner line at all**.
  Reading `ChunkScrapePlanner.plan()` (`worker/.../services/tasks/ChunkScrapePlanner.scala:71`):
  `if (keys.isEmpty) { publishEmpty(scraper); return 0 }` — when
  `planChunks()` finds the timetable block but zero `data-date` tabs, it
  publishes a clean "successful scrape of an empty repertoire" with NO log
  line, by design (see the doc comment above `publishEmpty`). So the
  worker fetched this venue's real page roughly every 10h from 09-14
  through the last attempt at 09-18 23:29 UTC and found **zero day tabs
  every single time** — a genuine, extended dark period (this is a
  single-screen small-town cinema that appears to run one screening day
  then go fully dark for up to a week), not a fetch or parse failure. It
  only *looked* fixed-worthy because the site had freshly republished a
  new block sometime in the ~11h between the worker's last attempt and my
  live check — the exact self-healing-lag shape, just with an unusually
  long (5-6 day) gap rather than DE/ES's typical few days. **No code
  change; nothing to fix.** Correcting the run's earlier `needs-human`
  verdict to `self-healing gap — confirmed via logs`.

  **Process note for next time:** a live page having content our archive
  doesn't is NEVER enough by itself to conclude a break, even after ruling
  out a parser bug — the archive only proves what the code found in the
  PAST; it says nothing about when the content was published. The
  question that actually distinguishes a break from a gap is "did the
  worker's OWN last few attempts see this content and drop it, or did the
  content simply not exist yet when they ran?" — and that can only be
  answered by reading what the worker itself logged, not by re-fetching
  the live page a second time.

**Baseline sample, completed after the main run:** 12 of the remaining 72
non-priority (1-film-archive) candidates, drawn at random — Neola Phoenix
Theater, Egyptian Coos Bay, Redwood Theatre Brookings, Black Bear Cinemas,
Capitol Cinema Aberdeen, Cinema Underground Williams Center, Pensacola
Cinema Art, Boone Theater, H&S Theatre Chandler, Arlington Cinema &
Drafthouse, Barron Theatre Pratt, Tahqua-Land Theatre – Land Newberry.
**12/12 genuinely empty** (Flicks' own `no-streaming-sessions` block, zero
`data-date` tabs) — same confirmation rate as the priority sample. Combined
with the 16 priority candidates checked earlier (1 self-healing-gap
[Grand Makwa, corrected above], 2 cadence-noise, 13 genuinely-empty), this
run's overall US archive≤10d confirmation rate is 28/28 checked = 0 real
bugs, giving a real ratio for comparison against future runs: at US cadence,
an "archive≤10d" white candidate is overwhelmingly either genuinely dark or
a stale single zero-reading, essentially never a live parser break. Red (3)
unchanged in shape, out of brief.

### ES — 202 white (down from 218), 0 in-window transitions, 25 archive≤10d candidates checked (11 Ocine-chain + 14 others), 0 bugs

23/202 (11.4%) seasonal-named. Noticed the "Ocine"-branded chain (19 venues
total in the ES roster) had 11 of its 19 venues white this run — a 58%
white share vs. the overall ES white rate of 33.7%, which looked like it
could be a shared-client/chain-specific break. Spot-checked 4 live
(3 white — Ocine Arenys E0651, Ocine Blanes E0462, Ocine Granollers E0507 —
plus 1 non-white control, Ocine Girona E0362): all 3 white ones read
`data-showtimes-dates="[]"` on sensacine.com right now, while the
non-white control shows a full 15-date near-term list. **Not a shared
client bug** — sensacine.com itself just currently has nothing listed for
those specific venues; a coincidental chain-wide lull, not our parser.
*(Follow-up 2026-09-25: not a lull — SensaCine never listed a real programme
for most Ocine venues. They now scrape the chain's own ticketing servers via
`OcineClient`; see `data/spain/ocine.json`.)*
The other 14 non-Ocine archive≤10d candidates (including 3 higher-film-count
ones given extra scrutiny: Cine Príncipe 11 films, Cines Antiguo Berri 6,
Cines Avenida 3D 4) were all confirmed `data-showtimes-dates="[]"` too,
cross-checked against two known-busy control theaters (Cinesa As Cancelas,
Cinesa Marineda City) that correctly returned populated date arrays. **0
bugs, 0 lag cases** across all 25 checked. Red (2) unchanged in shape, out
of brief.

---

## 2026-09-15

**Seventh all-five-country sweep.** Newest bucket ~2026-09-15 18:15 UTC. Same
sweep script shape as prior runs (`classify()` replicated, `.takeRight(3)`, skip
`*|enrichment` / `img:`). PL fully hand-probed; UK/DE/US/ES triaged by delegating
the mongo-aggregation + live-spot-check legwork to background subagents (per
this session's own delegate-probe-work convention), with every subagent claim of
a "confirmed bug" personally re-verified live before trusting it — see the DE/ES
finding below for why that mattered.

| DB | services | white | white % | red | green→white (in-window) |
|---|---|---|---|---|---|
| `kinowo` (PL) | 312 | **5** | 1.6% | 3 | 0 |
| `kinowo_uk` | 853 | **64** | 7.5% | 0 | 0 |
| `kinowo_de` | 1,538 | **401** | 26.1% | 21 | 0 |
| `kinowo_us` | 5,040 | **1,111** | 22.0% | 3 | 0 |
| `kinowo_es` | 601 | **218** | 36.3% | 2 | 13 |

vs. 2026-09-12: PL 5/310 (1.6%, flat), UK 60/852 (7.0%→7.5%, flat), DE 390/1537
(25.4%→26.1%, flat), US 715/5040 (14.2%→22.0%, up), ES 198/601 (32.9%→36.3%, up).
US's jump is the same weekly sawtooth the US seasonal cohort has shown every
run so far (see `reference_showtimes_weekly_sawtooth`): seasonal (mostly
drive-in) white share bounced back to 78% (245/314) after dropping to 34% on
09-12 — not a new trend, a swing this cohort has now shown three separate times
(09-04 low, 09-08 Labor-Day high, 09-12 low, 09-15 high again). ES's rise is
explained below (13 fresh transitions, all lag not breakage).

**In-window green→white transitions read 0 for PL/UK/DE/US again** — this is
the same cadence-vs-24h-retention blind spot documented on 09-08: DE and US
cadence (600/840 min) means a transition older than ~1-2 scrapes back has
already scrolled out of the 24h window by the time the sweep runs. Confirmed
concretely this run: Astoria-Filmtheater (DE, see below) has an
archive-attested green scrape at 2026-09-13 23:39, comfortably a real
green→white transition, but that bucket is >24h old by 18:15 on 09-15 and
had already scrolled out. Don't read "0 transitions" as "nothing broke" for
these four; ES's cadence (420 min) is the only one of the five short enough for
the in-window signal to still be reliable, which is exactly why it's the one
showing 13.

### ⚠️ New methodology finding: a far-jump in the day-list is a WEEKLY-GAP venue self-healing, not a parser bug — and two background subagents got this wrong

Both the DE and ES background triage agents flagged what looked like a live
"confirmed bug": a recently-white venue whose aggregator page, fetched live,
carries real upcoming showtimes. I re-checked both by hand before believing
either, per the standing "positive control before believing a negative" /
"don't claim wrong without evidence" rules, and **both were false positives**,
with an identical shape:

- **Astoria-Filmtheater (DE, theaterId A0020)** — `cinema_scrapes` shows a
  real scrape on 2026-09-13 (2 films, 16:00 showtimes), `lastBarren.since`
  2026-09-14 00:09. Fetched `filmstarts.de/kinoprogramm/kino/A0020/` live:
  `data-showtimes-dates="[2026-09-18,2026-09-19,2026-09-20]"` — no 15/16/17 at
  all. The per-day JSON for 09-15/16/17 all answer
  `{"error":true,"message":"next.showtime.on","nextDate":"2026-09-18",…}`;
  09-18 answers with a real film. This is a single-screen venue that plays a
  few days, goes dark, and plays again — the site's own day-list SKIPS the gap
  entirely, and `WebediaShowtimesClient.planChunks()` correctly reads that
  skip as "nothing to fetch between now and the 18th," which is exactly what a
  gap-programming venue looks like. It isn't broken; it just hasn't reached
  the next screening block yet, and the aggregator only started advertising
  that block sometime after our last scrape.
- **Cine Cortés (ES, theaterId E0250)** — same exact shape.
  `cinema_scrapes` shows a real scrape 2026-09-14 18:38 (2 films, 19:00/21:15
  showtimes that Monday), `lastBarren.since` 2026-09-14 22:37. Live
  `sensacine.com/cines/cine/E0250/`: `data-showtimes-dates=[2026-09-19,20,21]`
  — nothing for 15/16/17/18. Per-day JSON for 09-15/16 answers
  `next.showtime.on` → `nextDate: 2026-09-19`; 09-19 has real films. Monday
  screening, then dark until Friday. Same non-bug.

**Both `FlicksClient` and `WebediaShowtimesClient` share this exact
"day-list, not fixed grid" design** (`planChunks()` reads the venue's own
day-tab/attribute rather than probing a fixed window), so the failure mode —
and the false-positive-bug shape — generalizes across all four non-PL
countries. I spot-checked a fifth case in the same family, **Town Hall Theatre
Quincy (US, `flicks.us/cinema/town-hall-theatre-quincy/`)**: live page shows
`data-date="2026-09-18/19/20"`, nothing nearer — identical shape, also not a
bug. Four OTHER US samples from the same "archived ≤3 days but white" bucket
(Elk Rapids Cinema, Rex Theater Stanley, Dependable Drive-In Coraopolis, Kenda
Drive-In) rendered Flicks' `no-streaming-sessions` block with no live day tabs
at all — genuinely, currently empty, not lag.

**Net effect: no code fix from either flagged "bug."** Both venues should
self-heal on their clients' next scheduled scrape once the newly-published
block enters the fetch window (DE cadence 600 min, ES 420 min — both already
overdue as of this run's data cutoff). File this pattern alongside the
existing DE/US "cadence vs. retention window" trap from 09-08 as a standing
caution for future runs: **when a background agent (or a human) reports "the
aggregator has real content but we're not scraping it," always check whether
that content is for a NEAR date (real bug) or a FAR date past a gap the venue's
own day-list currently skips (self-healing lag) — the two look identical from
the archive alone and only the live per-day JSON around `today` distinguishes
them.**

### Poland — all 5 hand-probed live, 0 fixed, all confirmed dormant/lag, 1 new to the list

| Venue | Source | What the source says |
|---|---|---|
| Kino Lewart (Lubartów) | bilety24 `…/lubartowski-osrodek-kultury-1382` | "Brak wydarzeń" (no events) |
| Kino Wisła Brzeszcze | bilety24 `…/osrodek-kultury-w-brzeszczach-1539` | "Brak wydarzeń" |
| Kino Chatka Żaka (Lublin) | `umcs.pl/pl/kalendarz-wydarzen,9469,1.lhtm` | 0 `box-row`, page renders but empty |
| DKF Politechnika | Filmweb 1645, `seances?date=` for 09-15/16/17 | `[]` on all three — **10th consecutive run white**. Cinema still exists on Filmweb (`/cinema/1645/info` resolves fine) — this is a DKF (student discussion film club) that plausibly runs on the academic calendar, not a broken scraper. The 08-24 checkpoint ("escalate if still `[]` in October") has STILL not triggered — today is 2026-09-15 — but the next run (~09-18) or the one after (~09-21) should watch for the October rollover and escalate then if it hasn't recovered. |
| **Kino na Szekspirowskim (Gdańsk)** — NEW to the list | biletyna.pl `Gdansk/Kino-na-Szekspirowskim` | `cinema_scrapes` shows a real scrape 2026-09-12 18:01 (1 film, one 20:00 showtime), `lastBarren.since` 2026-09-12 18:31. **biletyna.pl now Cloudflare-blocks even a plain residential `curl`** (see `reference_biletyna_cloudflare_blocks_all_ips`), so probed the way the scraper's Zyte fallback does: `curl -u "$ZYTE_API_KEY:" … -d '{"url":…,"httpResponseBody":true}' https://api.zyte.com/v1/extract` (note: `browserHtml:true` 401'd against this key — `httpResponseBody:true` is what still authenticates). Decoded body reads "Brak wydarz…" — genuinely empty right now. A single-showtime venue that went quiet 3 days after its one screening reads as low-frequency programming, not a break; logging as intentionally-dormant unless it stays white for several more runs. |

No PL venue currently carries an active Filmweb fallback (`filmwebFallback.active: true` — empty this run), so no masked breaks to chase this time.

**Red (3, shape changed from last run, out of brief — not investigated):** Kino Roma, Kino MOK Nowa Ruda, Kino Centrum CSW Toruń (Kino Przedwiośnie and Nowe Kino Warszawa, red on 09-12, are no longer red).

### UK — 64 white, 0 in-window transitions, triaged via subagent, nothing new

Delegated the archive-join + spot-check to a background agent. Results: 54 of
64 have no `cinema_scrapes` entry ever (long-tail, presumptively never had a
scrape wired under this exact name or genuinely never screened); 7 share one
stale archive with `lastBarren.since = 2026-08-31` (15 days dormant — Village
Picture House Cuddington, The Barn Banchory, Belmont Filmhouse, Fuse Community
Cinema Prudhoe, Little Theatre Sheringham, Flix Student-Run Cinema
Loughborough, Phoenix Cinema Blyth); 3 have an archive ≤10 days old (Baldock
Arts & Heritage Centre, Pavilion Cinema Whitby, The New Vic Tisbury Village
Hall) and all 3 were spot-checked live on flicks.co.uk — all render
`no-streaming-sessions` with no day tabs, genuinely empty right now, not lag.
0 seasonal-named. No fixes, nothing new vs. 09-12's shape.

### DE — 401 white, 1 confirmed false-positive "bug" (see above), otherwise flat

Seasonal-named venues in the full 1,538-strong roster: 212, of which 205 (97%)
are white — consistent with every prior run's late-summer pattern. Of the 196
non-seasonal white venues: 61 have no archive ever, 8 have an archive >10 days
old (spot-checked 4 of them live — Akademie der Künste, Filmtheater Wertingen,
Kronen-Lichtspiele Rodgau, Campusfilmnächte — all confirmed `[]` on
filmstarts.de), and 1 (Astoria-Filmtheater) has an archive ≤10 days old, which
is the publication-lag false-positive detailed above. Red (21) unchanged in
shape, out of brief.

### US — 1,111 white, seasonal sawtooth back up, 1 false-positive checked, rest sampled genuinely empty

Seasonal-named venues (drive-ins mostly): 314 of the 5,040-strong roster, 245
(78%) white — the sawtooth swing described above, not a new trend. Of the
remaining ~865 non-seasonal white venues, 828 have a `cinema_scrapes` archive
≤10 days old (255 of those ≤3 days) — a much larger "recently active" share
than any prior run, but this is the known US cadence (840 min) structural
blind spot amplified by how many small US venues (historic single-screens,
drive-ins, revival houses) run non-daily/weekly programming: at this cadence
almost every gap between screening blocks classifies as "recently broke" by
the archive-age heuristic alone. Sampled 5 directly rather than trusting the
raw count: Town Hall Theatre Quincy is the same far-jump/self-healing shape as
Astoria-Filmtheater/Cine Cortés (live page already shows 09-18/19/20, nothing
nearer); Elk Rapids Cinema, Rex Theater Stanley, Dependable Drive-In
Coraopolis and Kenda Drive-In all rendered `no-streaming-sessions` with no day
tabs — genuinely, currently empty. Treating the 828 as a probe queue, not a
bug list, per the 09-08 methodology note; not exhaustively probed given the
volume and this run's 5-for-5 confirmation rate of "not a bug." Red (3) out of
brief.

### ES — 218 white, 13 green→white transitions (real, in-window — cadence is short enough to catch them), verdict: shared PUBLICATION-DAY clustering, not a shared outage, self-healing

All 13 transitioned venues' `lastBarren.since` cluster tightly between
2026-09-14 22:37 and 2026-09-15 03:48 UTC — a ~5h window. That clustering
looked at first like a shared-cause break (a temporary sensacine.com
degradation hitting many venues' scheduled scrapes at once) — but a real
site-wide outage would show up as FAILURES (the client throws when the venue
page 200s without the `data-showtimes-dates` attribute, or the fetch itself
errors), not as clean `zero` buckets with `successes=0, failures=0, zeroes=1`.
Every one of the 6 venues spot-checked live (Cine Cortés — the false-positive
detailed above — plus Zornotza Aretoa, Cine AMGu, Casa de Cultura Doctor
Velasco, Teatro Capitol, Cine Princesa, all confirmed `data-showtimes-dates=[]`
or a far-future-only list) is either genuinely empty right now or the same
publication-lag shape as Cine Cortés. The clustering is best read as many
small Spanish venues publishing their weekly programme on the same weekday
(several fell dark Sunday night/Monday and are picking back up midweek or the
following weekend) rather than one shared infrastructure failure — consistent
with all 13 using the one shared `WebediaShowtimesClient`/ES-market wiring but
each venue's OWN day-list, not a common failure path. Expect most of the 13 to
self-heal on their next scrape (ES cadence 420 min, several already overdue as
of this run). No code change.

Remaining ~205 non-transitioned white ES venues (routine triage, not
individually probed): 24 seasonal-named (11.7% of the white list; historical
share was higher, 27/78% on 09-12 — worth noting the seasonal COUNT in the
white list dropped even as ES's overall white % rose, consistent with the
rise being driven by the fresh transitions, not seasonal closures); of the
181 non-seasonal remainder, 147 have no archive ever, 6 have an archive >10
days old, 28 have an archive ≤10 days old (not spot-checked beyond the 6
covered by the transition cluster above).

---

## 2026-09-12

**Sixth all-five-country sweep.** No verbatim mongosh script was ever
committed here (the 09-08 entry noted this) — wrote the sweep fresh from the
`classify()` predicate this time (`db.getSiblingDB` × 5, replicate
`failures>0 ? (successes+zeroes>0 ? yellow : red) : successes>0 ? green :
zero`, `.takeRight(3)` on non-empty buckets, skip `*|enrichment` / `img:`).
Newest bucket ~2026-09-12 12:45 UTC.

| DB | services | white | white % | red | green→white |
|---|---|---|---|---|---|
| `kinowo` (PL) | 310 | **5** | 1.6% | 3 | 2 |
| `kinowo_uk` | 852 | **60** | 7.0% | 0 | 4 |
| `kinowo_de` | 1,537 | **390** | 25.4% | 21 | 1 |
| `kinowo_us` | 5,040 | **715** | 14.2% | 2 | 10 |
| `kinowo_es` | 601 | **198** | 32.9% | 1 | 2 |

vs. 2026-09-08: PL 7/310 (2.3%), UK 70/853 (8.2%), DE 395/1538 (25.7%), US
1143/5030 (22.7%), ES 215/602 (35.7%). PL, UK and US all improved; DE/ES flat.
**US's drop (22.7%→14.2%) confirms 09-08's read that the Labor Day spike was
temporary**, not a new baseline: seasonal-named US venues are back down to
34% white (was 81% on 09-08, 33% on 09-04) — a one-off holiday-week dip in a
weekly sawtooth (see `reference_showtimes_weekly_sawtooth`), not a trend.
DE/ES seasonal shares stayed high (DE 95%, ES 78%), consistent with both
runs before this one.

### Poland — all 5 hand-probed; 1 fixed, 1 regression from a fix 4 days ago, 3 confirmed still dormant

**Kino Praha — fixed AGAIN, @205d32428.** The weekday-label fix from 09-08
(`cbe5cdfb7`) held for exactly one page-refresh cycle: mteatr.pl's archive
shows Kino Praha scraping fine on 2026-09-09 13:37 (10 films), then going
white again sometime before this run started. The site had changed its date
stamp a second time — not by adding to it this time, but by **dropping the
4-digit year entirely**: "12 Wrz (Sb) / 16:10" instead of "09 Wrz 2026 (Śr) /
16:00". `PrahaClient.StampPat` still required the year, so it stopped
matching every stamp again: 0 films, 0 showtimes, four days after the last
fix.

Fix: the year is now optional in `StampPat`; when absent, `parseStamp` infers
it from `today` via the existing `ScraperParse.upcomingDate` (60-day grace) —
the same helper other yearless-day-header scrapers already use. `PrahaClient`
now takes a `today: LocalDate` parameter (defaulting to `LocalDate.now` in
Warsaw time, like `KinoPortClient` and friends), threaded through from
`CinemaScraperCatalog`.

**Test:** `PrahaNoYearSpec` replays a 2026-09-12 capture of the live (broken,
yearless) page — the pre-fix `PrahaClient` doesn't even compile against a
`today` parameter (the class had none), which is as clean a "fails before" as
a signature change gets; after the fix it reads "12 Wrz (Sb) / 16:10" as
2026-09-12 16:10. `PrahaWeekdayLabelSpec` (09-08's fixture, year + weekday
label) and the original `PrahaClientSpec` (June fixture, plain year) both
still pass unchanged — three date-stamp shapes now share one pattern.

**Layers:** `sbt worker/Test/test` green (3,462 tests) after
`./infra/bin/fetch-gitops` (the `deploy.*` failures on the first run were the
same missing-local-checkout artifact every run since 2026-09-04 hits, not
this change). `FilmScheduleEndToEndSpec` and `PageSnapshotSpec` both green
**without any snapshot regeneration** — the e2e fixture corpus under
`08-06-2026/` still replays a frozen capture from before either of Kino
Praha's two format drifts, so this fix's new code path isn't exercised by it,
and the old year-present path both specs cover is unchanged.

**Not pushed to main yet this run** — committed locally on this worktree's
branch (`auto/white-cinema-20260912-145507`); the standing end-of-run flow
(rebase → green → merge → push) runs after the rest of this sweep's triage,
so a second fix landing later in the same run doesn't need two round-trips.

**The other 4 white PL venues — all re-probed live, all still confirmed
dormant, no change from 09-08:**

| Venue | Source | What the source says |
|---|---|---|
| Kino Lewart (Lubartów) | bilety24 (redirected to `…/lubartowski-osrodek-kultury-1382`) | 0 `Film:` |
| Kino Wisła Brzeszcze | bilety24 `…/osrodek-kultury-w-brzeszczach-1539` | 0 `Film:` |
| Kino Chatka Żaka (Lublin) | `umcs.pl/pl/kalendarz-wydarzen,9469,1.lhtm` | 0 `div.box-row` |
| DKF Politechnika | Filmweb 1645, `seances?date=` for today and tomorrow | `[]` on both — **9th consecutive run white**. The 08-24 checkpoint ("if still `[]` in October, escalate") has not yet triggered — today is 2026-09-12, not October — but it is close; the next run (~09-15/16) should watch for the rollover and escalate if it crosses without recovery. |

**Two prior `needs-human` items have resolved themselves and are no longer
white:** Kino GOK Tychowo and Kino Powiśle/Sztum (both flagged needs-human in
earlier runs, both noted as "later recovered" in passing) are confirmed off
the white list this run — no code change was involved, don't reopen either
unless they reappear.

**Green→white transitions (2): both self-healed same day, no action** —
Dolnośląskie Centrum Filmowe (Wrocław) and Kino Głębocka 66 (Warszawa) each
had a short mid-afternoon 09-11 zero run (1-5 buckets) bracketed by green on
both sides; both are green again as of this run's newest bucket. Same shape
as Electric Palace Harwich (09-08) — a same-day blip, not a break.

**Red (3, unchanged in shape, out of brief):** Kino MOK Nowa Ruda, Kino
Przedwiośnie, Nowe Kino Warszawa (the last already red on 09-08) — not
white, not investigated this run.

### UK — 60 white (down from 70), 4 green→white transitions, all genuinely empty upstream

All four transitions — Arts Centre Stamford, Chiddingfold Village Hall
Cinema, Regent Blackpool, The Fullarton Castle Douglas — are small
community/arts-centre screens that each went green→zero within the same
90-minute window on 2026-09-11 evening (~20:15-20:45 UTC) and stayed zero
into this run. Fetched all four live via `flicks.co.uk/cinema/<slug>/`
directly (not through the residential proxy — a developer IP isn't
Cloudflare-403'd the way the worker's Fly egress is): all four render
Flicks' own `no-streaming-sessions` block with no `data-date` tabs at all,
i.e. genuinely empty upstream right now, not a parser gap. Plausibly a
shared upstream publish/rollover moment for several small venues at once
(the ~30-minute-wide clustering is suggestive) rather than four unrelated
coincidences, but nothing on OUR side to fix either way. Not investigated
further; UK's overall white share improved (8.2%→7.0%).

### DE — 390 white, flat, 1 green→white transition (seasonal, no action)

The one transition, "Open Air Kino, Mond & Sterne", is exactly the seasonal
pattern documented since 09-04 (`open.?air` in the name) — an outdoor screen
closing for the season, not a break. DE's seasonal white share is 95% (169
seasonal-named venues, 161 white), consistent with every prior run. Not
individually probed; nothing else stood out against the 09-08 baseline.

### US — 715 white (down from 1,143), 10 green→white transitions, 9 confirmed genuinely empty upstream

The Labor-Day seasonal spike 09-08 flagged as "confirm this isn't the new
baseline" was NOT the new baseline: seasonal-named US white share is back to
34% (was 81% on 09-08, 33% on 09-04) — a one-week dip, self-corrected.

Of the 10 green→white transitions in the full 24h window, one (Fox Theatre
Hutchinson) had already recovered by the time of this sweep (its last bucket
is green) — the sharpest possible confirmation that the low-cadence signal
here is usable when caught early. The other 9 — Center Smith Center, Garde
Arts Center New London, Historic Temple Theatre of Viroqua, Imperial
Theatre, Lund Theatre Viborg, Mountain Park Cinema, Redford Theatre Detroit,
Reynolds Drive-In, Senate Theater Detroit — were each fetched live via
`flicks.us/cinema/<slug>/`: **all 9 render the `no-streaming-sessions`
block**, genuinely empty upstream right now. All nine are small
single-screen historic theatres or a drive-in (Reynolds), the kind of venue
that plausibly runs a weekly or gappy schedule rather than a daily one — not
parser bugs. No code changes.

### ES — 198 white (down from 215), 2 green→white transitions

**Multicines Cáceres** already self-healed (its last 2 buckets are green,
so it isn't in this run's actual white list despite the adjacent-bucket
green→zero blip it shows mid-window). **Cine Capri** (theaterId E0230) is
still white as of this run's last bucket, but its live venue page right now
carries `data-showtimes-dates="[&quot;2026-09-12&quot;]"` — today's date IS
published — and the direct showtimes JSON for that date returns real films.
Cine Capri's archive shows its last content-bearing scrape at 2026-09-11
~22:36 UTC, right where its bucket history goes green→zero — so the
programme was posted sometime between our last two scrapes (ES cadence 420
min): publication lag, same shape as Electric Palace Harwich / Film-Eck, not
a parser break. Expect it to self-heal on its next scrape; not fixed, no
code change. ES's seasonal share (78%, 27 venues) is flat vs. 09-08 (81%).

---

## 2026-09-08

**Fifth all-five-country sweep.** This run's first sweep script draft required
exactly 3 non-empty buckets to call a service white — caught and fixed before
any numbers were recorded, because it silently UNDER-counted every country
whose cadence can't fill 3 buckets in the 24h retention window (i.e.
everywhere but PL): the draft script found 0 white in the US on the same data
where the corrected script (matching the real `classify()`'s `.takeRight(3)`,
no minimum) found 1,143. Flagging this here in case a past run's ad hoc
tooling made the same mistake — it wouldn't be visible in this log, only in
an implausibly-low white count for DE/US that didn't get questioned. See the
new "cadence vs. retention window" methodology note above. Newest bucket
2026-09-08 17:45 UTC.

| DB | services | white | white % | red | green→white |
|---|---|---|---|---|---|
| `kinowo` (PL) | 310 | **7** | 2.3% | 1 (Nowe Kino Warszawa) | 0 |
| `kinowo_uk` | 853 | **70** | 8.2% | 0 | 1 |
| `kinowo_de` | 1,538 | **395** | 25.7% | 21 | 0 |
| `kinowo_us` | 5,030 | **1,143** | 22.7% | 2 | 0 |
| `kinowo_es` | 602 | **215** | 35.7% | 1 | 5 |

vs. 2026-09-04: PL 8/325 (2.5%), UK 57/854 (6.7%), DE 386/1538 (25%),
US 779/5038 (15.5%), ES 198/602 (32.9%). DE and ES are flat. **UK ticked up
1.5pt and US jumped 7pt** — both explained below, neither systemic.

### Poland — all 7 hand-probed, 2 fixed, 5 confirmed still dormant

**KinoPort (Gdańsk) — fixed, @cbe5cdfb7.** gcsw.pl (the venue's WordPress site)
relaid its repertoire post: the month header moved `<h3>`→`<h4>`, and — the
part that actually broke parsing — the DAY headers ("03.09 (CZWARTEK)") moved
from `<h4>` into plain `<p>`. `KinoPortClient`'s tag-based dispatch
(`case "h4" => parse day` / `case _ => parse screening`) never saw a day
header again once it stopped living in `<h4>`, so `day` stayed `None` and
every screening paragraph was silently dropped: 0 films, 0 showtimes for the
whole September post. Two smaller format drifts rode along in the same post:
some titles now split across three sibling `<strong>` tags ("17:00" / "– " /
"Tony") instead of one, and captions dropped their `<em>` wrapper and "reż."
prefix ("1962, Orson Welles" instead of "1962, reż. Orson Welles").

Fix: day-header detection is now TEXT-SHAPE-based (`DayPat` matched against
any `h3`/`h4`/`p`'s text) rather than tag-based, so it doesn't care which tag
the site puts it in. This incidentally fixes a latent bug the old dispatch
had — a non-day `<h4>` (the month header, under the new layout) used to
unconditionally reset `day` to `None` just by being an `<h4>` that didn't
match `DayPat`, harmless only because month headers used to always open a
post before any screening. Screening parsing now joins ALL `<strong>` tags in
a paragraph instead of taking the first, and captions fall back to the plain
text after the first `<br>` when there's no `<em>`, with a director fallback
for a bare "YYYY, Name" credit.

**Test:** `KinoPortMarkupChangeSpec` replays a 2026-09-08 capture of the live
(broken) post — fails before (`List() was empty`), passes after. The original
`KinoPortClientSpec` (2026-07-31 capture, old markup) still passes unchanged —
the fix is backward-compatible.

**Kino Praha (Warszawa) — fixed, @cbe5cdfb7.** mteatr.pl started inserting a
parenthesised weekday abbreviation between the year and the slash in its date
stamp — "09 Wrz 2026 **(Śr)** / 16:00" instead of "09 Wrz 2026 / 16:00" — and
`PrahaClient.StampPat` required the slash immediately after the 4-digit year,
so every stamp on the page failed to match: 0 films, 0 showtimes. Fix: an
optional non-capturing `(?:\s*\([^)]*\))?` between the year and the slash.
`PrahaWeekdayLabelSpec` replays a 2026-09-08 capture; fails before, passes
after; the original `PrahaClientSpec` (June capture, no weekday label) still
passes.

**Layers:** `sbt testUnit` green after `./infra/bin/fetch-gitops` (13 + 1,371
+ 3,461 + 973 + 60 = 5,878 tests; the first run's 40 `deploy.*` failures were
the same missing-local-checkout artifact 2026-09-04 hit, not this change).
`FilmScheduleEndToEndSpec` and `PageSnapshotSpec` both green **without any
snapshot regeneration** — the e2e fixture corpus under `08-06-2026/` replays a
frozen capture of both venues from BEFORE either site's markup changed, so
neither fix's new code path is exercised by it; the old-markup path both specs
also cover is unchanged.

**Neither KinoPort nor Kino Praha is in the DiacriticMonthNameSpec /
`ScraperParse.DayMonthPat` family from 2026-09-04** — both are bespoke
one-off parsers (a WP-REST post walker, a `div.post`/`div.label` reader), so
this is two unrelated site-relayout bugs, not a shared regex trap.

**The other 5 white PL venues — all re-probed live, all still confirmed
dormant, no change from 2026-08-24/09-04:**

| Venue | Source | What the source says |
|---|---|---|
| Kino Lewart (Lubartów) | bilety24 `…/lubartowski-osrodek-kultury-1382` | 0 `Film:` |
| Kino Wisła Brzeszcze | bilety24 `…/osrodek-kultury-w-brzeszczach-1539` | 0 `Film:` |
| Patria (Zakopane) | `kinopatria.com/repertuar/` | 0 `h3.amy-movie-field-title` |
| Kino Chatka Żaka (Lublin) | `umcs.pl/pl/kalendarz-wydarzen,9469,1.lhtm` | 0 `div.box-row` |
| DKF Politechnika | Filmweb 1645 | `[]` — now the **8th** consecutive run white. The 08-24 entry's October checkpoint stands: if still `[]` in October, escalate to "has Filmweb quietly dropped the venue" rather than logging dormancy a 9th time. |

**Recovered since 2026-09-04 (2):** Kino KDK (Kutno) and Kino nad Wartą
(Konin) are no longer in the white list — both were "0 Film: of 73, all
Koncert/Spektakl" dormancy reads last run; nothing to fix, they just started
screening again.

### UK — 70 white, 1 green→white transition, triaged not fixed

**Electric Palace Harwich — investigated, no fix, self-heals.** The one
green→white transition in the whole run: green at 2026-09-07 17:45 UTC, then
zero at 00:45 / 07:45 / 14:45 UTC on 09-08, no errors recorded on any bucket
(so `fetch()` succeeded and returned nothing, not a network/parse failure).
Re-fetched live (as of 2026-09-08 ~19:50 UTC) through the REAL client path —
`programmeUrl` for the `timetable__day` tabs, then `sessionsUrl` with the
`is-ajax-call: yes` header the sessions fragment requires — and got real
content: the venue's own site currently advertises sessions on 09-11 through
09-14, and the sessions fragments for those days parse to 1-2
`cinema-times__article` blocks each. All three white buckets fall in the same
UTC calendar day, before those dates were apparently published; this is the
same publication-lag shape 2026-09-04 caught on Kinowerkstatt, not a client
bug. UK cadence (420 min) means it should self-heal on its own within a few
hours — check first on the next run before assuming otherwise.

The other 69 white UK venues were triaged, not hand-probed: 0 are
seasonal-named (`terraza|verano|open.?air|autokino|freibad|sommer|drive.?in`
matches nothing in the UK roster, as 2026-09-04 also found), 44 have never had
a `cinema_scrapes` archive entry (long-tail, presumptively just quiet), 8 have
an archive older than 10 days (long dormant), and 18 (including Electric
Palace Harwich) have a content-bearing archive ≤10 days old — per the cadence
note above, expect most of those 18 to be publication lag rather than bugs;
not spot-checked individually this run.

### DE / US / ES — triaged, sampled, not hand-probed exhaustively

**Seasonal closures explain most of the DE/US/ES volume, same as 2026-09-04,
and US's jump is almost entirely seasonal.** Cross-cutting the white set
against `terraza|verano|open.?air|autokino|freibad|sommer|drive.?in`:

| | seasonal-named venues | of them white | vs 09-04 |
|---|---|---|---|
| DE | 175 | **164 (94%)** | 158/175 (90%) |
| US | 300 | **244 (81%)** | 99/303 (33%) |
| ES | 27 | **22 (81%)** | 19/27 (70%) |
| UK | 0 | — | — |

US's seasonal share nearly TRIPLED (33%→81%) in the 4 days since 09-04, which
straddled US Labor Day (2026-09-07) — the traditional close date for drive-ins
and outdoor screens. Non-seasonal US white also rose (14.4%→19%, 680→899 of
~4,730), but the seasonal swing accounts for the bulk of the country's 15.5%→
22.7% jump. DE and ES's seasonal shares were already high on 09-04 and stayed
roughly flat.

**Nine-venue spot-check, live via the aggregators' own JSON/markup (not the
JS-rendered venue page — see the cadence note above for why the sessions/JSON
endpoint is the one that matters):**

| Venue | Country | Verdict |
|---|---|---|
| El cine Villablino, Cine Bahía, Cine AMGu, Cine Princesa, Sala d'actes Ajuntament (Balaguer) | ES — all 5 of this run's green→white transitions | `data-showtimes-dates="[]"` on all 5. Genuinely empty upstream right now, not a parser gap. |
| Kinowerkstatt (Saarbrücken) | DE | `data-showtimes-dates="[]"` today — unlike 2026-09-04, when the same venue had a real programme (that was the false-positive precedent). Genuinely empty this time. |
| Film-Eck (Wermelskirchen) | DE | `data-showtimes-dates=["2026-09-11",…,"2026-09-14"]` — real future programme, our last 2 scrapes (both zero, no errors) simply predate its publication. Publication lag, matches Electric Palace Harwich's shape; expect self-heal. |
| Curt's Theater, Town Hall Theatre Quincy | US | Both show flicks.us's own "Sorry, we haven't received movie times for this cinema yet" banner. Genuinely empty upstream. |

Net: of these 9 spot-checked venues (the 5 ES green→white transitions plus 4
sampled non-seasonal "recently broke" candidates in DE/US), 8 were genuinely
empty upstream and 1 (Film-Eck) was publication lag; none were parser bugs.
No code changes for DE/US/ES this run. **Red columns, unchanged in shape from
09-04** (DE: `filmstarts.de` 404s; US: `flicks.us` 404s; ES: 1 `HTTP 410`) —
out of brief, no action taken.

---

## 2026-09-04

**First all-five-country sweep.** Poland-only ended when the other workers came
back on 2026-08-29, and the run brief was widened to every country on this date.
Newest bucket 2026-09-04 16:45 UTC, retained window ~24 h in all five.

| DB | services | white | white % | red |
|---|---|---|---|---|
| `kinowo` (PL) | 325 | **8** | 2.5% | 1 (an `img:` poster row, not a cinema) |
| `kinowo_uk` | 854 | **57** | 6.7% | 1 |
| `kinowo_de` | 1,538 | **386** | 25% | 21 |
| `kinowo_us` | 5,038 | **779** | 15% | 2 |
| `kinowo_es` | 602 | **198** | 33% | 1 |

Those are the baseline numbers the next run should diff against. **Green→white
transitions inside the whole retained window: 3, all in Spain, all genuine.** No
country shows a systemic parser break.

**One fix landed — and it took out four cinemas, only one of which was white.**
See below. Access route also changed: `/uptime` is auth-gated at every address
and `flyctl proxy` is gone, so the methodology section above was rewritten for
the ssh-forward-to-`mongo-1` route and the five-database sweep.

### The fix: `\w` doesn't match "września" — `ScraperParse` now owns the pattern — @ae0b7e57b

**OKF Iluzja (Częstochowa) went white on 1 September** and was the only venue
this showed up on. Its weekly page was fine — 7 day blocks, 24 showtimes, 72 film
links, all server-rendered. What broke was one character class:

```scala
private val DatePat = """(\d{1,2})\s+(\w+)\s+(\d{4})""".r   // "4 września 2026"
```

Java's `\w` is ASCII-only unless the pattern is compiled with
`UNICODE_CHARACTER_CLASS`, so it stops dead at the first diacritic. Verified
directly:

```
"4 września 2026"    -> NO MATCH        "4 sierpnia 2026" -> MATCH
"4 października 2026"-> NO MATCH        "4 grudnia 2026"  -> MATCH
```

Exactly **two** of the twelve genitive month names carry a diacritic — `września`
and `października`. So the bug reads every date for ten months of the year and
none at all in September and October, then heals itself on 1 November. The
archive agrees to the day: Iluzja's last content-bearing scrape was
2026-08-31 23:03.

**The important half: three more cinemas had the same regex and NONE of them was
white.** Grepping the month-token patterns turned up four `\w` spellings among
~20 clients (every other client already used `\p{L}`), and the other three were
hiding in the two ways now written into the methodology above:

| Venue | Bar | What was actually happening |
|---|---|---|
| **OKF Iluzja** | white 24/24 | nothing to hide behind — the visible symptom |
| **Kino Agrafka** (Kraków) | green 24/24 | `fallback: true` — Filmweb covering for a dead own-site parse |
| **Kino Bułgarska 19** (Poznań) | green 24/24 | `fallback: true` — same |
| **Kino Pod Baranami** (Kraków) | green 24/24, `fallback: false` | **partial parse.** Its archive held 5 films, all in November/December — "4 listopada", "16 grudnia", ASCII month names that still matched. The whole current week was being dropped. The replayed capture goes from 5 showtimes to ~200. |

Pod Baranami is the one worth remembering: a venue can lose 97% of its programme
and still report success on every bucket, because "success" only asks whether the
parse returned *something*.

**Fix.** The two shapes move onto `ScraperParse.DayMonthPat` /
`DayMonthYearPat`, spelled `\p{L}`, with the trap written into the doc comment;
the four broken clients and the seven that already spelled it `\p{L}`
(`KinoSokolBrzozow`, `SystemBiletowy`, `KinoZbyszek`, `ArtKinoKrosno`,
`KinoCentrumCsw`, `KinoAmok`, `Ekobilet`) all share them now, so there is one
copy left to drift instead of eleven.

**Test (fail-before / pass-after):** `clients.cinemas.DiacriticMonthNameSpec`
replays each of the four venues' real repertoire page captured 2026-09-04.
Before: Iluzja / Agrafka / Bułgarska return `List()`, Pod Baranami returns only
its five Nov-Dec showtimes. After: all four parse the 4 September programme.
`ScraperParseSpec` covers both diacritic months on the shared patterns directly.

**Layers:** `sbt testUnit` green (13 + 3,135 + 1,150 + 821). The 37 `deploy.*` /
`ScrapeCadenceSustainabilitySpec` failures on the first run were a missing local
checkout, not this change — the GitOps manifests moved to `pawelkrupinski/movies-gitops`
and `./infra/bin/fetch-gitops` restores them; all 184 pass after that, so **run
`./infra/bin/fetch-gitops` once in a fresh worktree** before reading a `testUnit`
result. `FilmScheduleEndToEndSpec` green after regenerating `expected-schedules.txt`
(+2 blocks: Pod Baranami's Wajda retrospective titles "Bez znieczulenia (Rough
Treatment)" and "Krajobraz po bitwie (Landscape After the Battle)" — no film
disappeared) and `read-model-snapshot.json` (+184 lines). `PageSnapshotSpec`
green **without** regeneration — the recovered screenings are in Kraków and the
snapshot cities are Poznań / Wrocław / Warszawa.

**CONFIRMED IN PROD.** All 47 CI jobs green on `1632a0bb7`; Flux rolled
`movies-worker:main-20260904171602-8af99c5` at 17:22:48 UTC. Every one of the
four venues flipped on its first post-deploy scrape, and each of the three
failure shapes is visible in a different column:

| Venue | first post-deploy bucket | archive before → after |
|---|---|---|
| **OKF Iluzja** | 18:00 `s=1 z=0 f=0` — **white→green** | 08-31, 3 films/3 showtimes → **7 films / 24 showtimes** |
| **Kino Agrafka** | 18:15 **`fallback: true` → `false`** | 4 films/23 (Filmweb) → **5 films / 24** (own site) |
| **Kino Bułgarska 19** | 18:15 **`fallback: true` → `false`** | 18 films/58 (Filmweb) → **18 films / 58** (own site) |
| **Kino Pod Baranami** | 17:30 `s=1`, green before AND after | 5 films/**5 showtimes** → 62 films / **199 showtimes** |

Iluzja's 24 showtimes are exactly the 24 `span.time-f` on the live page, and Pod
Baranami's 199 match the replayed capture's ~200 — the fix restores the whole
programme, not a fragment of it. Bułgarska is the useful null result: its film
count is unchanged because Filmweb happened to carry the same 18 films, so the
ONLY signal that anything was ever wrong there was the `fallback` flag. Nothing
in the film counts would have told you.

### Poland — the other 7 white venues, all re-probed live, all dormant

| Venue | Source | What the source says |
|---|---|---|
| Kino KDK (Kutno) | `Bilety24OrganizerClient` `…/kino-kutnowskiego-domu-kultury-1474` | **0** `Film:` of 73 entries (51 Koncert, 9 Spektakl, 13 Wydarzenie) |
| Kino Lewart (Lubartów) | `…/kino-lewart-w-lubartowie-1382` | literally **"Brak wydarzeń"** — see the rename note below |
| Kino nad Wartą (Konin) | `…/koninskie-centrum-kultury-1626` | **0** `Film:` of 73 (62 Koncert, 11 Spektakl) |
| Kino Wisła Brzeszcze | `…/osrodek-kultury-w-brzeszczach-1539` | **0** `Film:` of 45 (all Koncert) |
| Patria (Zakopane) | `kinopatria.com/repertuar/` | 0 `h3.amy-movie-field-title`; renders **"Brak filmu"** |
| Kino Chatka Żaka (Lublin) | `umcs.pl/pl/kalendarz-wydarzen,9469,1.lhtm` | 0 `div.box-row`; renders **"Brak wydarzeń"** |
| DKF Politechnika | Filmweb 1645 | literal `[]` on every date sampled, into October |

**Kino Lewart — the organizer was RENAMED, and the redirect still works.** Our
URL `…/kino-lewart-w-lubartowie-1382` now 302s to
`…/lubartowski-osrodek-kultury-1382` — same numeric id, so the scraper follows it
cleanly and needs no change. The destination page carries no events of any kind.
Recording it so a future run doesn't read the rename as the cause.

**DKF Politechnika is now white for the seventh consecutive run**, and this run
finally has a control for it: **Kino Zachęta recovered** (Filmweb 2405 returns
real 4 and 6 September showtimes), which proves the Filmweb path and
`FilmwebShowtimesClient` are working and isolates 1645 as genuinely empty
upstream. The 08-24 entry set an October checkpoint for exactly this — it still
stands: if 1645 is still `[]` in October, ask whether Filmweb has quietly dropped
the venue rather than logging dormancy an eighth time.

**Recovered since 2026-08-24 (5):** Piast, Kino MDK, Kino PDK, Kino Kuźnica,
Kino Zachęta — every one of them a venue that autumn simply started for. Nothing
was ever wrong with their scrapers, exactly as those entries predicted.

### UK / DE / US / ES — triaged, not hand-probed

No green→white transition anywhere in these four, so nothing broke inside the
window. Two structural findings explain the bulk of the volume:

**Summer is over, and it shows.** Cross-cutting the white set against
open-air-sounding venue names (`terraza|verano|open.?air|autokino|freibad|sommer|drive.?in|freilicht`):

| | seasonal-named venues | of them white | vs country baseline |
|---|---|---|---|
| DE | 175 | **158 (90%)** | 25% |
| ES | 27 | **19 (70%)** | 33% |
| US | 303 | 99 (33%) | 15% |
| UK | 0 | — | 6.7% |

A German Autokino or a Spanish *terraza de verano* closing on 31 August is not a
scraper fault, and at 90% / 70% white these venues are a large, entirely expected
share of the two worst-looking countries. UK has no such venues at all — and the
lowest white rate of the four.

**A six-venue spot-check confirmed the aggregators, not our parsers, are empty.**
Herzog-Filmtheater and Alabama-Kino (DE), Hebden Bridge Picturehouse and Electric
Picture Palace Southwold (UK), Michigan Theatre of Jackson (US): every one
answers HTTP 200 with the aggregator's OWN empty-programme signal —
`no.showtime.error` / `nextDate:null` / `data-showtimes-dates="[]"` for Webedia,
the `no-streaming-sessions` "we haven't received movie times for this cinema yet"
block for Flicks. No blocks, no redirects, no markup drift.

**The sixth, Kinowerkstatt (Saarbrücken), was the interesting one — and it is a
TIMING artifact, not a bug.** filmstarts advertised 4 days and a real 19:00
screening while our last bucket (15:00 UTC) said zero. Replaying the live
captures through `WebediaShowtimesClient` settled it: the client plans all four
chunks and returns **3 films / 6 showtimes**. The programme was published
upstream after our scrape and the venue will go green on its next tick. Worth
recording as a method: when a DE/ES venue looks wrong, replay its captured venue
page + per-day JSON through the real client before suspecting it — that
distinguishes a parser bug from a publication lag in one run.

**Red columns, for completeness** (out of brief, no action taken): DE's 21 reds
are all `HTTP 404` on `filmstarts.de/kinoprogramm/kino/<id>/` and ES's 1 is an
`HTTP 410` — the gone-upstream shape already quarantined in memory. UK's single
red is Odeon Basingstoke ("All 2 backends failed" on the Vista `ocapi`), US's two
are flicks.us 404s.

---

## 2026-08-24

**PL: 10 white, 4 red**, out of 324 non-enrichment services, newest bucket
2026-08-24 08:45 UTC. `kinowo_de` and `kinowo_uk` hold **zero** `uptimeBuckets`
for the seventh run running (workers stopped since 2026-08-02, ~24 h retention) —
Poland only, again.

**One fix landed, and it came out of the RED column: Kino Studio (Opole) —
`fixed` @0ded8f0f4.** MDK Opole rebuilt its site onto a new CMS; both `.html`
slugs now hard-404 and the cinema's whole autumn season was sitting unread at a
new slug. Every one of the 10 white venues is genuinely film-dormant, all
re-probed live this run.

**Set changes vs 2026-08-21 (13 white / 2 red then):**
- **NEW white (0).** Again not a single green→white transition — no parser broke
  in the window.
- **RECOVERED (2):** **ADA Kino Studyjne** and **Kino CK Lublin**, both 24/24
  green. Their seasons started; nothing was ever wrong with either scraper.
- **LEFT the white set by going RED (1):** **Studio (Opole)** — the 08-21 entry
  predicted this venue would move by today and told the next run to check the
  slug. It moved harder than predicted (the whole site was rebuilt) and it is
  the run's one code fix. See below.
- **NEW red (2):** **Kino GOK** (503, `needs-human`) and the `img:
  www.multikino.pl` poster row (not a cinema). **Kino Warszawa** stays red on
  its expired certificate — unchanged and still `unfixable`, town-hall side.

### Kino Studio (Opole) — `fixed` @0ded8f0f4 — the site was rebuilt, not the cinema closed

The 08-21 entry left this as the one open action item: "the next run falls after
3 September — check whether the repertoire comes back on `kino-studio.html` or
whether they keep a new slug". Both slugs are now a **hard 404**:

```
curl -o /dev/null -w '%{http_code}' https://mdk.opole.pl/kino-studio.html          → 404
curl -o /dev/null -w '%{http_code}' https://mdk.opole.pl/kino-studio-przerwa.html  → 404
```

which is why the venue went from white to **RED, 24/24 buckets** — the two-slug
walk threw instead of parsing an empty page. `mdk.opole.pl` is now Drupal 11
("Design System"), extensionless URLs throughout, and the homepage links the
cinema at **`/kino-studio`**. That page is not dormant at all: it carries the
**whole autumn season — 14 films, weekly Thursday double-bills 18:00 + 20:30,
3 September through 3 December** (Drugie życie, O czym sobie nie mówimy, Takie
jest życie, Wędrówka na północ, … 500 mil). 28 showtimes were being dropped.

**What actually broke, beyond the URL** — the rebuild moved three things the
parser keys on, and exposed a fourth bug:

| | pre-rebuild | rebuilt |
|---|---|---|
| title | `<h1>` | `<h2><strong>` |
| showtime | `18.00 i 20.30` | `18:00 i 20:30` |
| poster | `<p><img></p>` | bare `<img>` |
| films per page | 1 | 14 |

The fourth is the interesting one. The state machine flushed a film when it hit
the **title**, but each block's date header *precedes* its title — harmless
while the page only ever held one film, fatal at 14: film #1 collected every
date on the page and every later film ended up with none. Films now flush at the
next block's START (poster, date header, or the `<hr>` between films).

**The dead-slug guard needed tightening too, and this is the trap worth
remembering:** the rebuilt site's "Strona nie znaleziona" body **carries
`ckeditor` elements of its own** (`title-section ckeditor`, a modal pane), so
the old `div.ckeditor` discriminator would have accepted a 404 body as content —
turning a doubly-dead source back into a white bar, exactly the failure the
guard was added in July to prevent. Only an editable content block carries
`ckeditor clearfix`, verified against all three page kinds (live repertoire,
break notice, 404), so that is the selector now.

While in there, the `produkcja:` / `czas trwania:` lines the page has always
carried are now harvested as **release year + countries + runtime**. Per the
hint-harvest memory the cinema-side year is the dominant lever on TMDB match
rate, and this programme is exactly the corpus that needs it — "Obcy",
"Ojczyzna", "Rozważna i romantyczna", "Róża" all resolve to the wrong film on a
yearless search.

**Test (fail-before / pass-after):** `KinoStudioClientSpec`, rewritten against a
freshly recorded `kino-studio` capture — 11 of its 14 cases fail on the old
parser (0 films returned; the dead-page case does not even throw, proving the
`ckeditor` leak) and all 14 pass after. The pre-rebuild capture is kept as
`kino-studio-opole-legacy-markup` with a test of its own, because the recorded
scrape corpus still carries that markup and the parser stays tolerant of both
spellings.

**CONFIRMED IN PROD.** All 47 CI jobs green and all six deploys rolled,
`kinowo-worker` among them (run 32711979480). Studio scrapes at :30 and had been
`failures=1` on every bucket through **09:30 UTC**; the **10:30 UTC bucket — the
first scrape after the deploy — is `successes=1`**. Downstream it landed whole:
`cinema_scrapes` records the venue scraped 10:34:26 UTC with
`listingComplete: true`, and **14 slots are in `web_screenings`** — the read
model the site actually serves — each with its two showtimes
(`drugiezycie|2025` on 03.09, `gorzkieswieta|2026` on 05.11, `500mil|2026` on
03.12), `filmUrl` following the new slug. The film ids carry the harvested
`produkcja:` year, so that half of the change is live too.

**Layers:** `sbt testUnitNoE2e` green (7 + 979 + 2988 + 553),
`FilmScheduleEndToEndSpec` green, `PageSnapshotSpec` green.
`read-model-snapshot.json` moved by exactly one line — the corpus film's
`filmUrl` following the slug — and `expected-schedules.txt` and all four
`expected-*.html` did not move at all (the corpus capture's single film is a
25.06 screening, already past the corpus date, and Opole is not a snapshot city).

### The 10 white venues — all re-probed live, all genuinely empty

| Venue | Client / URL | What the source shows |
|---|---|---|
| Kino nad Wartą | `Bilety24OrganizerClient` `…/koninskie-centrum-kultury-1626` | **0** `Film:` of 50 entries (39 Koncert, 7 Spektakl, 4 Wydarzenie) |
| Piast | `…/ostrzeszowskie-centrum-kultury-601` | **0** `Film:` of 52 (37 Koncert, 7 Spektakl, 8 Wydarzenie) |
| Kino Wisła Brzeszcze | `…/osrodek-kultury-w-brzeszczach-1539` | **0** `Film:` of 36 (29 Koncert, 3 Spektakl, 4 Wydarzenie) |
| Kino MDK | `…/miejski-dom-kultury-w-radomsku-1546` | **0** `Film:` of 42 (29 Koncert, 9 Spektakl, 4 Wydarzenie) |
| Kino PDK | `BiletynaClient` `biletyna.pl/Pyrzyce/Pyrzycki-Dom-Kultury` | **0** `ScreeningEvent` (1 `TheaterEvent`) — see the probe note below |
| Kino Kuźnica | `SystemBiletowyClient` `shd.systembiletowy.pl` | `#repertoire-no-events` showing; 185 calendar days ALL "brak terminów" |
| Patria | `kinopatria.com/repertuar/` | 0 `h3.amy-movie-field-title`; site renders "Brak filmu" |
| Kino Chatka Żaka | `umcs.pl/pl/kalendarz-wydarzen,9469,1.lhtm` | 0 `div.box-row`; "Brak wydarzeń" |
| Kino Zachęta | Filmweb 2405 | literal `[]` (2 bytes) for every date sampled |
| DKF Politechnika | Filmweb 1645 | literal `[]` for every date sampled |

All 10 `intentionally-dormant`, re-confirmed live. Three notes:

- **Kino PDK needed the production fetch chain, and a bare curl lies about it.**
  A plain probe returns 403 with `cf-mitigated: challenge` — biletyna.pl's
  blanket Cloudflare block, which per the standing memory hits residential IPs
  too. Re-probed through the SAME chain the scraper uses (Decodo proxy primary →
  Zyte → direct) it answers **61,078 bytes, no challenge, 0 `ScreeningEvent`,
  1 `TheaterEvent`** and `BiletynaClient` itself returns 0 films. Dormant, not
  blocked. A future run that reports PDK "empty" from a bare curl has proved
  nothing.
- **Kino Kuźnica's parser is NOT stale** — worth recording because the first
  read of the page suggests it is. The live page renders a
  `div.calendar-day[data-date]` grid and none of the three shapes
  `SystemBiletowyClient` parses. But `kgl.systembiletowy.pl` (Kino Kawiarnia, a
  populated instance of the same platform) renders `calendar-day` **204 times
  as well** — the identical date-picker widget — *alongside* 229 `event-item` /
  122 `event-title`. The calendar is the chrome every instance carries; the
  parser's `div.event-item[data-date]` shape is current. Kuźnica just has no
  screenings, through January 2027.
- **Kino Zachęta and DKF Politechnika** have now been white for six consecutive
  runs on empty Filmweb responses. Both are DKF-style venues whose seasons start
  in autumn; if either is still `[]` in October, it is worth asking whether
  Filmweb has quietly dropped them (the pattern that moved Chemik onto an
  own-site scraper) rather than continuing to log them as dormant.

### Kino GOK (Tychowo) — `needs-human: upstream 503 to our egress only`

Red, not white, so outside the brief, but it is a PL venue completely dark and
the diagnosis does not close. `bilety.goktychowo.pl` has answered **HTTP 503 on
every one of the last 24 buckets**, yet the identical MSI request from a
residential IP returns **200 / 49,606 bytes**:

```
curl 'https://bilety.goktychowo.pl/MSI/mvc/pl?sort=Name&date=2026-08-24'  → 200, 49606 bytes
worker (24/24 buckets)                                                    → HTTP 503
```

That asymmetry has exactly the shape of the flicks.co.uk / Multikino egress
blocks, whose remedy is the residential-proxy route (`proxyPrimary`) — but the
evidence does not yet distinguish an IP block from a host that is simply
rate-limiting or intermittently 503ing a datacenter ASN, and no other MSI venue
is affected. Routing a host through the proxy on a guess is the per-host
special-casing this repo avoids, so it is logged rather than fixed. The cheap
next step for whoever picks it up: run the MSI request from the worker machine
itself and compare, before touching wiring.

## 2026-08-21

**PL: 13 white, 2 red**, out of 310 non-enrichment services, newest bucket
2026-08-20 21:15 UTC. `kinowo_de` and `kinowo_uk` hold **zero** `uptimeBuckets`
for the sixth run running (workers stopped since 2026-08-02, ~24 h retention) —
Poland only, again.

**No white venue is a parser bug this run.** All 13 were re-probed live at the
URL their scraper uses and every source is genuinely empty. The one code change
that landed came out of the RED column instead: **Kino NCKF EC1 — `fixed`
@931a5aa09**, a TLS trust break, not a parse.

**Set changes vs 2026-08-15 (20 white / 0 red then):**
- **NEW white (0).** Not a single green→white transition: no parser broke in the
  window.
- **RECOVERED (6):** **Kino Świt**, **Kino Ślęża**, **Jaworzyna**, **Kino
  Kinematograf** and **Miejskie Centrum Kultury** are all green now (10/10 green
  buckets except Świt, which flipped mid-window: `GGGGGGGGWW` newest→oldest).
  Ślęża recovering is the one that was *predicted* — the 08-15 entry read its
  "Odyseja" card with an unpopulated showtime slot as a break that was about to
  end, and it did.
- **Chemik's fix is holding:** 10/10 green, four days after @c2a9b2fd0. Its
  portal-mate Kino Twierdza likewise 10/10.
- **LEFT the white set by going RED (1):** **Kino Warszawa** (Przeworsk) — its
  certificate expired, see below.
- **NEW red (2):** **Kino NCKF EC1** and **Kino Warszawa**. Both TLS, and they
  are opposite cases — one is ours to fix, one is not.

### Kino NCKF EC1 (Łódź) — `fixed` @931a5aa09 — a leaf-only chain, not a dead site

RED (not white), so strictly outside this run's brief, but it is a PL venue gone
completely dark on a five-minute diagnosis, so it got fixed.

`ec1lodz.pl` renewed on **2026-08-13** onto a leaf issued by `Certum OV TLS G2
R39 CA` and **serves that leaf alone — a one-cert chain, no intermediate.** The
site is perfectly healthy: a plain `curl` of
`ec1lodz.pl/narodowe-centrum-kultury-filmowej/repertuar-kina/` returns 200 with
1.35 MB, and the cert is valid to Feb 2027.

The trap is that the anchor was **never** missing. The R39 intermediate chains to
`Certum Trusted Root CA`, which `TlsTrust` has bundled since the Kinomuzeum work
— so "PKIX path building failed" here did not mean "unknown CA", it meant the
path builder had nothing to bridge leaf→root with. `enableAIAcaIssuers` exists
for exactly this and is on; it evidently does not survive the worker's egress
(the venue stayed red across every bucket in the window). Same remedy as
artmuseum.pl's case 2b: bundle the intermediate so the path closes offline with
no per-handshake network fetch.

**The evidence, reproducible in two commands:**

```
openssl verify -CAfile certs/certum-trusted-root-ca.pem ec1-leaf.pem
  → error 20: unable to get local issuer certificate
openssl verify -CAfile certs/certum-trusted-root-ca.pem \
               -untrusted certum-ov-r39.pem ec1-leaf.pem   → OK
```

**Test (fail-before / pass-after):** `TlsTrustSpec` — "bundle the Certum OV R39
intermediate so ec1lodz.pl's leaf-only chain needs no AIA fetch". Before:
`Certum OV R39 intermediate not bundled`. It pins the intermediate's published
SHA-256 (`F54CE21E…9C4C`) and asserts `intermediate.verify(root.getPublicKey)`,
i.e. that the bundled pair really is a complete offline path — the same shape the
home.pl case already uses. `sbt testUnit` green (7 + 979 + 2986 + 553 + 49).
No snapshot layer moves: a trust bundle changes no scraper output.

**The unit test is the gate, but the load-bearing verification was a REAL JVM
handshake** — the standing rule for this class is "verify through the JVM, not
curl", because OpenSSL accepts chains the JVM rejects. A throwaway spec did an
`HttpsURLConnection` GET of the live repertoire page through
`TlsTrust.augmentedContext` with `enableAIAcaIssuers` forced to **false**, so
only the bundled anchors could close the path: **200, 1,355,401 bytes**. Backing
the one PEM out of `BundledRootResources` and re-running reproduced prod's
failure exactly (`AbstractTrustManagerWrapper.checkServerTrusted`). The spec was
deleted before committing — it needs the network, so it does not belong in
`testUnit`.

**CONFIRMED IN PROD.** All jobs green in the deploy run (unit, integration, the
three e2e legs, every Chrome + WebKit page-test shard, iOS LocalServer) and all
six deploys rolled, `kinowo-worker` among them. EC1 scrapes hourly and had been
`failures=1` on every bucket through **21:00 UTC**; the **22:00 UTC bucket — the
first scrape after the deploy — is `successes=1`**, so the venue is not merely
un-red, it is returning showtimes again.

### Kino Warszawa (Przeworsk) — `unfixable: upstream certificate expired` (and dormant anyway)

The other red, and the mirror image of EC1 — **nothing on our side can fix it.**
`*.przeworsk.um.gov.pl` expired **2026-08-20 00:00 UTC**, which is exactly when
the bar turned red. Per the standing rule the first probe was plain `http://`:
port 80 is **not served** (Zyte returns 476, local curl times out), so the
scheme flip that recovered Kozienice and Wybrzeże is not available here.

Fetched anyway through Zyte to see what is behind the wall: 200, 27,064 bytes,
**0 `movies-movie__single`** — byte-for-byte the same empty shell it has served
since 08-12. So the venue was film-dormant before the cert died and still is.
No action: not worth disabling verification for one host (and per the standing
rule, no per-host special-casing), and the fix is the town hall renewing.
Expect it back as a white bar.

### The 13 white venues — all re-probed live, all genuinely empty

| Venue | Client / URL | What the source shows |
|---|---|---|
| ADA Kino Studyjne | `BiletynaClient` `www.biletyna.pl/Warszawa/ADA-Kino-Studyjne` | **0** events of any `@type` (Place/PostalAddress/Geo only); renders "Brak wydarzeń" |
| Kino PDK | `biletyna.pl/Pyrzyce/Pyrzycki-Dom-Kultury` | 0 `ScreeningEvent`; 1 `TheaterEvent` + 1 `ComedyEvent` |
| Kino MDK | `Bilety24OrganizerClient` `…/miejski-dom-kultury-w-radomsku-1546` | **0** `Film:` (23 Koncert, 6 Spektakl, 4 Wydarzenie, 1 Wystawa) |
| Kino nad Wartą | `…/koninskie-centrum-kultury-1626` | **0** `Film:` (26 Koncert, 5 Spektakl, …) |
| Kino Wisła Brzeszcze | `…/osrodek-kultury-w-brzeszczach-1539` | **0** `Film:` (23 Koncert, 4 Spektakl, …) |
| Piast | `…/ostrzeszowskie-centrum-kultury-601` | **0** `Film:` (28 Koncert, 5 Spektakl, …) |
| Kino CK Lublin | `Bilety24Client` `ck-lublin.bilety24.pl/repertuar/` | 15 events, **all 15 fetched**: 11 Koncert, 2 Wydarzenie, 1 Widowisko, 1 Spektakl — **0 Film** |
| Kino Kuźnica | `SystemBiletowyClient` `shd.systembiletowy.pl` | events wrapper empty, `#repertoire-no-events` showing; 184 calendar days Aug 2026–Jan 2027 all "brak terminów" |
| Patria | `kinopatria.com/repertuar/` | 7 day tabs (20–26 Aug), **0/7** populated `data-movie`; "Brak filmu" |
| Studio (Opole) | `mdk.opole.pl` two-slug fallback | break slug carries "…nasze kino jest nieczynne… Startujemy już 3 września" |
| Kino Chatka Żaka | `umcs.pl/pl/kalendarz-wydarzen,9469,1.lhtm` | `header-light` = 0, `box-row` = 0; "Brak wydarzeń" |
| Kino Zachęta | Filmweb 2405 | literal `[]` (2 bytes) for all 7 dates 08-21…08-27; `/info` still names it Kino Zachęta, Kleczew |
| DKF Politechnika | Filmweb 1645 | literal `[]` for all 7 dates; `/info` still names it, Wrocław |

Every one of the 13 is `intentionally-dormant`, re-confirmed live this run. Two
notes for the next run, both **not** action items today:

- **biletyna.pl now Cloudflare-challenges a residential IP too** (403 on the
  "Just a moment…" page), not just datacenter egress. Probing ADA and PDK needed
  the Zyte route the scraper itself uses (`bnFetch`); ADA additionally needed
  `browserHtml` — the plain `httpResponseBody` fetch came back "Website Ban".
  If a future run reports these two as empty from a bare `curl`, that is a
  blocked probe, not a dormant venue.
- **Studio (Opole) is one week from its own deadline.** The in-season
  `kino-studio.html` slug is still a soft-404 (200, `view_templates-404`, 0
  `div.ckeditor`) and the homepage still links "Kino STUDIO" to
  `kino-studio-przerwa.html`. The notice says 3 September. The **next run falls
  after that date**: check whether the repertoire comes back on
  `kino-studio.html` or whether they keep a new slug — if the latter,
  `KinoStudioClient.RepertoireUrl` needs updating and its fixture re-recording.

## 2026-08-15

**PL: 20 white, 0 red**, out of 322 non-enrichment services, newest bucket
2026-08-15 20:45 UTC. `kinowo_de` and `kinowo_uk` hold **zero** `uptimeBuckets`
for the fifth run running (workers stopped since 2026-08-02, ~24 h retention) —
Poland only, again.

**One code change, and this one is a genuine parser break with films behind it:
Chemik (Kędzierzyn-Koźle), `fixed` @c2a9b2fd0.** The venue was NOT dormant; we
had simply stopped recognising its rows.

**Set changes vs 2026-08-12 (19 white / 3 red then):**
- **NEW (1):** **Chemik** — `fixed` @c2a9b2fd0 (below). New to this log entirely.
- **FELL OFF (0)** — every one of the 19 carried over.
- **The RED set emptied (3 → 0):** **Kino Powiśle**, **Zacisze** and **Kino
  Muzeum** are all **24/24 green** this run. Powiśle's dead MSI host answered
  again on its own, so its `needs-human` ("own-site parser or retirement?") is
  **closed without a decision being needed** — do not re-open it, and do not
  retire the venue. Zacisze and Kino Muzeum squeezing under the 8 s adaptive
  budget for a full day likewise closes the "is the budget too tight" question
  for now.

**Distribution check:** 19 of the 20 whites are `allZeroHistory = true`. The five
that are not (ADA, CK Lublin, PDK, Kino Warszawa, Miejskie Centrum Kultury —
`allZero=false` in the sweep) each carry a **single isolated red/yellow timeout
blip** among 24 zeroes, not a green. So there is again **not one green→white
transition** in the window: no OTHER parser broke in the last 24 h. Chemik
itself flipped more than 24 h ago, before the window opens.

### Chemik (Kędzierzyn-Koźle) — `fixed` @c2a9b2fd0 — the venue prefix lost its spaces

`MsiClient` on `bilety.mok.com.pl`, which hosts **two** cinemas on one MSI page
and tells them apart by prefixing every title with the venue name. Chemik is
wired `titlePrefix = Some("Chemik")`, Twierdza `Some("TWIERDZA")`.

**The discriminator that made this a five-minute diagnosis:** **Kino Twierdza was
24/24 GREEN on the same 24 h, off the same URL, on the same fetch.** One venue
white and its portal-mate green cannot be a fetch, TLS, host or platform problem
— it can only be something that distinguishes the two, and the only thing that
does is the prefix match. (Worth reaching for in future runs: on any shared
portal, check the co-tenant's bar before probing anything.)

Live capture of `?sort=Name&date=2026-08` confirmed it — 13 films on the page,
split as:

| Venue | Rows | Spelling |
|---|---|---|
| TWIERDZA | 8 | `TWIERDZA - BUNTOWNIK` (spaced, unchanged) |
| Chemik | 5 | `Chemik-Flavia de Luce` (**no spaces**), and on two rows `.Chemik-.Psi Patrol i dinozaury` (**a stray dot on either side of the dash**) |

`cleanTitleForVenue` matched the literal `"Chemik - "`, so all five Chemik rows
returned `("", Nil)` and were dropped — a parse that succeeds and yields nothing,
i.e. white, indistinguishable from a dark venue.

**The fix keeps the dash and drops the ceremony:** the separator is punctuation a
box office typed, not a contract, so the matcher now accepts optional
whitespace/dots around a required `[-–—]`. Twierdza's spaced form still matches
the same rule.

**The drift is older than the bar, and the June corpus proves it.** The checked-in
`08-06-2026` fixture already carried **two** tight-dash rows —
`Chemik-K-popowe łowczynie demonów` and `Chemik-DKF:Orły Republiki` — that we
have been silently dropping since the corpus was recorded. Both now surface, so
this is a coverage fix as well as a white-bar fix.

**The second of those needed a title rule too, or it landed as garbage.**
`Chemik-DKF:Orły Republiki` cleans to `Dkf:orły republiki`, and
`xtra-dkf-bare-prefix` required a space after the colon (`^DKF\s*[-–—:]\s+`), so
the query went out with the banner attached, resolved to nothing, and the
screening projected as its OWN bare film record — no year, no poster, no
ratings, sitting next to the real `Orły republiki`. Widening the tail to `\s*`
lets it strip, and the slot now folds onto the real film as a `cinemaTitles`
spelling. Caught only because the snapshot regen surfaced the orphan row; worth
remembering that a scraper fix can hand the pipeline a title no rule was written
for.

**Tests (fail-before / pass-after):**

| Spec | Case | Before |
|---|---|---|
| `MsiClientSpec` | "keep Chemik's feed when the portal drops the spaces around the venue dash" | `List() was empty` |
| `MsiClientSpec` | "still read Twierdza's spaced prefix off the same tightened portal" | passed — pins that the OTHER venue never broke |
| `ExtraTitleRulesSpec` | `"DKF:Orły Republiki" -> "Orły Republiki"` | `"[DKF:]Orły Republiki" was not equal to "[]Orły Republiki"` |

The fixture is the real live page (both `date=2026-08` and `date=2026-09`),
recorded under `RealHttpFetch`'s exact `Chrome/124.0.0.0` User-Agent into
`kino-mok-kedzierzyn-tight-prefix/`, and the Twierdza test is the load-bearing
half of the pair: it proves the fixture is a working page and that the fix is
about the prefix, not about the fetch.

**Snapshots: two of the three layers moved, and they moved by exactly two slots.**
`expected-schedules.txt` gains `Chemik / K-popowe łowczynie demonów` and, on the
real `Orły republiki` film, `Chemik / Dkf:orły republiki` plus that spelling in
`cinemaTitles`. `read-model-snapshot.json` gains the two `opole|Chemik` screening
rows. **`expected-*.html` did NOT move** and needed no regen — Kędzierzyn-Koźle
is in the Opole city, and the rendered snapshots cover Poznań / Wrocław /
Warszawa / `/plan` only; `PageSnapshotSpec` 5/5 green untouched, which is the
check that says so rather than an assumption.

**CONFIRMED IN PROD, and here a green bar IS the success criterion** (unlike
Kinematograf, whose fix had to be confirmed by the *absence* of a red). Chemik
ran 24 consecutive `zero` buckets through **21:00 UTC**; the **21:30 bucket — the
first scrape after the deploy rolled — is `green` with `successes=1`**. All CI
shards green (`ci / test`, `integration-test`, `e2e` rest/scrape/staging, every
page-test shard on Chrome + Safari, `mobile-local-server`, iOS and Android
LocalServer) and all six deploys rolled.

**A guard was considered and deliberately NOT added.** The natural sibling of the
Sfinks/Kinematograf guards would be: if the page parsed rows but none carried our
prefix, throw (red) instead of returning empty. It would have caught this drift
on the first scrape. It is wrong here anyway — on a **shared** portal the other
venue's rows are always present, so a genuinely dark Chemik in a quiet January
with Twierdza still screening would go permanently RED. The co-tenant's green bar
is the signal, and it is available for free on /uptime; a throw would trade a
diagnosable white for a false red.

### The 19 carried-over venues — all re-probed live at the URL the scraper uses

All 19 returned **HTTP 200 with no cross-host redirect**, and none carries a film
programme. Counts measured this run.

| Venue | Client / URL | What the source shows |
|---|---|---|
| ADA Kino Studyjne | `BiletynaClient` `www.biletyna.pl/Warszawa/ADA-Kino-Studyjne` | JSON-LD holds **0** events of ANY type (Place/PostalAddress/Geo only); "Brak wydarzeń" |
| Kino PDK | `biletyna.pl/Pyrzyce/Pyrzycki-Dom-Kultury` | 0 `ScreeningEvent` of 2 (1 ComedyEvent, 1 TheaterEvent) |
| Miejskie Centrum Kultury | `biletyna.pl/Aleksandrow-Kujawski/Miejskie-Centrum-Kultury` | 0 `ScreeningEvent` of 4 (3 MusicEvent, 1 ComedyEvent) |
| Kino MDK | `Bilety24OrganizerClient` `…/miejski-dom-kultury-w-radomsku-1546` | 86 dated slot titles, **0** `Film:` (10 Spektakl, 30 Koncert, 4 Wydarzenie, 1 Wystawa) |
| Kino nad Wartą | `…/koninskie-centrum-kultury-1626` | 90 slots, **0** `Film:` (36 Koncert, 6 Spektakl, …) |
| Kino Wisła Brzeszcze | `…/osrodek-kultury-w-brzeszczach-1539` | 74 slots, **0** `Film:` |
| Piast | `…/ostrzeszowskie-centrum-kultury-601` | 90 slots, **0** `Film:` |
| Kino CK Lublin | `Bilety24Client` `ck-lublin.bilety24.pl/repertuar/` | 16 distinct `/wydarzenie/?id=` events, **every one fetched**: 10 Koncert, 2 Spektakl, 2 Wydarzenie, 1 Widowisko — **0 Film** |
| Kino Świt | `SwitClient` `dkswit.com.pl/kino/` | **0** real `div.cks-movie-card`; "Brak nadchodzących seansów filmowych." |
| Patria | `kinopatria.com/repertuar/` | 7 day tabs, all `data-movie=""`, 0 populated; "Brak filmu" |
| Studio (Opole) | `mdk.opole.pl` two-slug fallback | in-season slug: **0** `div.ckeditor` (soft-404); break slug: 1, "W czasie wakacji nasze kino jest nieczynne" |
| Kino Ślęża | `rcks.pl/kino-sleza/repertuar/` | **2** `div.movie` now — see below |
| Kino Chatka Żaka | `umcs.pl/pl/kalendarz-wydarzen,9469,1.lhtm` | `header-light` = 0, `box-row` = 0, "Brak wydarzeń" |
| Kino Kuźnica | `shd.systembiletowy.pl` | **185** × "brak terminów", 0 bookable |
| Kino Warszawa (Przeworsk) | `MsiClient` MSI months | 0 titles for both 2026-08 and 2026-09, byte-identical 27,098 B shells (unchanged from 08-12) |
| Jaworzyna | `EkobiletClient` `ekobilet.pl/kino-jaworzyna` | 10 `card-date`, **0** `available-color`, 9 `pointer-events-none`, 0 `event-card` |
| Kino Kinematograf | `muzeumkinematografii.pl/kino/repertuar-kina/` | 0 cards, `items-counte` = **0 wydarzeń**, 9 day tabs, 8 "brak seansów" |
| Kino Zachęta | Filmweb 2405 | literal `[]` (2 bytes) |
| DKF Politechnika | Filmweb 1645 | literal `[]` (2 bytes) |

#### Kino Ślęża — the summer break is ENDING, and the parser is right to stay quiet

`rcks.pl` now renders **two** `div.movie` blocks, not one: the standing
"Wakacyjna przerwa 🌞" notice **and a real film — "Odyseja"** (Nolan, with genres,
`// napisy //` and a full synopsis). That looks alarming next to a white bar, and
it is not: the card's own showtime slot reads **"Seans: Daty i godziny seansów
podamy niebawem"** and the page carries **zero** `D.MM.YYYY` rows. `parseMovie`
requires `showtimes.nonEmpty`, so it correctly declines to emit a film with no
screenings. **Nothing to fix; this is the earliest possible warning that the
venue is about to come back.** Next run: if "Odyseja" is still there with dates
posted and the bar is still white, THAT is drift — start at
`KinoSlezaClient.parseShowtimes` and the `<h6>Seans:</h6><ul><li>` shape.

#### A methodology note worth keeping: probe the URL the CATALOG names

A first sweep this run used venue URLs copied from the prose of previous entries
(`biletyna.pl/organizator/<slug>`, `mdkradomsko.bilety24.pl`, `kinoswit.pl`) and
came back with **nine 404s and two timeouts** — a result that reads like mass
site drift and is entirely an artifact. Every one of those hosts is fine; the
scrapers use different URLs (`www.biletyna.pl/<City>/<Venue>`,
`www.bilety24.pl/kino/organizator/<slug>-<id>`, `dkswit.com.pl/kino/`). **Take
the URL from `CinemaScraperCatalog` (and the client's `RepertoireUrl`), never
from this log's prose.** The uptime data says so independently: a white bar means
the fetch SUCCEEDED, so a 404 in your own probe is your probe's bug, not the
venue's.

### Next run's re-check list

1. **Chemik** — already **confirmed green at 21:30 UTC**, so nothing is owed
   unless it goes white again. If it does, the prefix drifted further; check
   whether the dash itself is gone (e.g. `Chemik Flavia de Luce`), which the
   current matcher still requires.
2. **Kino Ślęża** — sharpest item now. "Odyseja" is on the page awaiting dates;
   once dates are posted the bar must go green.
3. **Jaworzyna** — the 18.08 repopulation prediction is now DUE. Still 0
   `available-color` on 15.08. If it is white after **20.08**, that IS drift →
   `EkobiletClient.availableDates`.
4. **Kino MDK** — after 31.08 (WAJDA cycle), per the 2026-08-08 trigger.
5. **Studio (Opole)** — after 3 September; the two-slug fallback already handles
   the break, nothing owed before then.
6. **Kino Kinematograf** — still white, **not** red, so the @83cee0128 guard is
   not misfiring. Remaining item unchanged: around **03.09 / 10.09** check whether
   the two `wydarzenia` film events show up in the repertoire module; if they
   screen while it still reads "0 wydarzeń", repoint at
   `/wydarzenia/aktualne-wydarzenia/?wydarzenia_kategoria=kino`.
7. **DKF Politechnika** — when the academic year starts.
8. **Powiśle / Zacisze / Kino Muzeum** — recovered, all green. Nothing owed.

---

## 2026-08-12

**PL: 19 white, 3 red**, out of 310 non-enrichment services, newest bucket
2026-08-12 05:00 UTC. `kinowo_de` and `kinowo_uk` hold **zero** `uptimeBuckets`
for the fourth run running — their workers have been stopped since 2026-08-02
and retention is ~24 h, so there is still no DE/UK signal to read. Poland only.

**One code change, and it is a white-bar fix: Kino Kinematograf (Łódź),
`fixed` @83cee0128** — though not in the way the bar suggested. The venue is
genuinely dormant, so white is the *right* colour today; what was wrong is that
we could not have told the difference if it weren't.

**Set changes vs 2026-08-08 (19 white / 2 red then):**
- **FELL OFF (1):** **Kino Sfinks** — genuinely **recovered**, confirmed at
  source (below). Its five-run-old `needs-human` is now closed.
- **NEW (1):** **Kino Kinematograf** — `fixed` @83cee0128 (below). New to this
  log entirely; it had never appeared in a white set before.
- **Carried over (18):** ADA Kino Studyjne, DKF Politechnika, Jaworzyna, Kino
  Chatka Żaka, Kino CK Lublin, Kino Kuźnica, Kino MDK, Kino nad Wartą, Kino PDK,
  Kino Ślęża, Kino Świt, Kino Warszawa (Przeworsk), Kino Wisła Brzeszcze, Kino
  Zachęta, Miejskie Centrum Kultury, Patria, Piast, Studio (Opole).

**All 19 have `allZeroHistory = true`** — white across the entire retained
bucket window, with not one green→white transition anywhere in the set. As in
the last three runs, that distribution is itself the evidence that no parser
broke in the last 24 h: markup drift lands as a green→white flip and there are
none. Kinematograf is new to the *set* but not to the window — it flipped
between the 08-08 sweep and the start of the current one, i.e. more than 24 h
ago.

### Kino Kinematograf (Łódź) — `fixed` @83cee0128 — dormant, but we couldn't have known

`KinematografLodzClient`, own-site scraper on the Muzeum Kinematografii's
WordPress repertoire page. The only venue new to the white set, so it got the
hardest look, and the first read of it was wrong in a way worth recording.

**The alarming part first: the URL we scraped no longer exists.**
`muzeumkinematografii.pl/repertuar/` now **301s** to
`/kino/repertuar-kina/` (LiteSpeed, single hop). That is the Helios shape
exactly, and the followed page carries **zero** `article.cwb-movie-item` — the
one structure the parser reads. So the initial diagnosis was "site restructured,
parser blind".

**It is not markup drift, and the old fixtures prove it.** Both recorded
captures of the OLD page already contain the day carousel *and* the cards
together, so the widget is not new and the cards were not moved out of it:

| Capture | `cwb-movie-item` | day strip says | `items-counte` |
|---|---|---|---|
| `kinematograf-lodz` (07-06-2026) | 19 | 4 / 2 / 2 / 2 / 3 / 3 seanse | 19 wydarzeń |
| `08-06-2026` corpus (June) | 18 | 2 / 2 / 2 / 3 / 3 seanse | 18 wydarzeń |
| **live 2026-08-12** | **0** | **"brak seansów" ×8** | **0 wydarzeń** |

The structure is unchanged in kind; the programme is empty. Three independent
sources agree the venue is not screening:

- the repertoire widget's own counter reads **`0 wydarzeń`** and all eight day
  tabs 12–19.08 read **"brak seansów"**;
- the museum's ticket shop `sklep.kinomuzeum.pl/MSI/mvc/pl/` offers **9 events
  for 13–16 and 20–22 August, every one of them a museum tour** ("Zwiedzanie
  muzeum") — **zero film seats on sale**, which a screening cinema would have;
- `?days=<date>` is ignored server-side and flatpickr pins `minDate:"today"`, so
  there is no other date to query, and the homepage carousel renders its own
  `div.cwb-movie-empty-state` → "Brak seansu".

**There is no closure notice anywhere** — `/`, `/kino/` and the repertoire page
carry no "przerwa" / "nieczynne" / "remont" wording; `/kino/` says only "Kino
Kinematograf działa od 2006 r.". The newest `cinema` post is dated **2026-06-24**
and none of the 123 of them carries a date or time, so the repertoire module has
simply had nothing added since roughly the era of our June fixtures.

**Two film events DO exist, but as `wydarzenia` posts, in September** —
"Monterey Pop Tour z Piotrem Metzem" (03.09.2026 17:00) and "Kino według Kuby
Mikurdy: Opętanie" (10.09.2026 17:00), the first of which is the single entry
under `?wydarzenia_kategoria=kino`. Curated one-off screenings of the kind that
get event pages rather than repertoire rows. **Not scraped, deliberately** — two
event posts are not a repertoire, and the module we read still exists and is
still wired.

**So the fix is not a new parser — it is closing the blind spot.** The venue was
white for a reason we could not distinguish from the reason we most feared, and
the site had just restructured its URLs, which is the shape that renames a card
class next. Kino Sfinks taught this lesson at @73f19c8a5; this venue had the
same hole, plus a better signal to plug it with: **the widget publishes its own
item count**, so a zero-card parse can be *cross-checked* rather than merely
tolerated.

The guard is therefore two-pronged, and the second prong is the one that
matters:

1. **No accounting at all → throw.** A page with neither a card nor the widget's
   own `span.items-counte` / `a.cinema-day-item` markers is not the repertoire,
   and its emptiness says nothing about the programme (soft-404, slug rename,
   redirect landing elsewhere).
2. **Accounting that contradicts the parse → throw.** If the widget advertises
   *N > 0* screenings and we parsed none, the card is no longer
   `article.cwb-movie-item`. This is the case a presence-only guard still paints
   white — the page looks readable and merely empty — and it is precisely what a
   restyle would look like.

Today's page hits neither branch (`items-counte` = 0, all tabs "brak seansów"),
so **Kinematograf stays correctly white** and the day the museum restyles or
repopulates into markup we cannot read, it goes **red** instead of staying white
forever.

**One trap found while writing the guard, and it is a false-friend of exactly
the kind that would have made the guard useless:** the museum's **homepage**
carries both `div.movies-tickets-inner` and `div.cwb-movie-empty-state`, for its
own "coming soon" carousel. Either would have looked like a fine "this is the
repertoire, and it is empty" marker — and both would have accepted the *wrong
page* as a schedule, silently restoring the blind spot the guard exists to
close. The guard is keyed on the item counter and the day strip only, and the
spec pins that with the real homepage capture.

**The URL now points where the site publishes today** (`/kino/repertuar-kina/`)
rather than leaning on a redirect, and `sourceUrl` — /uptime's link — moves from
the homepage to the page actually scraped, so the next investigator clicking
through lands on the evidence.

**Tests (fail-before / pass-after):** `KinematografLodzClientSpec` goes from 11
inline-HTML cases to 20, and replays **four** captures where it previously
replayed none:

| Fixture | Asserts |
|---|---|
| `kinematograf-lodz` (real, populated) | still parses films — the guard must not break a working page |
| `kinematograf-lodz-dormant` (live 12-08) | returns empty and does **not** throw — a dormant venue stays white |
| `kinematograf-lodz-shape-drift` (live site ROOT) | throws — and pins the `movies-tickets-inner` false friend |
| `kinematograf-lodz-cards-restyled` (derived) | throws with "advertises 19" |

The last is the real 07-06 capture with **only its card class renamed**
(`cwb-movie-item` → `cwb-screening-card`) and nothing else touched, so the
counter still says 19 and the day strip still advertises 16. That is what a CMS
restyle looks like from outside, and it is not reachable from any live capture
because the venue is empty — the same reason the Sfinks spec used the site root
as its drift stand-in. Confirmed failing before the change with "Expected
exception java.lang.RuntimeException to be thrown, but no exception was thrown"
on exactly those two guard tests, and 20/20 green after.

**Verified prod-safe before pushing, which mattered here:** the guard throws on
a page it doesn't recognise, so a fixture captured under a browser UA that
differed from the worker's would have turned a correct white into a false RED.
Re-fetched under `RealHttpFetch`'s exact `Chrome/124.0.0.0` User-Agent: byte
count identical, `items-counte` = 0, 8 day tabs, 0 cards → `Some(0)` → empty, no
throw.

**CONFIRMED IN PROD — and note what "confirmed" means for this fix.** A green bar
is NOT the success criterion here, because the venue is dormant and the guard's
whole purpose is that a dormant venue keeps its white. The thing that had to be
verified is the *absence* of a false red, and it was: the **06:00 UTC bucket —
the first scrape after @83cee0128 deployed — is `zero` (white) with
`failures=0`**, unchanged from the 24 buckets before it. All CI shards green
(`ci / test`, `integration-test`, all three `e2e` shards, every page-test shard,
`mobile-local-server`) and all six deploys rolled.

**Snapshots: neither layer moved, and the fixture rename is the load-bearing
part of the diff.** `FakeHttpFetch` keys a fixture on `host / path /
query-fingerprint`, so changing the scraped URL without moving the recorded file
would have silently dropped the venue from the corpus. Both captures were
`git mv`'d to the new request path (`muzeumkinematografii.pl/kino/repertuar-kina`)
and `expected-schedules.txt` still carries its **21** Kinematograf lines,
`read-model-snapshot.json` unchanged. **Verified by hiding the corpus fixture and
re-running:** 2 of the 3 e2e tests fail without it, so the rename really is what
keeps the venue in the snapshot rather than something that merely appeared to
work. `testUnit` green, `FilmScheduleEndToEndSpec` 3/3 green.

### Kino Sfinks (Krosno) — `recovered`, and the longest-standing `needs-human` is closed

Carried as `needs-human` since 2026-07-11 — "no film-row markup renders
anywhere, so the parser cannot be rebuilt or test-backed" — and re-confirmed
empty on 08-04 and 08-08. **The venue has repopulated and the existing parser
reads it.** Confirmed at source, not inferred from the bar:
`kinosfinks.okn.edu.pl/wydarzenia-harmonogram.html` returns 200 / 143 KB with
`table.widok_listy` present, `div.empty-results` **gone**, and **12
`tr[onclick]` film rows** — VIVALDI I JA, KANDYDACI ŚMIERCI and HISTORIE
RÓWNOLEGŁE at 17:00 / 18:00 / 19:00 on 21–22.08 and onward.

The bar agrees and dates the return precisely: zero for the three buckets
through **2026-08-11 07:15 UTC**, then **green from 08:15 UTC** and green for
every one of the 21 buckets since.

Worth noting what this vindicates. The guard added at @73f19c8a5 was written for
exactly this moment — "the day the venue repopulates into markup this parser
cannot read, it goes red instead of staying white forever". The venue
repopulated into markup the parser *can* read, so the guard stayed silent and
the venue simply went green. That is the good outcome, and it means the
`widok_listy` selector was never the problem: the CMS had not migrated away from
it after all, the calendar had merely been empty. **Do not re-open this venue.**

### PL out-of-scope REDs (3) — fetch failures, not whites, but probed and characterised

The RED set grew from 2 to 3 and none of them is a white-run target. Logged with
evidence anyway, because two of the three are cheap and one has a known
precedent that a future run should not re-derive from scratch.

- **Kino Powiśle** (`MsiClient` on `kinosztumbilety.pl`) — 24/24 buckets red,
  `CircuitOpenException` behind `HttpConnectTimeoutException`. **The host is
  genuinely dead, and the plain-HTTP escape hatch does NOT apply here.** Probed
  both schemes with a 25 s cap: `https://` and `http://` each sat at
  `connect=0.000000` and timed out with `http=000`. That is no TCP at all, not an
  expired leaf — so this is the **Kino Zamek MSI shape** (dead ticketing portal),
  not the Kołobrzeg / Kozienice shape (working host, bad certificate). Zamek was
  ultimately fixed by moving onto the venue's own site; the same question should
  be asked here. **needs-human: decide whether Sztum's own site carries a
  programme worth a bespoke parser, or the venue should be retired.**
- **Zacisze** (`KinoZaciszeClient`) and **Kino Muzeum** (`KinoMuzeumGdanskClient`)
  — both `TimeoutException: … exceeded 8000ms adaptive budget`, each with a
  single green blip among 24–25 reds. **Both hosts answer fine from here**:
  `www.kinozacisze.pl` 200 / 124 KB / ttfb 1.43 s, `www.muzeum1939.pl` 200 /
  234 KB / ttfb 0.71 s. So the sites are up and the failure is on our side of the
  wire — a latency/budget problem from Fly, not a broken parser or a dead host,
  and the occasional green shows it sometimes squeezes under the budget.
  **needs-human, and explicitly a RED-run question, not a white one:** whether
  the 8 s adaptive budget is too tight for these two hosts.

### The 18 carried-over dormant venues — all re-probed live, all correctly empty

Every one returned HTTP 200 and none carries a film programme. Eight say so in
their own words on the page we scrape; the rest agree by structure. Counts are
from this run.

| Venue | What the source actually shows |
|---|---|
| ADA Kino Studyjne | JSON-LD holds **0** events of ANY `@type`; "Brak wydarzeń… nie mogliśmy odnaleźć wydarzeń" |
| Kino PDK (Pyrzyce) | 0 `ScreeningEvent` of 2 (1 Comedy, 1 Theater — Kabaret Trzecia Strona Medalu, 20.11) |
| Miejskie Centrum Kultury | 0 `ScreeningEvent` of 4 (1 Comedy, 3 Music — Papa D, Czerwone Gitary) |
| Kino MDK (Radomsko) | organiser `Film:` = **0**; its own section is `Spektakl:` only |
| Kino nad Wartą (Koło) | `Film:` = **0**; own section Koncert/Spektakl only |
| Kino Wisła Brzeszcze | `Film:` = **0**; the organiser event section is absent — "Brak wydarzeń" |
| Piast (Ostrzeszów) | `Film:` = **0**; own section Koncert/Spektakl only |
| Kino CK Lublin | `Film:` = **0** on both the landing page and `/repertuar/` (30 Koncert, 9 Spektakl) |
| Kino Świt | `div.cks-movie-card` = **0**; "Brak nadchodzących seansów filmowych." |
| Patria (Ruda Śląska) | 7 day tabs 12–18.08, every one `data-movie=""`, 0 film rows; "Brak filmu" |
| Studio (Opole) | break page carries it verbatim: "…nasze kino jest nieczynne… Startujemy już 3 września" |
| Kino Ślęża (Sobótka) | 1 `div.movie`, and it is "Wakacyjna przerwa 🌞" with no showtimes |
| Kino Chatka Żaka | `h3.header-light` = 0, `div.box-row` = 0, "Brak wydarzeń" |
| Kino Kuźnica (Suchedniów) | calendar payload: **184 of 184** days `"disabled":true`, each "— brak terminów" |
| Kino Warszawa (Przeworsk) | **0** events for both 2026-08 and 2026-09 (byte-identical 27,098 B shells) |
| Jaworzyna (Krynica) | 9 `div.card-date`, **0** `available-color`, 9 `pointer-events-none`, 0 event cards |
| DKF Politechnika | Filmweb 1645 → literal `[]` (2 bytes) on 08-12 / 08-15 / 08-20 / 08-29 / 09-10 |
| Kino Zachęta (Kleczew) | Filmweb 2405 → literal `[]` (2 bytes) on the same five dates |

**Two standing `unfixable` verdicts are unchanged and should not be re-opened:**
Kino Zachęta (the venue publishes only a JPEG poster, and the host 403s
non-browser agents — there is nowhere to repoint; see 2026-08-04) and Kino
Ślęża's summer break (`rcks.pl` IS the venue's own site, so there is no second
source).

#### Studio (Opole) — the in-season slug is now a soft-404, and that is FINE

Worth writing down because the raw observation looks alarming and the standing
note said the venue "will follow the live page on its own":
`mdk.opole.pl/kino-studio.html` — the URL named as `RepertoireUrl` — now serves
the site's **404 body under HTTP 200**, and the nav points "Kino STUDIO" at
`kino-studio-przerwa.html` instead.

**This is exactly the case @57429179a was built for and it is working.**
`KinoStudioClient` fetches BOTH slugs and takes the first that renders
`div.ckeditor`, precisely because the status code cannot pick between them.
Verified live this run: the in-season slug has **no** `div.ckeditor` (soft-404),
the break slug **has** one and carries the notice verbatim — "W czasie wakacji
nasze kino jest nieczynne… **Startujemy już 3 września** :)". So the client reads
the break page, finds no films, and returns empty — a correct white, not a
missed page. **Nothing is owed before 3 September**, and the earlier note's
promise holds: when the season restarts on the in-season slug, the in-season slug
wins on its own because it is tried first.

#### Jaworzyna — the 18.08 prediction has NOT yet come true, and the trigger stands

The 2026-08-08 entry predicted this venue would repopulate around **18.08** and
said that if it is still white after **20.08**, that IS drift. It is 12.08, so
the prediction is not yet due — but the forward probe was run anyway and it
matches the shape that entry described exactly: `?date=2026-08-18` shifts the
strip to 16–24.08 and **still** shows 0 `available-color` days, because tickets
for the 18–20 block are not on sale yet. 9 of 9 day cards remain
`pointer-events-none` and the page says "Brak wydarzeń na dzisiaj, sprawdź w
innym dniu". **Keep the trigger armed for the first run after 20.08.**

### Next run's re-check list

1. **Jaworzyna** — the sharpest item. Expected to repopulate around 18.08; if it
   is still white **after 20.08** that is drift, not a gap, and deserves a fresh
   look at `EkobiletClient.availableDates`.
2. **Kino Kinematograf** — the fix cannot be confirmed by a bar going green,
   because the venue is dormant and the guard's whole point is that it stays
   white. Two things to check instead: (a) it must still be **white, not red** —
   a red means the live page stopped rendering `items-counte`/the day strip and
   the guard is firing for real; (b) around **03.09 / 10.09**, when the two
   `wydarzenia` film events fall due, check whether the repertoire module
   repopulates. **If those September screenings happen while the module still
   reads "0 wydarzeń", the museum has moved its programme into the events post
   type and the client must be repointed at
   `/wydarzenia/aktualne-wydarzenia/?wydarzenia_kategoria=kino`** — that listing
   has a stable shape (`article.cwb-post-item`, title in `h3.cwb-post-title > a`,
   date in `.post-meta .date-time` as "DD.MM.YYYY, HH:MM").
3. **Kino MDK** — after 31.08, per the trigger from 2026-08-08 (unchanged): if
   the WAJDA cycle starts and films appear on `mdkradomsko.bilety24.pl` but not
   on the central organiser page, repoint the client at the storefront.
4. **Studio (Opole)** — after 3 September. Nothing owed before then; the two-slug
   fallback already handles the break.
5. **DKF Politechnika** — when the academic year starts.
6. **Kino Sfinks** — nothing owed. Recovered and green; do not re-open.
7. **The three REDs** are a red-run question, not a white one — but **Kino
   Powiśle** is the one with a real decision behind it (dead MSI host, own-site
   parser or retirement), and the Zamek precedent says check the venue's own site
   before retiring anything.

---

## 2026-08-08

**PL: 19 white, 2 red**, out of 327 non-enrichment services, newest bucket
2026-08-08 03:15 UTC. `kinowo_de` and `kinowo_uk` still hold **zero**
`uptimeBuckets` — their workers have been stopped since 2026-08-02 and retention
is ~24 h, so there remains no DE/UK signal to read. Poland only again.

**No white venue is white because of a parser bug.** All 19 were re-probed live
this run and every one is genuinely film-empty at the source we scrape. Both code
changes that came out of the sweep are **REDS**, not whites:
**Kozienicki Dom Kultury, `fixed` @ca41320db** — the second venue in five weeks
to be knocked offline by nothing but an expired certificate — and **Kino Zamek,
`fixed` @09cbc2cdb**, moved off a ticketing portal that stopped accepting TCP
onto the castle's own site, which turns out to carry five times the programme
the portal ever did.

**Set changes vs 2026-08-04's 07:45 re-sweep (20 white / 0 red then):**
- **FELL OFF (2):** **Kino Tur** — genuinely **recovered**, and confirmed at
  source rather than inferred from the bar going green (below). **Kino Zamek** —
  did not recover; it moved **white → RED** because its ticketing host went
  TCP-dark, and was then **`fixed` @09cbc2cdb** by moving it onto the castle's own
  site (below).
- **NEW (1):** **Jaworzyna** (Krynica-Zdrój) — `intentionally-dormant`, a
  scheduled mid-August gap (below).
- **Carried over (17):** ADA Kino Studyjne, DKF Politechnika, Kino Chatka Żaka,
  Kino CK Lublin, Kino Kuźnica, Kino MDK, Kino nad Wartą, Kino PDK, Kino Sfinks,
  Kino Ślęża, Kino Świt, Kino Warszawa (Przeworsk), Kino Wisła Brzeszcze, Kino
  Zachęta, Miejskie Centrum Kultury, Patria, Piast, Studio (Opole). Evidence for
  each in the table below.

**Every one of the 19 has `allZeroHistory = true`** — white across its entire
retained bucket window, with not one green→white transition anywhere in the
set. That distribution is itself the evidence that no parser broke in the last
24 h: markup drift lands as a green→white flip, and there are none. (Jaworzyna
is new to the *set* but not to the window — it flipped between the 08-04 sweep
and the start of the current window, i.e. more than 24 h ago.)

### Kozienicki Dom Kultury — `fixed` @ca41320db — the Kołobrzeg shape, one year later

RED, not white, so strictly out of a white run's scope — but it is the *exact*
pattern the 2026-08-04 entry told the next run to look for ("probe `http://`
before writing a host off, because a cert that expires is a **scheme** problem,
not a reachability one"), and it cost one probe to confirm.

- Error: `CircuitOpenException: circuit open for bilety.dkkozienice.pl`, behind
  `SSLHandshakeException (certificate_expired)`.
- The leaf is `CN=*.dkkozienice.pl`, issuer `nazwa.pl / nazwaSSL DV TLS G2 E29
  CA`, `notAfter = Aug 7 00:00:00 2026 GMT` — **it expired the day before this
  run**, which is why the venue was green on 08-04 and red now.
- **The portal answers the identical page over plain HTTP, with no redirect to
  HTTPS.** `http://bilety.dkkozienice.pl/MSI/mvc/pl?sort=Name&date=2026-08` →
  200, 97,069 bytes; `2026-09` → 200, 32,678 bytes; and the plain-HTTP body is
  **byte-identical** to the `https -k` fetch of the same path (65,826 == 65,826
  on the portal root).
- **The venue is very much screening** — 4 events for 2026-08-08 alone (`Psi
  Patrol i dinozaury` ×2, `Spider-Man. Całkiem nowy dzień 2D DUB`, `O czym sobie
  nie mówimy`), with forward dates through 10–12.08. The parent site
  `dkkozienice.pl` lists the same day and points bookings at
  `bilety.dkkozienice.pl/Msi/mvc/pl`. So this was a live cinema going dark
  purely on a scheme.

**Fix:** one character-class change in `CinemaScraperCatalog` — the `MsiClient`
`baseUrl` for this venue is now `http://bilety.dkkozienice.pl`, exactly as Kino
Wybrzeże has been since @b51d129a9. `MsiClient`'s `baseUrl` scaladoc, which
named Kołobrzeg as *the* plain-HTTP case, now names both and states the general
rule.

**Test (fail-before / pass-after):** `CinemaClientMarkersSpec` grows the
assertion that mirrors the Wybrzeże one —
`sourceUrls("Kozienicki Dom Kultury") shouldBe "http://bilety.dkkozienice.pl"`.
Confirmed failing before the catalog edit (`"http[s]://bilety.dkkozienic..." was
not equal to "http[]://..."`) and passing after. The scheme is the whole fix, so
pinning it is what stops a future edit silently flipping the venue back to red.

**Snapshot: `read-model-snapshot.json` shifted, and ONLY it.** Worth recording
*why*, because it is not obvious in either direction:

- The raw-HTML corpus did **not** move. `FakeHttpFetch` keys a fixture on
  `host / path / query-fingerprint` — **the scheme is not part of the key** — so
  the recorded `08-06-2026/bilety.dkkozienice.pl/MSI/mvc/pl.*` fixtures keep
  resolving untouched and the venue's films are still parsed in e2e. (Same
  reason the Kołobrzeg flip needed no fixture rename.)
- But `MsiScraper` resolves every showtime's booking `abs:href` **against
  `baseUrl`**, so all 19 of the venue's `bookingUrl`s in the projected read model
  flip `https://` → `http://`. Diff verified to be exactly those 19 lines and
  nothing else.
- `expected-schedules.txt` does **not** carry booking URLs, and Kozienice is in
  `radom` — not one of the three snapshot cities — so neither that file nor any
  `expected-*.html` moved. Confirmed by running the e2e spec *before*
  regenerating: only the read-model test failed, and only on the scheme.

**CONFIRMED GREEN IN PROD — no verification is owed to the next run.** Red for
every retained bucket through **04:30 CEST**, then **green at the 06:30 CEST
(04:30 UTC) bucket** — the first scrape after @ca41320db deployed — with
`successes=1, failures=0, zeroes=0`. Note the circuit breaker was no obstacle
even though it keys on the *host* (unchanged by the scheme flip): the worker
restarts on deploy, so its in-memory breaker state starts fresh.

### Jaworzyna (Krynica-Zdrój) — `intentionally-dormant`, and NOT a broken date strip

`EkobiletClient` on `ekobilet.pl/kino-jaworzyna`. The only venue that entered
the white set this run, so it got the hardest look — a green→white flip on an
own-platform scraper is the classic drift signature.

It is not drift. The page is 200 and renders "Brak wydarzeń na dzisiaj"; its
date strip has 9 `div.card-date` for 06.08–14.08, **all** carrying
`pointer-events-none` and **none** carrying `available-color`, which is exactly
the "no screening days" state `EkobiletClient.availableDates` is written to
read. **The class scheme has not been renamed** — checked against live control
venues on the same platform: `opolskielamy` has 7 `available-color` days and 9
event cards, `kinorejs` has 5 and 4. (Two other ekobilet venues,
`kino-centrum-jastrzebiezdrj` and `mokis-bielawa`, render no date strip at all
but do render event cards, so the landing parse alone carries them — also fine.)

The venue's own site says the same thing in its own way, and explains the shape:
`ckkrynica.pl/repertuar.html` publishes the repertoire as a **single JPEG**
(`Repertuar-31.07-20.08abc-2.jpg`, `Last-Modified` 2026-07-31), and that poster
covers **31.07–06.08 and then 18–20.08**. **07–17 August is absent from the
venue's own programme entirely** — today falls in that gap. Filmweb agrees
independently: id 561 still resolves to `{"name":"Jaworzyna","city":"Krynica
Zdrój"}` and returns `[]` for 08-08 through 08-20, with the cinema page reading
"Niestety to kino nie oferuje seansów w najbliższym czasie".

Note the shape for a future run: `ekobilet.pl/kino-jaworzyna?date=2026-08-18`
shifts the strip to 16.08–24.08 but **still** shows zero available days, because
tickets for the 18–20 block are not on sale yet. Ekobilet is not lying and our
parser is not blind — **expect this venue to repopulate on its own around
18.08**. There is no machine-readable second source to move to (the only other
listing, `kino.coigdzie.pl`, carries one stale entry with an ad-injected ticket
link pointing at a *different* venue's ekobilet slug — do not scrape it).

### Kino Tur (Turek) — `recovered`, and the standing migration trigger is now void

Carried as dormant since 2026-07-28 with an armed trigger: *migrate to the
venue's own `mdk.turek.pl` if biletyna is still empty while mdk has a current
monthly article.* **The trigger is now void** — biletyna itself came back.
`biletyna.pl/Turek/Kino-Tur` carries 2 `ScreeningEvent`s ("Super futrzak i
złośliwa wiewiórka", 2026-08-12 10:00; "Kręciołek", 2026-08-26 10:00) alongside
1 `ComedyEvent`, and our bar is green again. Confirmed at source, not inferred
from the bar. **Disarm the trigger; do not migrate this venue.**

### Kino Zamek (Szczecin) — white → RED, then `fixed` @09cbc2cdb

**→ FIXED later the same run.** The `needs-human` below was written on the
assumption that the castle's own listing page carried only two open-air festival
rows, so there was nothing to write a parser against. **That was wrong, and the
way it was wrong is worth remembering: the summer listing is not the venue.**
The `08-06-2026` corpus's own capture of the same page — taken in JUNE, when the
castle was in season — carries **35** kino events, one page per film. The
category collapses to two entries in August because the season is over, not
because the site stopped publishing films. Judging a source's shape from an
off-season snapshot nearly retired a venue that publishes a full monthly
programme.

The MSI diagnosis below stands and the host is genuinely dead. What changed is
the conclusion: the castle's own site is a *better* source than the portal ever
was, so the client now reads `/wydarzenia/kino/` and the event pages it links,
and the dead host is gone from the catalog entirely. Two facts settle that it is
an upgrade rather than a fallback:

- **The portal was already failing this venue while it was still UP.** On
  2026-08-04 it answered fine and advertised **zero** films — that is why Kino
  Zamek was WHITE in the last run — at a time when the castle's own site was
  carrying a nine-film open-air season. We were reporting a programmed cinema as
  empty for reasons that had nothing to do with the outage.
- **The venue's coverage roughly quintuples.** In the recorded corpus Kino Zamek
  goes from **7 films** to **33 films / 45 showtimes**, each with its director,
  and the film set of the whole corpus is unchanged — no junk entered.

**The parser's real difficulty is that one page is not one film**, in three
distinct shapes that all had to read correctly:

| Shape | Example | Where the title lives |
|---|---|---|
| Single film, in season | `casablanca` | `<h1>` — the only bold line near the date is the strand, "SZCZECIŃSKIE ŚWIĘTO KLASYKI FILMOWEJ W KINIE ZAMEK" |
| Cycle, one page many films | `zamkowe-noce-filmowe-2026` | each film's own bold heading; the `<h1>` is the cycle's name |
| Festival umbrella | `szczecinskie-swieto-klasyki-…` | each film's bold heading — but the **14pt** bold marks the WEEK |

So the title comes from the nearest bold heading above a screening only when
those headings actually DIFFER between screenings; otherwise the page is about
one film and the `<h1>` names it. Two traps found by testing rather than by
reading:

- **Keying the heading on the 14pt span** (which is what marks a film on the
  cycle page) produced three "films" called *"II TYDZIEŃ POKAZÓW – GODARD,
  WAJDA…"* — the festival's weeks. Bold alone is the marker; 14pt is the week.
- **The same film appears on two pages** — its own and the umbrella — and only
  its own page carries `Reżyseria:`. Merging by title but keeping the first
  entry seen silently dropped **every** director, because the umbrella page
  happened to be scraped first. The merge has to union the fields.

Dates carry no year in the prose, so each is resolved against the page's own
`<p class="event-details">` list (`21-06-2026`), which does. Past screenings are
DROPPED rather than rolled forward: a cycle page still lists its opening night,
and a next-occurrence rule would have published "30 czerwca", read in August, as
a screening in June 2027.

**Tests:** 15-case `KinoZamekClientSpec` over TWO recorded captures — `kino-zamek`
(2026-08-08, the summer cycle) and `kino-zamek-season` (the June listing from the
corpus, its event pages captured today) — so both shapes are held. Covers the
week-heading trap, the cross-page merge keeping its director, year-from-the-page,
past-night dropping, the listing shape guard (throws) versus a genuinely empty
category (returns empty), and total-outage propagation versus a single page
blipping. `testUnit` 4,530 green, `web/PageTest` 179 green, `FilmScheduleEndToEndSpec`
green and stable.

**Snapshots:** `expected-schedules.txt` and `read-model-snapshot.json`
regenerated; **no `expected-*.html` moved** (Szczecin is not a snapshot city).
The corpus diff is worth reading as the evidence this is an improvement — Zamek's
`cinemaTitles` lose their strand suffixes ("Człowiek z marmuru – wajda: re- wizje"
→ "Człowiek z marmuru", "Milczenie owiec – jonathan demme" → "Milczenie owiec")
and therefore MERGE into the canonical film instead of sitting beside it as a
near-duplicate spelling. **The corpus's film set is byte-for-byte the same set** —
33 Zamek films resolved cleanly against fixtures that already existed
(`casablanca|1943`, `opetanie|1981`), so nothing metadata-less was added.

Not perfect, and worth knowing: one June page whose `<h1>` reads *"Zamkowe Noce
Filmowe: Szkoda, że nareszcie"* keeps the cycle name in its title, because the
strand-stripper deliberately splits on the en-dash only — splitting on the colon
too would truncate it to the cycle's name, which is worse. It is one unresolved
film in a June capture; the live page uses a different shape.

**CONFIRMED GREEN IN PROD, with its films.** Red for every retained bucket
through 08:30 CEST, then **green at the 09:30 CEST (07:30 UTC) bucket** — the
first scrape after the deploy — `successes=1, failures=0, zeroes=0`. And the
programme actually landed in the read model, which is the part a green bar alone
would not prove: `web_screenings` now holds the four remaining open-air nights,
each TMDB-resolved — `ostatniwikingdensidsteviking|2025` (11 Aug),
`fathermothersisterbrother|2025` (18 Aug),
`wartoscsentymentalnaaffeksjonsverdi|2025` (25 Aug) and
`paulrobienapostolprzyrody` (1 Sep), all at 21:30.

*(Note for anyone spot-checking these in mongosh: the stored instants read
`21:30Z` and render as 23:30 in a CEST shell. That is not an offset bug — this
corpus persists a showtime's wall-clock digits as though they were UTC, for every
client. Multikino Tarnów's 10:10 screening is stored `10:10Z` the same way.)*

**The MSI diagnosis that led here (unchanged):**

The standing "festival filter-gap" `needs-human` on this venue is now a
different, harder problem, and the change is worth recording precisely because
the bar colour moved for a reason unrelated to the old diagnosis.

`KinoZamekClient` reads showtimes from an MSI portal at
`bilety.zamek.szczecin.pl` and uses `zamek.szczecin.pl/wydarzenia/kino/` only to
supply the film-slug filter. **The MSI host no longer accepts TCP connections.**
DNS still resolves (`213.155.191.11`), but three `curl` runs each sat at
`connect=0.000000` and hit the 60 s cap with `http=000`, and `nc` to :443 and
:80 both time out. That is what the `TimeoutException: exceeded 8000ms adaptive
budget` is — a dead host, not a slow one. The listing host by contrast is
healthy and fast (200, ~190 KB, ttfb 0.17–0.27 s across three runs).

~~Why this is not fixable in a white run:~~ *(superseded — see above.)* The
reasoning was that rebuilding on `zamek.szczecin.pl/wydarzenia/kino/` meant a new
parser for a page carrying only two entries — `Zamkowe Noce Filmowe 2026`
(open-air, 4/11/18/25 August 21:30) and `44-45 Pomorskie Spotkania z Diaporamą` —
with no regular repertoire to write a fail-before/pass-after test against. The
missing step was checking what that same page looks like IN SEASON, which the
corpus already had on disk: 35 film pages. Both shapes are now covered by
recorded fixtures.

### Kino MDK (Radomsko) — still `intentionally-dormant`, but the picture moved

The venue flagged last run as "most likely to come back". It has **not** come
back on the surface we scrape, but the reason it was dormant has changed, so the
trigger is re-armed rather than merely repeated:

- The bilety24 organiser page (`…-1546`) still lists **0** films (3 Spektakl),
  and the venue's own storefront `mdkradomsko.bilety24.pl` likewise.
- **What changed is the venue site.** Last run every film card on
  `mdkradomsko.pl/kino-radomsko/` led to a `pec-events` page reading "TO
  WYDARZENIE JUŻ SIĘ ODBYŁO" — stale posters for a finished run. **That is no
  longer true**: all three detail pages now open cleanly with no such banner, and
  two carry **future** dates — "WAJDA: re-wizje" from **31 August 2026 17:00**
  and "FEDERICO FELLINI: ciao a tutti!" **7–21 September 2026**. (A third,
  "KONWICKI", carries a junk date of 25 June 1996 while its prose says
  czerwiec–grudzień 2026.)
- These are **cycle** pages, not individual showtimes, so there is nothing to
  scrape yet even if we repointed today.

**Trigger for the next run after 31 August:** if the WAJDA cycle starts and the
films appear on `mdkradomsko.bilety24.pl` but **not** on the central organiser
page, that is the signal to repoint the client at the storefront. If they appear
on neither while the venue site says the cycle is running, the venue publishes
only prose and there is nothing to move to.

### The other carried-over dormant venues — all re-probed live, all correctly empty

Five state it in their own words on the page we scrape; the rest agree by
structure. Counts are from this run.

| Venue | What the source actually shows |
|---|---|
| Kino Świt (Warszawa) | `div.cks-movie-card` = **0**; "Brak nadchodzących seansów filmowych… Dodaj wydarzenia z kategorią „Film"" |
| Patria (Ruda Śląska) | container renders, items = **0**; day tab says "Brak filmu" |
| Kino Ślęża (Sobótka) | 1 `div.movie`, 0 `<li>` showtimes — still the notice "Wakacyjna przerwa 🌞" |
| Studio (Opole) | break still announced verbatim: "…nieczynne… Startujemy już 3 września" |
| Kino Kuźnica | `/repertoire.html` still 302s to `/messages/noRepertoire` |
| Kino Chatka Żaka | `h3.header-light` = 0, `div.box-row` = 0, "Brak wydarzeń" |
| Kino Warszawa (Przeworsk) | MSI shell renders; **0** events for both 2026-08 and 2026-09 |
| Kino Sfinks (Kraków) | `table.widok_listy` = 0, `div.empty-results` = 1, "Brak wydarzeń" |
| ADA Kino Studyjne | biletyna `Place.events` = **0** total |
| Kino PDK (Pyrzyce) | 0 `ScreeningEvent` of 2 (1 Comedy, 1 Theater) |
| Miejskie Centrum Kultury | 0 `ScreeningEvent` of 4 (3 Music, 1 Comedy) |
| Kino nad Wartą (Koło) | bilety24 `Film:` = 0 (3 Koncert, 1 Spektakl) |
| Kino Wisła Brzeszcze | bilety24 "Brak wydarzeń" |
| Piast (Ostrzeszów) | bilety24 `Film:` = 0 of 9 (5 Koncert, 2 Spektakl, 1 Kabaret, 1 Warsztat) |
| Kino CK Lublin | 1 item tagged Film — improv theatre over a projection, not a screening |
| DKF Politechnika | Filmweb 1645 `[]` on 08-08 / 08-10 / 08-14 — academic break |
| Kino Zachęta (Kleczew) | Filmweb 2405 `[]` on the same three dates |

**Two `unfixable` verdicts stand unchanged and should not be re-opened:** Kino
Zachęta (the venue publishes only a JPEG poster; there is nowhere to repoint —
see 2026-07-31) and Kino Sfinks's dormant half (no film-row markup renders
anywhere, so the parser cannot be rebuilt or test-backed — though the *blind
spot* was closed at @73f19c8a5, and that guard is still correctly silent).

**Studio (Opole)** remains the cleanest scheduled recovery: the soft-404 trap was
already defused at @57429179a, so when the break ends on **3 September** the
client will follow the live page on its own. Nothing owed before then.

### Next run's re-check list

1. ~~**Kozienicki Dom Kultury**~~ — **done in-run: green in prod at the 04:30 UTC
   bucket.** Nothing owed. (Worth a glance only if it ever goes red again — that
   would mean the venue finally renewed its certificate *and* started redirecting
   HTTP to HTTPS, at which point flip the scheme back.)
2. **Jaworzyna** — expected to repopulate on its own around **18.08**. If it is
   still white after 20.08, that IS drift and deserves a fresh look.
3. **Kino MDK** — after 31.08, per the trigger above.
4. ~~**Kino Zamek**~~ — **done in-run: green in prod at the 07:30 UTC bucket, with
   all four open-air nights projected.** What is still owed is a **September**
   re-check, when the category switches back to one page per film. That shape is
   covered by the `kino-zamek-season` fixture, but September will be the first
   time it runs against a LIVE in-season listing — so confirm the film count
   jumps from ~4 to ~30 rather than staying flat.
5. **Studio (Opole)** and **DKF Politechnika** — after 3 September / the start of
   the academic year respectively.

---

## 2026-08-04

**Poland only, and that is not a shortcut** — `kinowo_de` and `kinowo_uk` both
hold **zero** `uptimeBuckets` because their workers stopped on 2026-08-02 and
the collection's retention is ~24 h. There is no DE/UK uptime signal to read at
all this run; both countries' `web_movies` still hold their last projection
(1,239 / 1,703 rows). PL: **21 white, 1 red**, newest bucket 2026-08-04 06:00
UTC, out of 349 services.

**One real bug found and fixed — artKino (Krosno), `fixed` @a56453c4b.** Of the
five venues new to the white set this run, one was a genuine parser break and
four are genuinely un-programmed.

**Set changes vs 2026-07-31 (18 white then, 21 now):**
- **FELL OFF (1):** Cyfrowe Kino (Środa Śląska) — green in every one of its last
  24 buckets. Recovered on its own.
- **NEW (5):** **artKino** (real bug, fixed), **Kino MDK** (Radomsko),
  **Miejskie Centrum Kultury** (Aleksandrów Kujawski), **Piast** (Ostrzeszów),
  **Kino Ślęża** (Sobótka). All five belong to cities added in the recent
  8-city expansion, so none of them appears in any earlier entry in this log —
  they are new to the roster, not newly broken.
- **Carried over (16):** unchanged from last run's list.

### artKino (Krosno) — `fixed` @a56453c4b

`ArtKinoKrosnoClient`, own-site scraper on
`artkino.rckp.krosno.pl/strona-375-repertuar.html`. **White through its entire
retained bucket history (24/24 zero) while the page was fully programmed** —
34 screenings across 12 day headers, 2–13 August.

Root cause is markup drift, and specifically drift in the ONE structural
assumption the parser made. It read the time out of the anchor's immediately
preceding text node, which is exactly how the page used to render a line:

```html
<br/>14:15 - <a href="/wydarzenie-…">TOY STORY 5</a>
```

The venue has since restyled every line so the time sits in its own coloured
span, and the anchor is frequently buried several spans deeper still:

```html
<span style="color:#993300;"><span style="color:#000000;">13:45 -</span> <a href="…">PUCIO</a></span>
<span style="color:#993300;"><span style="color:#000000;">15:00 -</span>&nbsp;<span class="filmInfo__info …"><span itemprop="name"><span style="color:#993300;"><a href="…">…</a>
```

The anchor's previous sibling is now a bare space (or nothing at all), so
`timeBefore` returned `None` for all 34 links and the client emitted an empty
list — a silent zero, not an error, hence white rather than red.

**Fix:** walk the day's `<p>` in document order and pair each film anchor with
the most recent `HH:MM` seen before it, instead of reaching for one specific
sibling. That reads both shapes. The time is *consumed* by the anchor it pairs
with, so an anchor with no time of its own still yields nothing rather than
inheriting the previous line's — the conservative behaviour the old code had.

**Second defect, same page, found while testing:** one day header read
`4 sieprnia (wtorek)` — a hand-typed transposition of "sierpnia". The exact-match
month lookup dropped that whole day (3 screenings). The month word now falls
back to its first three letters via the existing `ScraperParse.polishMonthAbbrev`;
the three-letter prefixes are unambiguous across all twelve Polish months.

**Tests:** `ArtKinoKrosnoClientSpec` now replays TWO recorded captures — the new
`art-kino-krosno` (today, restyled) and the previous capture kept as
`art-kino-krosno-plain-time-lines` (2026-06-23, flat lines), so the parser is
held to both shapes. Fail-before / pass-after confirmed: against the new
capture the old parser returned `List()` and 5 of 6 tests failed; after the fix
all 6 pass. `sbt testUnit` green — 4,408 tests, 0 failures.

No snapshot layer shifted: the e2e corpus stores already-parsed scrape *records*
(`cinema-scrapes-pl.json.gz`), and the raw-HTML replay dir `08-06-2026` has no
`artkino.rckp.krosno.pl` host, so no artKino path feeds `expected-schedules.txt`,
`read-model-snapshot.json` or the rendered HTML.

### Kino MDK (Radomsko) — `intentionally-dormant`

`Bilety24OrganizerClient` on
`bilety24.pl/kino/organizator/miejski-dom-kultury-w-radomsku-1546`. The one
venue this run that went white *during* the retained window — green until the
**2026-08-03 17:15 UTC** bucket, white since. Worth writing down how it was
settled, because the first read of it was wrong:

- Organiser page: `Film:` = **0** (37 Koncert, 20 Spektakl, 2 Wydarzenie,
  2 Wystawa). So the source really is empty of films.
- The venue's own site `mdkradomsko.pl/kino-radomsko/` **looks** like a live
  film programme — VAIANA, ZWIERZOGRÓD 2, MŁODY WASZYNGTON, plus Wajda /
  Fellini / Konwicki retrospectives, with July-2026 poster uploads. That reads
  as "our source went empty while the venue is screening", i.e. the classic
  move-to-own-site case.
- **It isn't.** Every one of those cards links to a `pec-events` detail page
  and **all three film pages carry "TO WYDARZENIE JUŻ SIĘ ODBYŁO"** ("this
  event has already taken place"). The posters are stale; the films finished.
- The venue also runs its own bilety24 storefront, `mdkradomsko.bilety24.pl`,
  whose *Repertuar* page lists three theatre pieces (MATKA, MATKA ODCHODZI,
  STARA KOBIETA WYSIADUJE) and **no films**.

Two independent surfaces plus the detail pages agree: the summer film run ended
on 3 August and nothing has replaced it yet. Our parser is right. **Re-check
next run** — this is the venue most likely to come back on its own, and if it
returns films on the storefront but NOT on the central organiser page, that is
the trigger to repoint the client.

### Miejskie Centrum Kultury (Aleksandrów Kujawski) — `intentionally-dormant`

`BiletynaClient` on `biletyna.pl/Aleksandrow-Kujawski/Miejskie-Centrum-Kultury`.
The JSON-LD `Place.events` array holds exactly 4 entries, **0** of them
`ScreeningEvent`: 3 × `MusicEvent` + 1 × `ComedyEvent` (Czerwone Gitary, Paweł
Stasiak, Kabaret Chyba, "Zaduszki Muzyczne"), dated 2026-09-12 → 2027-01-09.
The venue's own `mckaleksandrowkujawski.pl` embeds the same biletyna widget and
shows the identical non-film lineup. Not a cinema-shaped programme at all right
now.

### Piast (Ostrzeszów) — `intentionally-dormant`

`Bilety24OrganizerClient` on
`bilety24.pl/kino/organizator/kino-piast-w-ostrzeszowie-601`: `Film:` = **0**
(31 Koncert, 8 Spektakl, 3 Wydarzenie, 1 Wystawa). The venue's own domain
`ock.ostrzeszow.pl` redirects to `ock-ostrzeszow.bilety24.pl`, which
independently shows 3 concerts and no films — its entire web presence is that
storefront, so there is no third surface to contradict the two.

**Latent heads-up (not a bug today):** our organiser URL 301-redirects —
`kino-piast-w-ostrzeszowie-601` → `ostrzeszowskie-centrum-kultury-601`. Same
numeric id, redirect followed, fetch succeeds, so nothing is broken and there is
no fail-before behaviour to test. But the venue has renamed itself away from
"Kino Piast" in bilety24's own slug, which is the shape that bit Helios before.
If bilety24 ever stops honouring the old slug this goes red, and the fix is to
update the URL in `CinemaScraperCatalog`.
**→ DONE 2026-08-04.** Both renamed organisers are now addressed by the slug
bilety24 publishes today — Piast and, found the same way, **Kino Wisła
Brzeszcze** (`kino-wisla-w-brzeszczach-1539` →
`osrodek-kultury-w-brzeszczach-1539`). "No fail-before behaviour to test" was
true of the *scrape* (the redirect is followed, so both sides parse identically)
but not of the *wiring*: the catalog assertion in `CinemaScraperCatalogSpec`
fails on the old slug and passes on the new one. The recorded corpus fixtures
were renamed with it — they are keyed by request path, so changing the URL
without moving them would have silently dropped both venues from
`expected-schedules.txt`, which is the real risk in this change and the reason
the e2e spec is the guard that matters.

### Kino Ślęża (Sobótka) — `intentionally-dormant`

`KinoSlezaClient` on `rcks.pl/kino-sleza/repertuar/` — this IS the venue's own
official site, so there is no second source to check. Exactly one `div.movie`
block, and it is not a film: it is titled "Wakacyjna przerwa 🌞" and reads *"Nasze
kino robi krótką wakacyjną przerwę… Do zobaczenia już wkrótce w Kinie Ślęża!"*.
Its `<h6>Seans:</h6>` has no `<ul><li>` beneath it, so `parseShowtimes` returns
empty and the `showtimes.nonEmpty` filter correctly drops the block. Parser
working as designed on a venue on summer break.

### PL out-of-scope RED (not white — fetch failure, different mode)

- **Wybrzeże** — 3-scrape-failing for the **fifth** run running (07-21, 07-24,
  07-28, 07-31, 08-04): `CircuitOpenException: circuit open for
  bilety.rck.kolobrzeg.pl`, behind an `SSLHandshakeException
  (certificate_expired)` at the source. The breaker is doing its job. Nothing
  fixable from our side — the cinema must renew its certificate.
  **needs-human, five runs old: decide whether to retire the venue rather than
  keep retrying it indefinitely.**
  **→ RESOLVED 2026-08-04, and the venue is NOT retired.** "Nothing fixable from
  our side" was one assumption short: the portal also answers on plain HTTP —
  200, the byte-identical month page, no redirect to HTTPS — so the expired
  certificate is only load-bearing if we insist on the `https://` scheme. The
  catalog now wires `http://bilety.rck.kolobrzeg.pl`, the same way the other
  own-site venues that never offered working TLS (Kino Moskwa, Kino Sokół, Kino
  Bułgarska) have always been wired. Worth generalising next time a host goes red
  on TLS: probe `http://` before writing the venue off, because a cert that
  expires is a *scheme* problem, not a reachability one. (No CA bundling could
  have helped — `TlsTrust` already carries the `home pl DV TLS G2 R35 CA`
  intermediate that issued this leaf; an expired leaf fails validation regardless
  of anchors.)

### Re-sweep the same day, 07:45 UTC — independent confirmation, no new bug

The morning run above was interrupted mid-merge, so a second sweep was run from
scratch rather than trusting its log: the white set was rebuilt from prod
`uptimeBuckets` without reading the entry above, then every venue in it was
re-fetched live. Both halves reproduce the morning's findings exactly, and the
two fixes that landed between the sweeps are confirmed working in prod.

**PL: 20 white, 0 red**, out of 350 services (310 after dropping `*|enrichment`
and `img:` rows), newest bucket 2026-08-04 07:45 UTC. Against the morning's
**21 white / 1 red**, both deltas are the fixes:

- **artKino — the fix works.** Zero for its entire retained history through the
  06:15 bucket, then **green at 07:15** — the first scrape after @a56453c4b
  deployed. It has left the white set.
- **Wybrzeże — the red is gone**, and the venue did not have to be retired
  (@b51d129a9, plain HTTP). The RED set is now **empty**, the first run in five
  with nothing failing at fetch.

**Only one venue changed state inside the retained window**, and it is the one
the morning run already singled out: **Kino MDK** (green ×10, then white from
the 2026-08-03 17:15 UTC bucket). The other 19 are white across every retained
bucket — long-dormant, not newly broken. That distribution is itself the
evidence that no parser broke recently: markup drift lands as a green→white
transition, and there is exactly one, already diagnosed.

**All 20 sources re-fetched live: every one returned HTTP 200, and none carries
a film programme.** The verdicts are not inferred from our own parser's silence
— five venues say so in their own words on the page we scrape:

| Venue | The source's own words |
|---|---|
| Patria | `Brak filmu` on every day 04.08 → 10.08 |
| Kino Świt | `Brak nadchodzących seansów filmowych.` |
| Kino Ślęża | `Wakacyjna przerwa 🌞` |
| Studio (Opole) | `W czasie wakacji nasze kino jest nieczynne… Startujemy już 3 września` |
| Kino MDK | organiser listing is `MATKA ODCHODZI` + symphonic concerts, no film |

The rest agree by structure rather than by wording: all four biletyna venues
carry **zero** `ScreeningEvent` entries — Kino PDK against 1 `TheaterEvent` + 1
`ComedyEvent`, Kino Tur against 1 `ComedyEvent`, MCK Aleksandrów against 3
`MusicEvent` + 1 `ComedyEvent`, and ADA Kino Studyjne against no events of any
type at all (an entirely unprogrammed venue page); the bilety24
organiser pages (nad Wartą, Wisła Brzeszcze, Piast) list no film category; and
both Filmweb-backed venues return a literal `[]` from the seances API on **every
one of the next 15 days** — `cinema/1645` (DKF Politechnika) and `cinema/2405`
(Kino Zachęta), the static ids the catalog wires. For the university film club
that is the academic summer break already logged on 2026-06-28.

#### Kino Zachęta (Kleczew) — `intentionally-dormant`, and the usual fix is foreclosed

Worth its own note, because this is the venue in the set that best fits the
"Filmweb went silently empty → move it to its own site" pattern, and the fix
does not exist here. Three independent checks:

- **Filmweb agrees it is empty, and says so in words, not by omission.** The
  cinema's own showtimes page renders *"Niestety to kino nie oferuje seansów w
  najbliższym czasie"*. So `[]` is Filmweb reporting no programme, not Filmweb
  having dropped the venue — the page and the id (2405) both still resolve.
- **The venue is also absent from Filmweb's `/showtimes/Kleczew` city listing**
  (6 cinemas, none of them Zachęta). Harmless today because the catalog wires
  the id statically, but it means `FilmwebCinemaIdResolver` could not re-derive
  this id if the static one ever had to be replaced.
- **Its own site cannot be scraped instead.** `bckkleczew.pl/repertuar.html`
  publishes the repertoire as a single JPEG (`od2407.jpg` — the "from 24.07"
  poster) with no HTML showtimes anywhere on the page, and the host 403s any
  non-browser user-agent. There is no own-site scraper to move to; reading it
  would need OCR. The one local-news mirror (`hejkleczew.pl`) last updated in
  **December 2025**, so it is not a source either.

Do not re-open this one as "Filmweb went empty, repoint it" on a future run —
the empty result is correct, and there is nowhere to repoint it to. If the venue
does go dark permanently, retiring it is the only option, not a new client.

#### Kino Sfinks — the calendar is still empty, but the blind spot is `fixed` @73f19c8a5

Re-checked the 2026-07-11 `needs-human` in full: **every** per-day page
`/wydarzenia-2026-08-04…20.html`, the month page, all seven
`wydarzenia-kategoria-*.html` pages and the unfiltered listing render
`div.empty-results` → "Brak wydarzeń", 0 rows. (The film titles now visible on
the harmonogram — VINCI 2, KOMPLETNIE NIEZNANY — are ZAPOWIEDZI teaser panels
carrying no date or time, not screenings.) So the original verdict stands: with
no film-row markup rendered anywhere, the parser still cannot be rebuilt or
test-backed, and that half remains **needs-human**.

But re-reading `KinoSfinksClient` while confirming that turned up something
that *was* fixable, and is the more valuable half. The client selects
`table.widok_listy tbody tr[onclick]` — the table this site removed site-wide —
so it returns an empty list for **two different reasons at once**: the venue is
dormant, AND we can no longer read the page. Both painted white, which is
precisely why the CMS migration went unnoticed for five runs: a silent zero is
indistinguishable from a dormant venue, so nothing ever escalated.

The fix does not guess at the new markup. It only refuses to call a parse
"empty" unless the page *accounts* for being empty — either it rendered the
listing table (zero films is then a real category-filter result) or it rendered
the CMS's own `empty-results` marker. A page with neither is a failed scrape,
not an empty one, and throws. Same guard as `KinoStudioClient` / `MsiClient` /
`KinoAwangarda2Client` / `KinoPatriaClient`.

Net effect: **nothing changes today** — the marker is present, so Sfinks stays
correctly white — and the day the venue repopulates into markup this parser
cannot read, it goes **red** instead of staying white forever. That is the day
the parser becomes rebuildable, and it will now announce itself instead of
having to be rediscovered by a future sweep.

**Tests:** two real captures recorded — `kino-sfinks-empty-calendar` (today's
empty harmonogram → must return empty, must NOT throw) and
`kino-sfinks-shape-drift` (today's site root standing in for the schedule URL
serving something that isn't the schedule, the Helios slug-rename shape → must
throw). Fail-before/pass-after confirmed: the drift test failed with "no
exception was thrown" before the change. `sbt testUnit` green (4,410 tests) and
`FilmScheduleEndToEndSpec` green. No snapshot layer shifted — all six Sfinks
fixtures in the `08-06-2026` raw-HTML corpus carry `widok_listy`, so the guard
never fires there and the pipeline's output is byte-identical.

**Kino Zamek (Szczecin) re-checked and still not actionable.** Its MSI page is
the liveliest of the twenty — 8 distinct times, 113 KB — but August is `lato na
tarasach` concerts, `CZERWONY KAPTUREK` for children and theatre. The venue
publishes a `Kino` subcategory filter and currently has nothing under it, so the
standing festival filter-gap `needs-human` cannot be reproduced, let alone
tested, until it programmes films again.

**Unrelated oddity, logged not chased:** `mdk.opole.pl/kino-studio.html` now
carries injected Russian casino spam in its body text ("…самоуверенных
хайроллеров"). It is on the venue's page, not ours, and `KinoStudioClient` reads
only structured showtime blocks so nothing leaks into our titles today. Worth
remembering if that venue ever starts emitting nonsense film titles — the page
is compromised, not the parser.

**Verdict: no white venue is white because of a parser bug** — every one is
film-dormant at source, and the one recent transition is a repertoire ending,
not drift. **One code change did come out of the sweep** (`fixed` @73f19c8a5,
Kino Sfinks): not a venue restored, but a blind spot closed, which is what a
re-read of the longest-standing `needs-human` turned up. See its section above. The
next run's re-check list is unchanged: **Kino MDK** first (most likely to
return, and if it returns films on `mdkradomsko.bilety24.pl` but not on the
central organiser page, that is the trigger to repoint the client), then
**Studio** after 3 Sept and **DKF Politechnika** when the academic year starts.

---

## 2026-07-31

**All three countries swept.** Totals: **PL 18 white / 2 red**, **DE 167 white /
4 red (of 1,538)**, **UK 57 white / 0 red (of 848)**. Newest bucket everywhere
2026-07-31 07:30 UTC. **One real bug found and fixed — KinoPort (Gdańsk),
`fixed` @385cfd9b5.** Discovery method unchanged (`/uptime` auth-gated): a mongosh
replay of `UptimeController`'s predicate over prod `uptimeBuckets` via the
running `flyctl proxy` on `127.0.0.1:27017`, iterated over `kinowo` /
`kinowo_de` / `kinowo_uk` with `db.getSiblingDB`.

**The headline is that both of the last run's open verification items closed
cleanly, and the two remaining big cohorts (DE open-air, UK Flicks) were
re-tested adversarially and are confirmed NOT our bug.**

> **A SECOND PASS followed the first** (same day, on request: "fix everything
> outstanding"). It cleared six of the items this run had logged as follow-ups or
> needs-human, including a **second real white-bar bug — The Old Court Windsor
> was reading a different venue's page.** See **"Follow-up pass"** at the end of
> this entry; the sections above are the original investigation record, with
> `→ fixed in the follow-up pass` markers where they were later actioned.
>
> **Both white-bar fixes are CONFIRMED GREEN in prod** (KinoPort 09:30 UTC,
> The Old Court 10:30 UTC) — no verification is owed to the next run.

### ✅ Verification owed from 2026-07-28: the UK Flicks span-button fix WORKS

@d618f3a86 (match the session button by CLASS, not tag, so `<span>`-only
unbookable venues still yield showtimes) was shipped but unobserved at last
hand-off. **All four target venues have gone GREEN** — Barn Cinema Dartington,
Broadway Cinema Villa Marina, Watersmeet and Cube Cinema Bristol are all absent
from today's UK white set, and all four still advertise day tabs on Flicks
(7 / 7 / 3 / 4 `data-date` values respectively). Neuadd Dwyfor Pwllheli (last
run's un-probed transient) also fell off. **Confirmed end-to-end; close this
item.**

### PL — 18 white (was 20)

**Set changes vs 2026-07-28:**
- **FELL OFF (4):** Kino Cytadela, Kino MOK Nowa Ruda, Kino ŚDK, Kozienicki Dom
  Kultury. (Cytadela had been flagged "escalate if still white in 2+ runs" — it
  recovered instead, so that escalation is void.)
- **NEW (2):** **KinoPort** — a real bug, **`fixed`** (below). **Kino Zachęta** —
  `unfixable` (below).
- **Carried over (16, all re-probed live this run, all still dormant):** ADA Kino
  Studyjne, Cyfrowe Kino, DKF Politechnika, Kino CK Lublin, Kino Chatka Żaka,
  Kino Kuźnica, Kino PDK, Kino Sfinks (needs-human), Kino Tur, Kino Warszawa
  (Przeworsk), Kino Wisła Brzeszcze, Kino Zamek (needs-human), Kino nad Wartą,
  Kino Świt, Patria, Studio (Opole). **None recovered-but-broken** — every
  parser is correctly reporting an empty venue. Evidence per venue in the
  carried-over section below.

### KinoPort (GCSW, formerly CSW Łaźnia, Gdańsk) — `fixed` @385cfd9b5

- Old client: `FilmwebShowtimesClient(1735)`. **Classic "Filmweb went silently
  empty for a small venue".** `/api/v1/cinema/1735/seances?date=…` returns
  exactly `[]` (2 bytes) for all seven dates 2026-07-31 → 08-06, while
  `/api/v1/cinema/1735/info` still resolves correctly to
  `{"name":"KinoPort","city":"Gdańsk","street":"Strajku Dokerów 5"}` — the id is
  right, the feed is abandoned. The e2e corpus had the same rot baked in: all
  seven `08-06-2026/www.filmweb.pl/api/v1/cinema/1735/seances.*` fixtures were
  `[]`, so the white bar was reproducible offline.
- Meanwhile the venue is screening **five films a day**. `laznia.pl/kinoport/
  repertuar/` is now only a pointer ("Obecnie informacje nt. kina i bieżącego
  repertuaru znajdziecie na: www.gcsw.pl"). GCSW is WordPress + Elementor and
  publishes the entire repertoire as ONE post under the `kino` category.
- **The permalink rotates** (`/2026/07/22/repertuar-kinoport-gdansk/`), so the
  client reads the stable WP REST route `**/wp-json/wp/v2/posts?categories=49**`
  and takes `content.rendered`. (Checked the alternatives: `/category/kino/`,
  `/kinoport/` and `/kino/` all 404, and `/repertuar/` 302s to whichever post the
  alias was last pointed at — today a stale June one. So the `sourceUrl` /uptime
  link is GCSW's homepage, which does link the live post.)
- Markup: `<h3>…Lipiec 2026</h3>` month header (**year optional** — the next
  header reads just "Sierpień"), `<h4>30.07 (czwartek)</h4>` day, one `<p>` per
  screening as `<strong>18:00 – Title</strong> (72′)<br><em>2026, reż.
  Director</em>`. The `′` is U+2032 PRIME, not an apostrophe.
- **The trap that would have silently doubled the venue's listing:** the same
  post continues below with an Elementor accordion titled **"ARCHIWALNE
  SEANSE"** holding identically-shaped PAST screenings (2–25 July), in a second
  long-form day format ("2 lipca (czwartek)"). The client drops any `<details>`
  whose `<summary>` says ARCHIWALNE — structural, not a text offset — and the
  spec pins that "Truposz" / "Mystery Train" (archive-only titles) never appear
  and the earliest showtime is 2026-07-30 18:00.
- Result: **23 films / 42 showtimes**, 2026-07-30 → 08-21, with runtime, release
  year and director read off each paragraph. No booking deep-links exist (box
  office only), so `Showtime.bookingUrl` is `None` by design.
- **Fail-before / pass-after, at three layers.** (1) New `KinoPortClientSpec`,
  8 cases over a real 2026-07-31 recorded fixture. (2) `CinemaScraperCatalogSpec`
  had an explicit guard *"scrape KinoPort off Filmweb, not the retired gcsw.pl
  alias"* — inverted to assert `gcsw.pl` and NOT `www.filmweb.pl`. (3) The e2e
  corpus: `expected-schedules.txt` went from **0 to 8** KinoPort lines.
  `CinemaClientMarkersSpec`'s Filmweb examples moved to Kino Zachęta (still
  Filmweb-backed) and its KinoPort `sourceUrl` assertion now pins GCSW.
- Snapshots: `read-model-snapshot.json` (+502 lines) and `expected-schedules.txt`
  regenerated and re-run stable. **`expected-*.html` did NOT shift** — Gdańsk is
  not one of the three snapshot cities and none of KinoPort's art-house slate
  displaced a poster/source tail in Poznań / Wrocław / Warszawa / `/plan`.
- Dead code removed in the same commit: the seven `[]` Filmweb-1735 corpus
  fixtures.

### Kino Zachęta (Kleczew) — `unfixable: the venue publishes its repertoire only as a JPEG`

- Client: `FilmwebShowtimesClient(2405)`. Filmweb is genuinely empty — `[]` for
  all seven dates 07-31 → 08-06 — and `/info` confirms the id still maps to
  `{"name":"Kino Zachęta","city":"Kleczew","street":"Al. 600-lecia 33"}`. So this
  is the same Filmweb-went-empty shape as KinoPort.
- **But there is nothing to repoint to.** The operator is Biblioteka-Centrum
  Kultury; `bckkleczew.pl/repertuar.html` (linked from the town site
  `kleczew.pl/strona-2573-kino_zacheta.html`) has a content div consisting of a
  **single `<img src="files/image/od2407.jpg">`** and nothing else. Reading the
  JPEG: *Kino "Zachęta" w Kleczewie zaprasza*, columns **24.07–30.07** only —
  Ekipa Zwierzaków 13.30, Vaiana 15.00/17.00, Odyseja 19.00. `Last-Modified`
  Fri 24 Jul 2026. The venue runs a weekly Thu→Wed poster cadence, so the
  published week **ended the day before this run**; guessed successors
  (`od3107`, `od0108`, `od3007`, `od0208`.jpg) all 404.
- `/130-godziny-otwarcia.html` confirms it is an operating cinema ("Kino jest
  czynne od poniedziałku do niedzieli. Seanse … 15:30, 17:30 i 19:30"). No break
  announced (BCK's news RSS, latest item 30 Jul, has no cinema-break post).
  `kinozacheta.pl` / `zacheta.kleczew.pl` / `mgokis.kleczew.pl` /
  `gokis.kleczew.pl` all fail DNS. Zero hits for "Kleczew"/"Zachęta" on
  biletyna.pl, bilety24.pl, ekobilet.pl or systembiletowy.pl — no ticketing
  portal we already have a client for carries it.
- **No text scraper can be pointed at a JPEG**, so there is no fix a
  fail-before/pass-after test could back. The only routes are OCR of
  `bckkleczew.pl/files/image/*.jpg` (a new capability, and brittle against a
  hand-made poster) or accepting the gap. **Left on Filmweb** — if BCK ever
  restores its Filmweb feed the venue recovers for free.
  **needs-human if we want Kleczew covered: decide whether an OCR path is worth
  building for one small venue.**

### DE — 167 white (was 323), 4 red — re-tested; still an upstream coverage gap

The white count roughly halved, and what remains is **overwhelmingly open-air /
Sommerkino / Kinomobil** by name — exactly the cohort last run flagged as
under-carried by filmstarts. This run tested that hypothesis properly, including
the one scenario that would have made it OUR bug.

- **Control healthy:** `theater-A0076` returns `{"error":false}` with 100 904 /
  123 817 / 6 028 bytes of `results` across three dates.
- **Eight sampled white venues** (Landsberg `A2207`, Biesdorf `A1809`, Güstrow
  `A2933`, Freibad Göttingen `A1788`, Filmnächte am Elbufer `A1801`, Autokino
  Dillingen `A2926`, Kinomobil Löchgau `A0797`, Traumstern `A1190`): all three
  dates each, **HTTP 200, exactly 776 bytes,
  `{"error":true,"message":"no.showtime.error","nextDate":null,"results":[]}`**.
  `nextDate:null` means filmstarts has no FUTURE showtime at all, not just none
  that day. Their `/kinoprogramm/kino/<ID>/` pages are 200 with
  `"itemListElement": []` (control: 12 elements).
- **Three are provably screening on their own sites:** Filmnächte am Elbufer
  ("52 Veranstaltungen in Dresden" — Nürnberg 03.08 21:00, Marty Supreme 04.08
  21:00, Supergirl 05.08 21:00), Open Air Kino im Freibad Göttingen (12 dated
  shows ≥31.07 in `OpenAirKino_2026-WEB.pdf`), and **Traumstern Lich — note, a
  year-round arthouse, NOT open-air** ("Kinowoche: 30. Juli – 5. August 2026").
  One — Open Air Kino Landsberg — is **correctly** empty: its own site's API
  returns films for the indoor Olympia site only, the open-air site is dark.
- **The decisive new test — is it a mis-mapped id on our side? NO.** Filmstarts'
  own Dresden city page lists "Filmnächte am Elbufer" exactly once, as
  `<a href="/kinoprogramm/kino/A1801/">` with `data-theater="{"id":"A1801"…}"` —
  **the same id we use**, and no alternative exists. (`/suche/` is 410 Gone
  site-wide, hence the city-page route.) Our request URL shape matches
  `WebediaShowtimesClient` exactly.
- **Verdict: `needs-human` — an aggregator coverage gap, unchanged but now
  firmly evidenced.** Filmstarts does not carry open-air festival programmes or
  some small arthouses; those publish only on their own sites. A fix means
  bespoke own-site clients (the cohort is name-identifiable: `Autokino…`,
  `Sommerkino…`, `Open Air…`, `Kinomobil…`), which is a project, not a
  white-run change — and no fail-before/pass-after test can be written against a
  source that correctly reports what it knows.
- **DE red (4, down from 23, out of scope):** all still HTTP 404 on
  `/kinoprogramm/kino/<ID>/` — Inselkino Baltrum `G01C9`, Kino Kiste `A0743`,
  Heppel-Ettlich `A2843`, Kino Babenhausen `A2165`. Venues delisted from
  filmstarts, correctly red rather than white. They will never recover on their
  own and burn retries every cycle. (Standing item, second run running; the set
  shrank from ~11 ids to 4.) **→ fixed in the follow-up pass: all four verified
  gone for good and retired from the roster.**

### UK — 57 white (was 63), 0 red — swept all 57; **zero client bugs**

Method (same discriminator that found the span-button bug): a Flicks venue page
with a programme carries `data-date` day tabs; 200-with-zero-tabs means Flicks
holds no sessions at all. Slugs resolved against Flicks' authoritative
848-entry `sitemap-cinemas.xml` rather than guessed — 17 of the 57 differ from
the naive slug (e.g. Plaza Community Cinema Liverpool → `plaza-crosby`,
Belmont Filmhouse → `belmont-filmhouse-aberdeen`).

- **Control:** `curzon-soho` → 200, **24** `data-date` tabs. Method sound.
- **56 of 57 resolved; every one HTTP 200 with ZERO `data-date` tabs.** The pages
  are genuine, correctly-titled venue pages (~56 KB), not soft-404s or
  redirects — healthy pages run 64 KB+.
- **Venues with tabs while we recorded zero: NONE.** Last run's bug class is
  fully cleared; there is no second one hiding behind it.
- **1 unresolved:** **The Old Court Windsor** — absent from Flicks'
  `sitemap-cinemas.xml` entirely; `the-old-court-windsor`, `old-court-windsor`,
  `the-old-court` all 404 (the only Windsor entry is a different venue,
  `the-screen-cinema-windsor`). Our config points at a slug Flicks no longer
  publishes. **→ fixed in the follow-up pass: it was worse than a stale slug —
  the venue was wired to a DIFFERENT venue's page, and now has its own client.**
- **Everyman Cinema Durham** is on `GatsbyBoxOfficeClient`, not Flicks (its
  Flicks slug exists but is irrelevant); previously confirmed "closed until
  further notice". `intentionally-dormant`.
- Verdict for the other 55: **needs-human — aggregator coverage gap**, unchanged
  from 2026-07-28. Several are genuinely closed (Belmont Filmhouse shut since
  Oct 2022; Watermans Brentford closed Apr 2024), but Phoenix East Finchley, the
  ICA and ARC Stockton are unambiguously operating and simply missing from
  Flicks' backend. The only fix is per-venue own-site scrapers.

### PL out-of-scope RED (not white — fetch failure, different mode)

- **Wybrzeże** — 3-scrape-failing for the **fourth** run running (07-21, 07-24,
  07-28, 07-31): `CircuitOpenException: circuit open for
  bilety.rck.kolobrzeg.pl`, behind the TLS certificate that expired at the
  source. The breaker is behaving correctly. **needs-human — four runs old; the
  cinema must renew its cert, we cannot fix this from our side.** Worth deciding
  whether to retire the venue rather than keep retrying it indefinitely.
- **api.trakt.tv** — NEW red this run, and not a cinema: `HTTP 403` on
  `/search/movie?query=…`. An enrichment source rejecting us, not a scrape.
  Out of scope for a white run but **worth a human's eye** — a 403 (not 429)
  suggests a key/permission problem rather than throttling.
  **→ investigated in the follow-up pass; two header defects fixed, the
  credential itself left for a human.**

### Carried-over PL dormant — per-venue evidence (all re-probed live this run)

All 16 still genuinely un-programmed; parsers correct. Counts quoted are from
this run.

- **ADA Kino Studyjne** — biletyna, `ScreeningEvent`=0, "Brak wydarzeń".
- **Cyfrowe Kino (Środa Śląska)** — after stripping `<style>`/`<script>` (the
  theme's CSS names the classes), the real DOM has only the empty container
  `<div class="amy-movie-items"></div>`, **0** real items.
- **DKF Politechnika** — Filmweb 1645 `[]` for 07-31, 08-01, 08-05. Academic break.
- **Kino CK Lublin** — `ck-lublin.bilety24.pl`, `Kup bilet - Film:`=0, 2 concerts.
- **Kino Chatka Żaka** — reads `umcs.pl/pl/kalendarz-wydarzen,9469,1.lhtm`;
  `h3.header-light`=0, `div.box-row`=0, "Brak wydarzeń".
- **Kino Kuźnica** — `shd.systembiletowy.pl`; `repertoire.html` now redirects to
  `/messages/noRepertoire` ("Sprzedaż online … jeszcze się nie rozpoczęła").
- **Kino PDK (Pyrzyce)** — biletyna, `ScreeningEvent`=0; only 1 ComedyEvent +
  1 TheaterEvent.
- **Kino Sfinks (Kraków)** — `table.widok_listy`=0, `tr[onclick]`=0,
  `.empty-results`=1, "Brak wydarzeń". Calendar has **not** repopulated, so
  there is still no populated row shape to rebuild the parser against.
  **needs-human, unchanged since 2026-07-11.**
- **Kino Tur (Turek)** — biletyna `ScreeningEvent`=0 (only the ComedyEvent
  "Mariusz Kałamaga"). Last run left a trigger: migrate to the venue's own
  `mdk.turek.pl` **if** biletyna is still empty while mdk has a current monthly
  article. **The trigger has NOT fired** — mdk.turek.pl's newest repertoire
  article is still **czerwiec 2026**; `…-lipiec-2026` and `…-sierpien-2026` both
  404. Both sources are empty, so the venue is simply not programming. Keep the
  trigger armed for the next run.
- **Kino Warszawa (Przeworsk)** — real host `bilety-kino.przeworsk.um.gov.pl`;
  2026-07 and 2026-08 both 200 with an empty event list.
- **Kino Wisła Brzeszcze** — bilety24 organiser 1539: `Film:`=0; 16 Koncert,
  4 Spektakl, 2 Wydarzenie, 1 Wystawa.
- **Kino Zamek (Szczecin)** — "Nie znaleziono wydarzeń spełniających kryteria";
  only category links, **no individual film-title slugs**. Its MSI side for
  2026-08 has concerts only. Standing `needs-human` festival filter-gap,
  unchanged; likely self-resolves when normal repertoire returns.
- **Kino nad Wartą (Koło)** — bilety24 organiser 1626: `Film:`=0; 22 Koncert,
  6 Spektakl.
- **Kino Świt (Warszawa)** — `div.cks-movie-card`=1 (container only), "Brak
  nadchodzących seansów filmowych".
- **Patria (Ruda Śląska)** — day tabs 31.07–06.08 all render "Brak filmu".
- **Studio (Opole)** — break to **3 września 2026**, confirmed live. **See the
  stale-URL heads-up below.**

### Heads-up: `KinoStudioClient` reads a soft-404 → fixed in the follow-up pass

`mok.opole.pl` now 403s; the live host is **`mdk.opole.pl`**. `KinoStudioClient
.RepertoireUrl` → `mdk.opole.pl/kino-studio.html` returns **HTTP 200 but serves
the CMS 404 page** (`<title>404 - Młodzieżowy Dom Kultury w Opolu`, no
`div.ckeditor`), while the real page is `mdk.opole.pl/kino-studio-przerwa.html`
(`<title>Kino STUDIO`, has `ckeditor`, "W czasie wakacji nasze kino jest
nieczynne. Startujemy już 3 września :)").

Not fixed this run, deliberately: the venue is on a confirmed break to 3 Sept,
so the URL fix would surface **zero** films and there is no fail-before /
pass-after behaviour test to write — the bar stays white either way. It is a
latent trap though: **when the break ends on 3 September the scraper will keep
reading a soft-404 and Studio will stay white for a reason that is entirely
ours.** Follow-up: repoint the client (or make it follow whichever of the two
pages carries `div.ckeditor`) and re-record its fixture. **Best done in the
first run after 3 Sept, when a populated page exists to test against.**

### Previous run's `MsiClient` 2-month horizon cap → fixed in the follow-up pass

Unchanged and still not actioned — see the 2026-07-28 entry. It causes no white
bar; every out-of-window title measured was a concert / stand-up / theatre that
`OnlyMovieEventsFilter` would drop anyway.

### Test-infra note for the next run: how to run `itAll` locally here

Worth 10 minutes of a future run's time. A fresh auto worktree has **no
`.env.local`**, and `itAll` is only `addCommandAlias("itAll", "all
web/IntegrationTest/test worker/IntegrationTest/test")` — it sets nothing up. So
a bare `sbt itAll` aborts every Mongo-backed spec with *"MONGODB_URI not set"*.
Run it as:

```
MONGODB_URI="mongodb://127.0.0.1:28017/?directConnection=true" \
MONGODB_DB="kinowo_it_<something-unique>" \
TMDB_API_KEY=$(grep '^TMDB_API_KEY=' /Users/pawel/projects/movies/.env.local | cut -d= -f2-) \
sbt itAll
```

**:28017 is the local brew replica set, NOT :27017** — that is the `flyctl proxy`
to prod. And **always pass a unique `MONGODB_DB`**: the specs default to
`"kinowo"`, which on the local instance is full of residue from previous runs
(that residue alone caused two spurious failures this run).

The per-suite corpus databases now clean themselves up: a whole-corpus spec takes
its `<MONGODB_DB>_<suite>` through `IntegrationCorpusDatabase.withDatabase`, which
drops it when the scope closes, and `WorkerWiringNormalizerIntegrationSpec` drops
its `kinowo_it_wiring_*` in `afterAll`. So a run leaves behind only the base
`MONGODB_DB` you named — that one is SHARED by every spec in both modules (`itAll`
runs web and worker in parallel), so nothing drops it for you.

Mid-run this layer also failed on `integration.StagingFoldIntegrationSpec` →
*"should keep a retired key's screenings"* (`BsonInvalidOperationException:
Missing field: updatedAt`) while passing when run alone. That was **not** this
run's change — confirmed by running the same suite in a throwaway worktree at
`origin/main`, which failed identically — and it has since been **fixed on main
by someone else at @5cf7ce873** (a sibling spec was seeding an `updatedAt`-less
row into a shared `pending_movies`; it now gets its own isolated database). The
transferable lesson, which cost time here and there: **a spec passing alone says
nothing about the suite** — when the it/ layer goes red, check `origin/main`
before assuming it is yours.

---

## 2026-07-31 — Follow-up pass ("fix everything outstanding")

A second pass over the same run's backlog, on request. **Six items cleared,
including a second real white-bar bug.** Everything below is one commit,
`@57429179a`, with all layers green locally (`testUnit` 4,279 · `itAll` 12 + 111 ·
`PageTest` 172); neither snapshot layer shifted.

### The Old Court Windsor — `fixed` — **we were scraping the wrong venue**

Logged above as "needs-human: a slug Flicks no longer publishes". That was too
generous to us. The catalog line read
`flicks("the-screen-cinema-windsor", TheOldCourtWindsor)` — **The Screen Cinema
Windsor is a different venue.** The Old Court appears nowhere in Flicks' 848-entry
`sitemap-cinemas.xml` (no `oldcourt` slug exists at all), so this venue was
pointed at a neighbour's page and had scraped to zero for as long as it has been
wired. A white bar caused entirely by our own mapping — not the venue, not the
aggregator.

Its own site carries the programme, so a new `TheOldCourtClient` reads
`oldcourt.org.uk/events`: **12 films / 15 showtimes**, 2026-08-07 → 11-26.
Three things worth knowing before touching it again:

- **The page has NO class names** (it is styled with CSS `@scope`). Every hook is
  structural — `a[href^=/event/]` for the event block, child-walking for the title.
- **jsoup's `select` matches the ROOT element too**, so `block.selectFirst("div")`
  returns the block itself and any descendant query lands on the wrapper, whose
  text is the title with every booking line glued on. Cost two recording rounds;
  the client now walks `children` explicitly.
- **What makes an event a film is the booking path**, `/sales/the-old-court-cinema/`
  — not the title. The listing is one flat stream of 42 events mixing in quiz
  nights, DJ sets, sound baths and ballet. "Rocky Horror Night" and "Disclosure
  Day" ARE films here; "Alice In Wonderland - The Ballet" is not.
- Dates are prose with **no year** ("Fri 7th Aug 20:30-21:15"), inferred forward
  from `today`; the spec pins the December→next-August rollover.
- Programme-strand suffixes are stripped: "Tuner (The Old Courters)" (the seniors'
  matinee) and "Tuner" are one film on two days, and would otherwise render as two
  near-duplicate cards. Same for "Backrooms" and "(Independent film)".

Six-case `TheOldCourtClientSpec` over a recorded 2026-07-31 fixture.

### Kino Studio Opole — `fixed` — the soft-404 trap, defused before it bites

Flagged above as "do this in the first run after 3 Sept". Doing it now instead,
because the fix is test-backable today and the failure mode is worse than logged:
`KinoStudioClient.parse` fell back to scanning the **whole `<body>`** when the CMS
content div was absent, so a soft-404 fed the site's nav and footer to the
date/title state machine — which can only manufacture junk films, never recover
real ones.

`mdk.opole.pl/kino-studio.html` returns **HTTP 200 with the site's "Błąd 404"
body** (no `div.ckeditor`), while the live page is `kino-studio-przerwa.html`.
The status code cannot tell the dead slug from the live one; the content div can.
The client now tries both slugs in order, takes the first that renders content
(lazily, so a healthy in-season page still costs one request), and — if NEITHER
does — **throws**, so a dead source surfaces RED rather than white. Same guard as
`MsiClient` / `KinoAwangarda2Client` / `KinoPatriaClient`.

Three new cases over three recorded fixtures: soft-404 → falls through and finds
the films; a real break page → zero films and NO throw (a dormant venue is
correctly white); both dead → throws.

### `MsiClient`'s 2-month horizon cap — `fixed`

Carried since 2026-07-28. `fetchUnfiltered` fetched exactly `YearMonth.from(today)`
and `.plusMonths(1)` across all ~12 MSI venues, which `ScrapeHorizon`
(`MaxDays = 730`, "we want ALL future screenings") forbids. It now walks forward a
month at a time, bounded by the horizon, stopping after `MaxEmptyMonths = 3`
consecutive months that yielded nothing.

**Three, not two** — deliberately. MSI venues publish one to two months ahead, so a
threshold of two cannot see a programme resuming after a two-month summer gap,
which is exactly where half these venues are right now. A dormant venue still
costs three requests. A month that fails to fetch counts as empty for the stop
rule: we cannot tell a missing month from a quiet one, and treating failures as
"keep going" would walk the full two years on every blip. The total-outage guard
is unchanged in meaning — if EVERY attempted month failed, propagate.

Test serves the recorded month page ONLY at today+2, which the old client never
requested; plus a request-counting fetch pinning that the walk still stops cheaply.

### Four delisted German venues — `fixed` (retired)

The DE red set (`A0743` Kino Kiste, `G01C9` Inselkino Baltrum, `A2843`
Heppel - Ettlich, `A2165` Kino Babenhausen), 404ing for weeks and burning retries
every cycle. Checked against Filmstarts' own exhaustive city/state listings —
**none has been re-issued under a new id**, and the control proves 404 means
deletion rather than dormancy: a live-but-idle venue (`A1809` Freiluftkino
Parkbühne Biesdorf) returns HTTP 200 with `no.showtime.error`, never 404.

Independently confirmed each is really gone as a cinema: Kino Kiste closed
31.12.2025; Baltrum has no operator (the Gemeinde is advertising for one);
Heppel - Ettlich is open but as a Kleinkunst stage; Babenhausen's hall runs
theatre only. Removed from `data/germany/regions.json` and `GermanRosterData`
(roster 1,533 → **1,529**, and every count/comment quoting 1,533 updated with it).

**Trap recorded for whoever regenerates the roster:** do NOT re-point Heppel to
`A1575` "Neues Rottmann". Munich's city page does list that id, but it is a
separate operating cinema at Rottmannstr. 15, not a rename. `CountrySpec` now
fails if any of the four ids returns to the roster.

### `api.trakt.tv` 403 — partly fixed, credential left for a human

Two real defects found, one fixed, and the likely root cause identified but not
ours to change.

- **`RealHttpFetch` was sending duplicate headers.** `buildRequest` applied
  caller-supplied headers with `HttpRequest.Builder.header`, which **APPENDS** —
  so a caller overriding a default got BOTH values sent. `WikidataClient` exists
  precisely to satisfy Wikimedia's "identify yourself" policy and was shipping its
  polite UA *alongside* a Chrome string. Now `setHeader`. This is a general fix,
  not a Trakt one; `RealHttpFetchSpec` pins it (and it fails on the old code).
- **`TraktClient` sent two of the four headers Trakt documents as required.** No
  `Content-Type: application/json` (a 412 case) and no `User-Agent`, so it
  inherited `RealHttpFetch`'s Chrome UA — a browser string on a JSON API, exactly
  the shape Trakt's Cloudflare bot rules (announced Dec 2025, and confirmed on
  their forum as "experimenting with some firewall rules to block bot traffic")
  target. It now identifies as `kinowo/1.0`.
- **The 403 itself is most likely the credential.** Trakt's status table is
  explicit: `403 = "Forbidden - invalid API key or unapproved app"`, while rate
  limiting is 429, a missing content type is 412 and VIP-gating is 426. Search is
  still public (no OAuth), and no announcement revokes keys. **The discriminator
  is the response content-type**: Trakt's own app layer returns a 9-byte JSON
  `Forbidden`, whereas Cloudflare returns an HTML interstitial. **needs-human:
  check the app at `trakt.tv/oauth/applications` and whether
  `TRAKT_API_CLIENT_ID` is still valid.** The header fixes remove the other
  variables; they are not claimed to fix the 403.

Worth noting separately: `TraktClient.fetch` swallows every failure into `"[]"`,
so an invalid key is indistinguishable from "no match" at the call site. Not
changed here (it would ripple through the enrichment ladder), but it is why this
took an external investigation to diagnose rather than a log line.

#### Correction + how much Trakt is actually worth (measured after the fact)

**Correcting the diagnosis above.** I wrote that the 403 "most likely means the
client_id is rejected". The uptime history undercuts that. Trakt's 24h bucket
window is a **clean cutover, not a decay**: 40 successes and ZERO failures
through **2026-07-30 16:15 UTC**, then 97 failures and zero successes from 18:30
onward, with no mixed bucket anywhere. A credential that worked yesterday
afternoon and stopped dead is as consistent with a WAF/IP block as with a revoked
key — which promotes the Chrome User-Agent this pass removed from "hygiene" to a
genuinely plausible culprit. **Check the response content-type before touching
the key**: Trakt's own app layer returns a 9-byte JSON `Forbidden`, Cloudflare
returns an HTML interstitial.

**And it is worth very little either way.** Trakt supplies IDENTIFIERS, never
ratings, and sits at the DEEPEST tier of the tmdbId ladder in `MovieService`. The
branch is reachable only when ALL of: TMDB title/director search found nothing,
the row still has no tmdbId, the row HAS an imdbId from a non-TMDB source, AND
TMDB's own `/find` on that imdbId already missed. Letterboxd and Filmweb→Wikidata
are then tried after it.

Measured in prod 2026-07-31 across all three databases:

| | films | no tmdbId | **Trakt-eligible** (no tmdbId **and** has imdbId) |
|---|---|---|---|
| `kinowo` (PL) | 909 | 237 | **1** |
| `kinowo_de` | 1,246 | 35 | **1** |
| `kinowo_uk` | 1,616 | 143 | **4** |
| **total** | **3,771** | **415** | **6** |

So Trakt's entire addressable population is **6 films, 0.16% of the corpus** — the
other 409 tmdbId-less rows have no imdbId either, so the branch never even runs
for them. Its realised yield right now is **zero**: those same 6 are still
unresolved despite Trakt working normally until yesterday afternoon. They are
exactly the long tail it was meant to catch and didn't — a K-pop VR concert
(`tt38691436`), an Opera Australia broadcast, a Royal Ballet & Opera broadcast, a
Tamil film (`kandan|2026`), a 2008 Polish film, and a 2025 "Blade". Cost is ~137
calls/day.

**Caveat, stated because it bounds the claim:** this is a SNAPSHOT. A film Trakt
resolved in the past now carries a tmdbId and is invisible to the count, and
there is **no provenance field recording which resolver bound an id and no metric
counting Trakt hits** — so the ceiling above is exact, but historical yield
cannot be reconstructed from the data we keep.

**Recommendation for a human:** do not spend much on this. Fixing the key is
worth a five-minute look at `trakt.tv/oauth/applications`; if that is not it,
retiring `TraktClient` and its resolver is defensible — Letterboxd occupies the
same tier by the same imdbId key, and the measured ceiling is six films. If we DO
keep it, the cheap improvement is provenance (record which resolver bound a
tmdbId) so the next person can answer this question from data instead of
inference.

#### Outcome (2026-08-01): retired

We took the retirement option. `TraktClient`, `TraktIdResolver`, their specs and
fixtures are gone; the `traktIdResolver` rung is removed from both
`ImdbIdResolver` and `MovieService.resolveTmdbId`, so Letterboxd is now the
first id-crosswalk rung after TMDB's own `/find`. `NoTraktIntegrationSpec` guards
against it coming back. The `TRAKT_API_CLIENT_ID` / `TRAKT_API_SECRET` Fly
secrets are now unread by any code path and can be unset at leisure.

### Prod verification of this run's fixes

- **KinoPort — CONFIRMED GREEN.** Its 2026-07-31 **09:30 UTC** bucket flipped to
  green after the deploy (`succ=1`), ending five straight white buckets. Verified
  in prod, not merely shipped — no follow-up owed.
- **Kino Studio Opole — still white, and that is CORRECT.** The venue's break runs
  to 3 September, so the (now correctly-read) live page carries zero films. The
  fix's value is that it is reading the REAL page instead of a soft-404, and that
  a genuinely dead source would now go red. **Re-check after 3 Sept**: if Studio
  is still white once its season restarts, the two-slug fallback needs another
  look.
- **The Old Court Windsor — CONFIRMED GREEN.** UK venues scrape on a slow chunked
  cadence (its buckets run ~7h apart: 13:30 / 20:30 / 03:30 UTC), so this needed
  waiting out; the **10:30 UTC** bucket came back `succ=1, fail=0, zero=0` and
  `kinowo_uk.web_screenings` now holds real rows keyed to `oldcourt.org.uk/event/…`
  URLs. **No follow-up owed. Both of this run's white-bar fixes are verified in
  prod.**
  - Cosmetic, NOT a scraper bug, noted so nobody re-diagnoses it: the projected
    row is `supertroopers3|2001` — enrichment bound the 2001 original rather than
    the 2026 film. The client supplies no year (the venue's listing has none), so
    this is the known year-disagreement class, resolved at enrichment, not here.
  - Only 9 of the 12 films had projected at the time of checking; the rest were
    still incubating in `pending_movies`. Expected for a venue whose entire slate
    is new film+cinema pairs.

### Still open after this pass

- **Kino Zachęta (Kleczew)** — `unfixable` unless we build OCR; the venue
  publishes only a JPEG. Unchanged.
- **Wybrzeże** — expired TLS certificate at the source, four runs old. Nothing we
  can do from our side; worth a human deciding whether to retire the venue.
  **→ resolved in the 2026-08-04 pass above: the portal serves the same page over
  plain HTTP, so the venue was re-wired `http://` rather than retired.**
- **The DE open-air cohort and the 55 remaining UK Flicks venues** — genuine
  aggregator coverage gaps. Fixing them means bespoke own-site clients per venue,
  which is a project, not a white-run change. The Old Court above is the template
  if anyone wants to start.
- **Kino Sfinks / Kino Zamek** — both still waiting on the venue to repopulate its
  calendar before a parser can be written or tested against it.

---

## 2026-07-28

**This run covered ALL THREE countries** (previous runs were Poland-only).
Per-country uptime lives in a SEPARATE Mongo database — `kinowo` (PL),
`kinowo_de`, `kinowo_uk` — reachable over the one `flyctl proxy` via
`db.getSiblingDB(...)`; a PL-only sweep silently ignores ~2,400 foreign venues.
Totals: **PL 20 white / 1 red**, **DE 323 white / 23 red (of 1,538 services)**,
**UK 63 white / 0 red (of 850)**. One real bug found and fixed — in the UK
Flicks client (`fixed` @d618f3a86, below). Full per-country detail after the
Polish section.

### PL — 20 white

**20 cinemas were 3-scrape-white** (newest bucket overall 2026-07-28 00:00 UTC /
02:00 Warsaw; every venue's three white buckets land 23:15–00:00 UTC, i.e. within
~45 min of the newest — actively scraping, not a boot artifact). **No code change
shipped — all 20 white venues are genuinely film-dormant.** All three venues NEW
to the white set were probed live and resolve to dormancy at the data layer; the
17 carried-over venues were batch-re-probed and none had recovered-but-broken.
Discovery method unchanged (`/uptime` auth-gated): a mongosh replay of
`UptimeController`'s predicate over prod `uptimeBuckets` via the running
`flyctl proxy` on `127.0.0.1:27017` — per service, last 3 non-empty buckets all
`status==zero` (`successes==0 && failures==0 && zeroes>0`), excluding
`|enrichment` / the 6 enrichment sources / `img:*`.

**Set changes vs 2026-07-24 (was 18):**
- **RECOVERED / fell off the white set:** **Kino Krapkowice** — its summer break
  was announced to 31 Jul but it reopened early; the 24h bucket history reads
  `wwwwwwwGGGGGGGGGGGGGGGGG` (green from ~02:45 UTC on 07-27 onward, last green
  2026-07-27 23:30 UTC). Parser was correct all along; venue simply resumed.
- **NEW this run (3, all probed live, all dormant):** **Kino Cytadela**,
  **Kino Tur**, **Kino MOK Nowa Ruda** — see below.
- **Carried over (17, all still white):** ADA Kino Studyjne, Cyfrowe Kino, DKF
  Politechnika, Kino CK Lublin, Kino Chatka Żaka, Kino Kuźnica, Kino nad Wartą,
  Kino PDK, Kino Sfinks (needs-human), Kino Świt, Kino ŚDK, Kino Warszawa
  (Przeworsk), Kino Wisła Brzeszcze, Kino Zamek (needs-human), Kozienicki Dom
  Kultury, Patria, Studio (Opole). Batch-probed live this run — every one still
  genuinely film-dormant or on a known break; **no parser recovered-but-broke.**

**Out-of-scope heads-up (RED, not white — fetch failure, different mode):**
**Wybrzeże** was 3-scrape-**failing** (red) for the third run running (07-21,
07-24, 07-28): `CircuitOpenException: circuit open for bilety.rck.kolobrzeg.pl`,
behind the expired TLS certificate diagnosed 07-24. The breaker correctly
surfaces it red, not white. Not a white target. **needs-human — now three runs
old; the cinema must renew its cert, we cannot fix this from our side.**

### Kino Cytadela (Muzeum Historii Polski, Warszawa) — `intentionally-dormant`
- Client: `CytadelaClient` @ `muzhp.pl/repertuar`. Live: HTTP **200**, 83,043 B,
  no redirect (`/pl/repertuar` serves the byte-identical document). Parser
  selectors all zero: `repertoire-list__title`=0, `repertoire-item`=0,
  `repertoire-item__time`=0, `/kino-film/` hrefs=0.
- **Not markup drift** — the surrounding contract is intact (`repertoire-filters`
  form, `repertoire-index` section still present); the results container is
  server-rendered EMPTY: `<div class="repertoire-list js-list-data"
  id="result-list"></div>`, and the filter form carries the site's own
  zero-signal `data-maxpages="0"` plus the empty-state string
  `data-textnofound="Brak wyników dla podanego zakresu wyszukiwań"`. No
  client-side hydration (no `__NEXT_DATA__` / `fetch(` / `admin-ajax` /
  `wp-json`); the site's own `/kino-repertuar/filterAjax` endpoint 404s.
- Cross-checked for films elsewhere and found none: the booking shop
  `sklep.muzhp.pl` has only "Gadżety"/"Wydawnictwa" product trees and its ticket
  groups (`/rezerwacja/grupa-wydarzen.html`, idgw=16/20) list 15 items that are
  all guided tours/walks — zero cinema products. Archived `/kino-film/<slug>`
  pages (e.g. `pamietniki-tatusia-muminka`, `pucio`, last crawled May 2026) still
  return 200 but their `ul.movie-detail-header__dates-list` is empty. `/kino`
  is generic marketing copy. `sitemap.xml` and `robots.txt` both 404.
- Caveat worth carrying: **no break announcement exists** — `/wydarzenia` and
  `/aktualnosci` have zero hits for przerwa/wakacje/nieczynne/wrzesień/wracamy.
  So this is an unannounced gap rather than a stated hiatus. Parser correct, no
  test-backable fix. **Re-check next run; if still white in 2+ more runs, escalate**
  (the museum may have quietly stopped programming the cinema).

### Kino Tur (Turek) — `intentionally-dormant` (between repertoires)
- Client: `BiletynaClient` @ `biletyna.pl/Turek/Kino-Tur`. Live: HTTP 200,
  77,156 B; the single ld+json block's `events` array holds exactly **1** entry,
  `@type` **ComedyEvent** ("Mariusz Kałamaga – Mamo! Papier się kończy!",
  2026-10-03) — **0** `ScreeningEvent`. The parser's ScreeningEvent-only filter
  correctly drops it.
- **This is a fresh, benign gap, not a source going empty.** The 24h bucket
  history reads `GGGGGGGGGGGGGGGGwwwwwwww` — green until **2026-07-27 16:00 UTC**,
  white only for the last 8 buckets. Biletyna was carrying this venue's films
  right up to yesterday; the last screenings ran 24–27 July and the August
  repertoire simply is not loaded yet. Expect recovery when it is.
- Noted for a future run (do NOT act on it now): the venue's own site
  `mdk.turek.pl` publishes monthly repertoire articles (Joomla,
  `div.com-content-article__body`, e.g. `/index.php/repertuar-kina-tur-czerwiec-2026`
  → "Drzewo Magii" 5–8.06, "Toy Story 5" 19–22.06, "Gwiezdne wojny. Mandalorian
  i Grogu" 26–29.06), and `miastoturek.pl/wydarzenia` carries weekly "Kino Tur
  zaprasza na filmy" listings. `kinotur.pl` / `dk.turek.pl` do not resolve;
  Filmweb has no `/kina/Turek` page (404). **Repointing would be wrong today** —
  biletyna is a live, working source that merely ran out of dated events
  yesterday, and mdk.turek.pl has no July/August article either (only up to
  czerwiec 2026). If biletyna is STILL empty in 2+ runs while mdk.turek.pl has a
  current monthly article, THAT is the moment to migrate to an own-site scraper.

### Kino MOK Nowa Ruda — `intentionally-dormant` (unchanged since 2026-06-28)
- Client: `MsiClient` @ `bilety.nowaruda.pl`. Both in-window month pages
  (2026-07, 2026-08) return HTTP 200 with **0** `movies-movie__single js-event`
  blocks. Swept 2026-09 → 2026-12 as well: the only event anywhere is
  **1** block in 2026-10 — *"Piotr Bałtroczyk - Stand Up 2026"*, 2026-10-11
  17:00, `czas: 90 min.` — a stand-up, correctly dropped by
  `OnlyMovieEventsFilter`. Exactly the diagnosis from 2026-06-28; the venue has
  simply never resumed film programming. Parser correct.
- Portal quirk (no impact, recorded so a future run doesn't chase it): this
  venue's `/MSI/mvc/pl/Repertoire/GetShortEventsWithFilters?date=…` endpoint
  **ignores the `date` param** and returns the same 2,643-byte body for
  2026-07/08/09/10, always the next upcoming item — and it tags that stand-up
  `"eventCategoryName":"kino"`. `MsiClient` doesn't read that endpoint (it parses
  the month page), so nothing is affected, but the "kino" label there is not
  trustworthy evidence of a film.

### Heads-up (NOT a white cause, evidence-backed, left for a human): `MsiClient` caps its scrape horizon at 2 months
- `MsiClient.fetchUnfiltered` fetches exactly `YearMonth.from(today)` and
  `.plusMonths(1)`. That is a hard-coded horizon cap, which the standing rule
  (`ScrapeHorizon.MaxDays = 730`, "want ALL future screenings") forbids — every
  screening 2+ months out is invisible at all ~12 MSI venues (Cinema1 Gdańsk,
  GOK Tychowo, Chemik, Twierdza, Nowa Ruda, Przeworsk, Sztum, Kozienice, ŚDK,
  Skarżysko, Wybrzeże, Planeta Brzesko).
- **Measured today, it hides zero films**, which is why nothing shipped. Swept
  2026-08 → 2026-11 across five MSI portals and pulled every out-of-window title:
  Sztum 09 (KONCERT ELENI, RETRANSMISJA ANDRE RIEU) and 10 (KONCERT ŚLĄSKICH
  GWIAZD, ANDRE RIEU, SPEKTAKL "ŻONA DO ADOPCJI"); mok.com.pl 10 (Gala Fado) and
  11 (stand-up Rutkowskiego, Spektakl "Sklep z facetami"); Cinema1 09 (ANDRE RIEU
  – NIECH ŻYJE MAASTRICHT! 2D NAPISY); GOK Tychowo 10 (Koncert Liszt Meets
  Queen). Every one is a concert / stand-up / theatre / concert-retransmission —
  non-film, and `OnlyMovieEventsFilter` would drop them anyway. These venues
  publish only ~1–2 months ahead, so the cap costs nothing right now.
- Not fixed this run on scope grounds: it causes **no** white bar, and lifting it
  changes scrape volume for 12 venues at once (and would likely shift
  `read-model-snapshot.json`) — too broad for an unattended white-cinema run with
  zero current data gain. **Follow-up for a human:** replace the fixed 2-month
  window with a `ScrapeHorizon.MaxDays`-derived month range (stopping early on
  consecutive empty months so it stays cheap). Test-backable in
  `MsiClientSpec` — a `FakeHttpFetch` serving a film in month+2 fails before /
  passes after.

### UK — 63 white, 0 red — ONE REAL BUG, `fixed` @d618f3a86

62 of the 63 are `FlicksClient` venues (flicks.co.uk, the UK listings
aggregator); the 1 remaining is `GatsbyBoxOfficeClient` (Everyman Durham).
Swept all 62 Flicks venue pages and split them by whether Flicks itself
advertises any day tabs:

- **1 venue** — Neuadd Dwyfor Pwllheli — the venue page did not resolve at all
  (transient URLError on the sweep). Not re-probed; re-check next run.
- **57 venues** — HTTP 200 but **zero `data-date` day tabs**: Flicks holds no
  sessions for them at all. **Not our bug and not fixable in our code.** Several
  are genuinely closed (Belmont Filmhouse Aberdeen — shut since Oct 2022;
  Watermans Brentford — building closed Apr 2024 pending relocation; Everyman
  Durham — "closed until further notice"). But several others are unambiguously
  OPERATING and simply missing from Flicks' backend — verified live against
  their own sites: **Phoenix Cinema East Finchley** (phoenixcinema.co.uk listing
  The Odyssey / Toy Story 5 / Blue Heron with dated screenings 2026-07-28
  onward), **Institute of Contemporary Arts** (ica.art/films), **ARC
  Stockton-on-Tees** (arconline.co.uk). Control: `curzon-soho` returns 4 films
  for 2026-07-28, so the endpoint and the `is-ajax-call: yes` header are fine —
  the zeroes are upstream coverage gaps, not a fetch failure.
  **needs-human: an aggregator coverage gap.** The only fix is per-venue
  own-site scrapers; that is a project, not a white-run change. Worth deciding
  whether the biggest names (Phoenix, ICA) justify bespoke clients.
- **4 venues** — HTTP 200 **WITH day tabs**, i.e. Flicks WAS advertising a
  programme while we recorded zero. **That is the bug.** Barn Cinema Dartington
  (tabs from 2026-07-28), Broadway Cinema Villa Marina (from 07-28), Watersmeet
  (07-29), Cube Cinema Bristol (from 08-02).

**Root cause — `FlicksClient` required the session button to be an `<a>`.**
Flicks renders a session button as `<a class="times-calendar-times__button">`
only where the venue has an online booking deep-link wired in. Venues without
one render the identical card with the button as a plain
`<span class="times-calendar-times__button">` — same time text, no href.
`parseDay` did `li.selectFirst("a.times-calendar-times__button")`, so every such
`li` yielded nothing, the film yielded no showtimes, the article was dropped and
the venue scraped to **zero films → white**, while Flicks was plainly listing
its programme. Measured: dartington 07-28 has `a=0, span=6`; villa-marina
`a=0, span=4`; every one of the four affected venues is span-only. A sample of
currently-GREEN venues (curzon-soho `a=22`, phoenix-picturehouse-oxford `a=14`,
watershed-bristol `a=12`) is `<a>`-only, so bookable venues are unaffected by
the change and no partial loss was hiding there.

**Fix** (`fixed` @d618f3a86): match the button by CLASS, not tag
(`.times-calendar-times__button`), in both the session loop and the
`data-eventjson` lookup; the booking link simply stays `None` when there is no
href — an unbookable screening is still a screening. `parseTime` already falls
back from the absent `data-optlabel` to the visible "4:00 pm" text, so no time
handling changed. Fail-before/pass-after `FlicksClientSpec` block over a real
recorded fixture (Barn Cinema Dartington, 2026-07-28 — 2 films, 6 span buttons,
0 `<a>`): before the fix `parseDay` returned `List()` and 4 new tests failed;
after, all 21 pass. Also pins that the duplicated desktop/mobile buttons collapse
to one showtime per screening via the existing `(time, booking)` dedup.
Snapshots did NOT shift (`read-model-snapshot.json` / `expected-*.html` are
built from a corpus with no span-button fixture), confirmed by `testUnit`
— which includes `e2e/Test/test` — passing untouched. CI green end-to-end on
the merge commit (all test layers + all six deploys, incl. `kinowo-worker-uk`).

**Verification still owed — do this FIRST next run.** At hand-off the four
venues' newest uptime buckets (03:45–04:45 UTC) still PREDATED the deploy
(~05:15 UTC), so the fix is shipped but has NOT yet been observed working in
prod; UK venues scrape on a chunked cadence, so a given venue can go 1–2h
between scrapes. **Confirm Barn Cinema Dartington, Broadway Cinema Villa Marina,
Watersmeet and Cube Cinema Bristol have gone GREEN.** If they are still white
with `data-date` tabs present on their Flicks page, the span-button fix was not
the whole story and the venue needs re-probing.

### DE — 323 white, 23 red — no code change; a source-coverage story

All 1,538 German venues are one client (`WebediaShowtimesClient`) against ONE
host, `www.filmstarts.de`, one `theaterId` each. So a DE-wide problem would be
systemic — it isn't.

- **The API is healthy and is NOT blocking us.** Control venues return full
  slates (`theater-A0076` → 11–13 films/day, with `nextDate` set correctly when
  a single day is empty). The 323 white venues return HTTP 200 with
  `{"error":true,"message":"no.showtime.error","results":[],"nextDate":null}` —
  and `nextDate:null` persisted across +1/+3/+7/+14 days. Their
  `/kinoprogramm/kino/<ID>/` pages are 200 (not 404) but carry an empty
  `schema.org/ItemList`. So filmstarts itself is asserting "no programme", and
  our parser is correctly reporting zero.
- **Sampled 7 white venues against their OWN websites.** 5 confirmed real:
  **CinemaxX SI-Centrum Stuttgart** — closed since a mid-2025 burst pipe
  flooded it, still shut (its own booking widget says "NICHTS ANZUZEIGEN" while
  the neighbouring CinemaxX Liederhalle sells a full slate); **Mephisto
  Augsburg** — permanently CLOSED (ARB Kino GmbH insolvency, shut end of Jan
  2026, hall being converted by the Staatstheater); **Scala Schopfheim** —
  "Sommerpause… ab 2. September"; **Kinocafe Taufkirchen** — "Sommerpause… bis
  Mi 26. August"; **CineAStA Trier** — student cinema, semester break (expected).
  2 look like genuine filmstarts staleness, both OPEN-AIR: **Lichtburg Open Air
  Lemförde** (its own site's JSON-LD has ScreeningEvent "Die Ältern" 2026-07-31
  and 08-01) and **Open-Air Kino Neu-Anspach** (event on 2026-08-03).
- **Verdict: `intentionally-dormant` in the main, with an open-air caveat.**
  Late July is peak Sommerpause for German indoor cinemas AND the semester
  break, so a 21% white rate is plausible; the sample says the white set is
  dominated by real breaks/closures rather than an aggregator bug. But
  filmstarts appears to under-carry seasonal open-air venues specifically. Not
  actioned: a fix means own-site scrapers for the open-air cohort, and no
  fail-before/pass-after test can be written against a source that is correctly
  reporting what it knows. **needs-human if we want open-air coverage** — the
  cohort is identifiable by name (`Autokino…`, `Sommerkino…`, `Open-Air…`).
- **DE red (23, out of scope but worth recording):** all `filmstarts.de`. The
  underlying errors in the 24h window are **HTTP 404 for
  `/kinoprogramm/kino/<ID>/`** on ~11 distinct theater IDs (`G02Q9`, `A2907`,
  `A1451`, `G01C9`, `A2843`, `A2203`, `A1458`, `A0613`, `A0875`, `A2165`,
  `A2846`) — venues DELISTED from filmstarts, correctly surfacing red rather
  than white — plus `HttpTimeoutException` (96×) and the resulting
  `CircuitOpenException`. The 404 IDs are a concrete, mechanical cleanup: they
  will never recover on their own and each burns retries every cycle.
  **needs-human: prune or re-resolve those theater IDs.**

**UK red = 0**, but the 24h error log shows `403` on
`cineworld.co.uk/…/quickbook/10108/dates` and on seven `myvue.com` cinema ids,
plus proxy `401 WWW-Authenticate header missing` (28×) — chain endpoints
rejecting us intermittently. They are not currently 3-scrape-red, so out of
scope; noted so a future run can tell a new problem from a standing one.

---

## 2026-07-24

**18 cinemas were 3-scrape-white** (real buckets ~12:30–15:30 UTC / 14:30–17:30
Warsaw, newest bucket overall 15:30 UTC — actively scraping, every venue's three
white buckets within ~1h of the newest, not a boot artifact). **No code change
shipped — every white venue is genuinely film-dormant, on a known break, or a
standing needs-human.** Both venues NEW to the white set were probed live and
resolve to dormant at the data layer (no fixable parser bug). Discovery method
unchanged (`/uptime` auth-gated): a mongosh replay of `UptimeController`'s
predicate over prod `uptimeBuckets` via the running `flyctl proxy` on
`127.0.0.1:27017` — per service, last 3 non-empty buckets all `status==zero`
(`successes==0 && failures==0 && zeroes>0`), excluding `|enrichment` /
the 6 enrichment sources / `img:*`.

**Set changes vs 2026-07-21 (was 18):**
- **RECOVERED / fell off the white set:** **Centrum 3D Przemyśl** — my 2026-07-21
  fix (`fixed` @0f74f76d0, own-site `KinoCentrum3DPrzemyslClient`) worked
  end-to-end; no longer 3-white. And **Teatr Ziemi Rybnickiej** (was
  intentionally-dormant) — off the white set.
- **NEW this run (both probed live, both dormant):** **Cyfrowe Kino** and
  **Kino ŚDK** — see below.
- **Carried over (16, all still white):** ADA Kino Studyjne, DKF Politechnika,
  Kino CK Lublin, Kino Chatka Żaka, Kino Krapkowice, Kino Kuźnica, Kino nad Wartą,
  Kino PDK, Kino Sfinks (needs-human), Kino Świt, Kino Warszawa (Przeworsk),
  Kino Wisła Brzeszcze, Kino Zamek (needs-human), Kozienicki Dom Kultury, Patria,
  Studio (Opole). Batch-probed live this run — every one still genuinely
  film-dormant or on a known break; no parser recovered-but-broke.

**Out-of-scope heads-up (RED, not white — fetch failure, different mode):**
**Wybrzeże** was 3-scrape-**failing** (red), same as 2026-07-21:
`SSLHandshakeException (certificate_expired) … PKIX path validation failed` for
`bilety.rck.kolobrzeg.pl`, then `CircuitOpenException: circuit open`. The venue's
TLS certificate has expired at the source; the breaker correctly surfaces it red
(not white). Not a white target. **needs-human** if it doesn't self-resolve (the
cinema must renew its cert) — flagged for the next run / a human.

### Cyfrowe Kino (Środa Śląska) — `intentionally-dormant`
- Client: `CyfroweKinoClient` @ `dksrodaslaska.pl/aktualny-repertuar/` (own-site
  WordPress "amy-movie" theme; migrated off Filmweb 2313 previously). Live: HTTP
  **200** (38.7 KB, no redirect). The naive grep sees 4 `amy-movie-item` / 4
  `st-item`, but those are all inside the theme's `<style>` CSS — after stripping
  `<style>`/`<script>` the real DOM has the list container
  `<div class="amy-movie-items"></div>` **empty**, zero `div.amy-movie-item`
  children, and no film title / date / poster anywhere. No client-side loader
  either: the only `admin-ajax.php` / `wp-json` refs are generic WP oembed
  boilerplate (`wp-json/wp/v2/pages/141`, oembed) — no amy-movie repertoire ajax
  action. So the venue's own site currently lists **no films**; the parser
  (`select("div.amy-movie-item")`) correctly returns empty. Small Dom-Kultury
  cinema, summer-dormant. Re-check next run.

### Kino ŚDK (Świebodzin) — `intentionally-dormant`
- Client: `MsiClient` @ `https://bilety.kino.swiebodzin.pl:4433` (Świebodziński
  Dom Kultury MSI portal). Both month pages (2026-07, 2026-08) return HTTP **200**
  with **0** `div.movies-movie__single` (the JS-shell). Verified past the render
  layer via the portal's own data endpoint
  `/MSI/mvc/pl/Repertoire/GetShortEventsWithFilters?date=2026-07` (and `…08`):
  both return `{"repertoireEvents":[],"dates":[]}` (55 bytes) — **genuinely empty
  at the data layer**, not a fetch failure (so the MSI total-outage guard added
  2026-07-11 correctly leaves it white, not red). No test-backable fix; venue
  film-dormant for the summer. Re-check next run.

### Kino Sfinks (Kraków, Nowa Huta) — `needs-human` (unchanged since 2026-07-11)
- Client: `KinoSfinksClient` @ `kinosfinks.okn.edu.pl/wydarzenia-harmonogram.html`.
  Live this run: still empty ("Brak wydarzeń" / `.empty-results`, no
  `table.widok_listy tr[onclick]`) AND still on the drifted markup. Zero screening
  rows anywhere ⇒ no populated row shape to sample ⇒ a new parser still can't be
  written or test-backed blind. **needs-human — re-check once the venue
  repopulates its calendar**, then rebuild the parser against the new (populated)
  row shape (treat `.empty-results` as zero screenings, not a parse failure).

### Kino Zamek (Szczecin) — `needs-human` (unchanged festival filter-gap)
- Client: `KinoZamekClient`. The `zamek.szczecin.pl/wydarzenia/kino/` listing still
  yields only festival/banner slugs, no individual film-title slugs, so the
  per-title→slug prefix match filters every MSI film out. Same standing product
  call as 2026-06-28…07-21 — likely self-resolves when normal repertoire resumes
  and individual film slugs return.

**Carried-over dormant (14, batch-probed live this run — all still
`intentionally-dormant`):** ADA Kino Studyjne (`"events":[]` + "Brak wydarzeń", 0
`ScreeningEvent`), DKF Politechnika (Filmweb 1645 `[]` for 2026-07-25/26 —
summer/academic break), Kino CK Lublin (0 `Kup bilet - Film:`, only theatre/
concert programme), Kino Chatka Żaka ("Brak wydarzeń"), Kino Krapkowice (summer
break → **31 Jul**, still in window), Kino Kuźnica (header-only
`table.tbl_repertoire`), Kino nad Wartą (0 `Film:` anchors), Kino PDK (0
`ScreeningEvent`), Kino Świt ("Brak nadchodzących seansów filmowych"), Kino
Warszawa Przeworsk (MSI `repertoireEvents:[]` 07/08), Kino Wisła Brzeszcze (0
`Film:`, 39 Koncert + 8 Spektakl), Kozienicki Dom Kultury (MSI
`repertoireEvents:[]` 07/08), Patria ("Brak filmu" every slot), Studio Opole
(`kino-studio.html` soft-404, `kino-studio-przerwa.html` live → break to
**3 Sept**). Each parser correct; each venue genuinely un-programmed or within its
known break window.

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
