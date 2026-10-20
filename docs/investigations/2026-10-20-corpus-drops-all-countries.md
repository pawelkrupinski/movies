# Corpus, coverage and traffic drops in every country except Germany's films, 2026-10-06 → 2026-10-20

Investigated 2026-10-20 (data up to about 10:00 UTC), read-only. All times are UTC. This covers es, pl, uk and us on
every metric, and de on everything except films and films with a TMDB id (those are covered in
[2026-10-20-de-corpus-decline-oct12-19.md](2026-10-20-de-corpus-decline-oct12-19.md)).

Known and excluded: caches start cold after the 2026-10-20 06:54–06:58 worker/web restarts; OMDb has returned 401
since about 02:50 today; the per-country TMDB backfill restarts (pl 09:25, uk 09:45/09:53, …).

Method: the movies Prometheus on monitoring-1 (`10.20.0.11:9090`, `query_range` at 1h steps, plus 1d `increase`
steps for counters), with the series the `application-health`, `worker-diagnostics` and `kinowo-http` dashboards
plot. Logs come from VictoriaLogs (`10.20.0.11:9428`) and causes from `git log forgejo/main`. The public
`/:city/api/repertoire` was used to sample which films have no id. A "drop" means more than about 2%, or a step
change that a restart does not explain. Values falling to 0 for one scrape at a restart are ignored. Showtimes
have a weekly cycle (each country's new week arrives on a fixed weekday, then runs down), so they are compared
week over week (WoW).

## Summary

**Verdict: nothing is broken.** No cinema chain went dark, no scraper broke country-wide, and no TMDB/IMDb outage
emptied the corpus. Of the eight movements below, three are dilution or settling after the 2026-10-12 source
expansion, one is weekly timing, two are external traffic changes, and two are small, slow trends worth watching:
Poland's TMDB coverage, and Poland's stale-venue count.

| # | Country | Metric | From → to | When | Cause | Real loss? |
|---|---|---|---|---|---|---|
| 1 | pl | Films with a TMDB id (share of films) | 797 / 1,126 (70.8%) → 709 / 1,112 (63.8%); IMDb id 68.7% → 62.3%; any rating 67.7% → 61.0% | Gradual, 10-07 → 10-20, with steps at 10-14 12–18h (−1.6 pp) and 10-19 18h (−1.7 pp) | The corpus shifted toward non-film events and new Polish releases that have no TMDB/IMDb record yet; identity rules tightened 10-14/15 | Partly: about 20 genuine films unmatched |
| 2 | uk | Films with a TMDB id (share) | 96.6% → 58.8%, now 62.5% (the absolute count *rose* 1,368 → 1,671) | Step 10-12 12h → 10-13 00h | Dilution: own box offices (43 Savoy, 36 Spektrix, 9 Ticketsolve, BFI, …) added about 1,400 titles, many of them events | No |
| 3 | uk | Films (total) | 2,816 → 2,597 (−7.8%), now 2,723 | 10-13 06h → 10-19 06h | Identity settling after the 10-12 expansion (merges and retires exceeded new films) | No |
| 4 | es | Films with a TMDB id (share) | 99.1% → 92.0%; any rating 90% → 84% | Step 10-12 12h | Dilution: 56 chain venues moved to Taquilla and 10 to Artesiete, adding operas, ballets and long calendars | No |
| 5 | es | Showtimes (WoW) | −32% (43k → 29k at 10-14 02h) | 10-13 → 10-14 | The weekly refresh landed a day later (10-14/15, not 10-07/08) after Monday 10-12's national holiday. It peaked higher (66k vs 64k) | No |
| 6 | us | Films (total) | 2,234 → 2,050 (−8.2%) | Gradual, 10-08 → 10-19 | Steady churn: daily retires exceed new films (seasonal October lull). The share with TMDB stayed 97.4–97.9% | No (watch) |
| 7 | pl (and de/es/uk/us) | Web traffic | pl 2xx 16.8k → 13.8k/day (−18%); pl `/:city/movie/:slug` 4.6k → 1.1–1.4k/day; `/:city/movies` in de/uk/us ~2.5k/1k/6k → ~0–150/day | pl gradual 10-06 → 10-13; `/:city/movies` step at **10-09 04:00** | External, most likely crawlers: no web deploy at 10-09 04:00 (web pods up since 10-08 10:21); the probe/other traffic and the landing routes held | No (not user traffic) |
| 8 | pl | Cinemas with fresh listings (stale venues) | 1 → 8 stale venues (fresh listings older than 7 days) | Gradual, 10-06 → 10-20 | A handful of small own-site venues failing upstream (expired TLS certs, connection errors) | Yes, but small (8 of about 400) |

## No drop

- **Showtimes** (`sum by (country) (max by (country, city) (kinowo_worker_showtimes))`): week over week, de is about
  100%, uk 95–106%, us 94–104%, and pl's dip to 80% at 10-13 16h was the weekly refresh arriving an hour later (pl's
  new week lands around 18h on Tuesday/Wednesday; 10-20's has not landed yet). Right now: es +23%, pl +25% (refresh
  timing), uk +6% and us +4% versus a week ago.
- **Films served** (`kinowo_worker_movies_served`, `kinowo_web_movies_served`): worker and web agree. The jumps are
  page restructures (US metros → towns 10-11 21:55; UK page rules, 76 → 264 pages, 10-13), not drops. City series
  that vanished (for example us `bristol-tennessee`, `birmingham-alabama`) were renamed to bare slugs (`bristol`,
  `birmingham`, 301-redirected) and still carry their showtimes.
- **Cinemas with listings**: `kinowo_worker_cinema_never_content` *fell* everywhere (de 326 → 121, es 150 → 121,
  uk 32 → 18, us 561 → 511, pl 2 → 1). `cinema_scrape_never_scraped`, `scrape_listing_incomplete_streak_venues` and
  `seasonal_venues_not_reopened` are 0 throughout. Stale venues fell in de (44 → 0) and es (22 → 0), stayed 0 in uk
  and us, and rose in pl only (row 8). The cities that went from some showtimes to none in the last week are
  single-venue towns with 1–35 showtimes (us 38, es 5, pl 3, de 1), which is the normal end of a small venue's week.
- **Enrichment**: `kinowo_worker_share_cards_coverage_ratio` stayed at 1.0 everywhere.
  `kinowo_worker_rating_resolved_not_run` is 0 for every site. Rating subsets track the id subsets in every country
  (OMDb's 401 today has not moved `imdb_rating` yet: uk is at 1,628, up). `unresolved_with_showtimes` is 0
  everywhere.
- **de, other than films and TMDB**: showtimes WoW 99–100%, served flat WoW, listings flat WoW, never-content and
  stale venues both down, traffic up.
- **Cache entries**: worker `movie_corpus` equals the corpus total (so it moves only with rows 3 and 6).
  `task_dedup` is noisy and resets at restarts. The web `response`/`minifier`/`cards` caches are reset by every
  restart (14 worker/web rollouts on 10-12 alone, and today's 06:54) and refill in hours. `tmdb_answers` and
  `tmdb_film_hits` are new series from today's build (es dipped 3.9% at 08:00, which is cache churn).
- **Scraping health**: worker `http_total` success per day is flat in es, pl and us. uk rose after 10-12, and de rose
  after 10-11. Breaker opens are low. A proxy authentication failure ("too many authentication attempts. Limit: 3")
  hit worker-pl/uk/us on 10-20 between 00:00 and 00:20 only: Flicks UK/US, Cineworld and Multikino were skipped for
  20 minutes and recovered. The worker log shows Multikino 403s every day of the window, yet pl showtimes did not
  drop.

## Evidence per drop

### 1. Poland: TMDB/IMDb/rating coverage drifting down

`kinowo_worker_corpus_movies{country="pl"}` at 6h steps: the total held at 1,050–1,136 while `with_tmdb_id` went
797 (10-07 12h) → 768 (10-10 12h) → 722 (10-12 12h) → 738 (10-14 12h) → 731 (10-16 12h) → 717 (10-18 12h) → 709
(10-20 06h). The share fell in steps: 10-07→10-10 (70.8 → 68.8%), 10-12 12h (−0.8 pp), 10-13 18h (−0.9 pp), 10-14
12–18h (−1.6 pp), 10-16 18h (−0.9 pp) and 10-19 18h (−1.7 pp). The last two came with no worker deploy (there was
none between 10-15 23:40 and 10-20 06:54), and both were evenings when the total jumped by about 20: new titles
arriving without an id.

What the unmatched films are (`/warszawa/api/repertoire`, 579 titles, 168 with no IMDb link): mostly not films.
They are festival blocks (Terra, Splat! FilmFest, Cinemaforum, LIAF-style programmes), children's cycles ("Misiowe
Poranki", "Bajki i zabawy"), ballet and opera relays, e-sports ("League of Legends Worlds"), marathons, talks, and
improv nights. Only 21 look like films. Some are new Polish releases with no TMDB/IMDb record or match yet ("Jak
żyć, żeby nie zwariować", "Przepis na święta", "Cicha powódź", "Jedziemy do domu", "Lachtara. Tęsknota", "TEDE.
Forever Rap"; TEDE's IMDb lookup failed today on OMDb's 401). Others are strand-prefixed copies of the same title
that stay separate films, each with no id ("Kino na obcasach: Jak żyć…", "WSP: Jak żyć…", "KLAPS: Jak żyć…").

Identity changes are a small part of it: `kinowo_worker_identity_take_reversals_total{country="pl"}` totalled
about 16 over the window. Daily `identity_regroupings_total` shows pl `fresh` ≈ `retire` (25–80 a day each). The
10-14/15 rule tightening (for example `ca4059b55` "Fill no film on one database's take alone: drop the Filmweb
fill", 10-14 00:29, live by the 10-14 rollouts) matches the 10-14 12–18h step. Germany's investigation measured
the same rule batch removing about 105 TMDB ids there.

### 2–4. UK and Spain: the 2026-10-12 source expansion

Worker rollouts on 10-12 (08:23–09:38 and later) shipped `e166da09d` (43 UK Savoy Systems cinemas from their own box
office), `c46c74efa` (36 UK Spektrix venues), `c605c938a` (9 UK Ticketsolve venues), `f41a8a829` (BFI Southbank),
`2d0717ded` (21 Savoy box offices), `ac1da1739` (56 Spanish chain venues from taquilla.com, whose calendars run
months past SensaCine's) and `f49b9fb35` (10 Artesiete venues).

- uk: total 1,414 (10-12 06h) → 2,806 (10-13 00h) while `with_tmdb_id` went 1,368 → 1,650, so about 1,100 new titles
  have no TMDB id. `identity_regroupings_total{country="uk",kind="fresh"}` was 1,697 on 10-12. Worker HTTP success
  rose 36k → 126k on 10-12, and `cinema_never_content` fell 31 → 20. London's repertoire is now 1,329 titles, 261
  with no IMDb link. They are mostly festival programmes (London Breeze, LKFF, LIAF, LPFF), "+ Q&A" variants, opera
  relays and live events.
- uk decline afterwards (row 3): over 10-13 → 10-19, regroupings were fresh 373, retire 556 and merge 454: the new
  titles settling into fewer films, as in Germany. Nearly all of it came out of the no-id titles (the total fell
  about 210 while `with_tmdb_id` moved −13). The total is back up to 2,723 after today's scrapes.
- es: total 215 → 236 at 10-12 12h with `with_tmdb_id` 213 → 221. Madrid's three titles with no id are operas and a
  ballet. The share has held at about 92% since.

### 5. Spain: showtimes week over week

In 6h samples, the first week's refresh landed 10-07 00h → 12h (39k → 61k). The second landed 10-14 06h → 10-15 12h
(27k → 66k), one day later and higher. Monday 10-12 was Spain's Fiesta Nacional. Today, at 35k (10-20 06h), es is
+17% on the week before.

### 6. US: slow corpus shrink

The total went 2,234 (10-08 06h) → 2,050 (10-19 18h). The share with TMDB stayed at 97.4–97.9% and the share with
any rating at 94.0–94.6%. Regroupings 10-13 → 10-19: fresh 270, retire 306, so more films aged out than arrived,
spread across cities and without a step. Showtimes week over week are 94–104%, and listings
(`identity_cutover_listings`) dipped to 93% on 10-18 and are at 96% now. `cinema_never_content` fell 552 → 511 and
stale venues stayed at 0. The 54 closures retired on 10-12 (`2611689ae`) had no showtimes, so they removed nothing.
This is consistent with a seasonal lull in releases.

### 7. Web traffic

`increase(kinowo_web_http_requests_total[1d])` by country, route and status:

- pl 2xx per day: 16,830 (10-06) → 13,828 (10-19). The constant probe/"other" bucket held at about 11.5k a day.
  The whole decline is film detail pages, `/:city/movie/:slug`: 4,614 → 1,852 (10-12) → 1,063 (10-19). The landing
  page `/` rose (142 → about 800 a day from 10-08).
- `/:city/movies` (browse/facets) fell from 250–460 an hour across countries to 23–90 an hour at **10-09 04:00**
  (de 2.5k → 130 a day, uk 1k → about 0–150, us 6k → about 0–140). No web pod restarted then: all web pods had been
  up since 10-08 10:21. No commit touched browse, robots or Caddy then.
- Spikes, not drops: uk 37k film-page hits on 10-18 and us 100k on 10-19 were bursts.
- There are no access logs with user agents (web pods log about 150 lines a day), so the crawler attribution is
  inferred, not proven.

### 8. Poland: stale venues 1 → 8

`kinowo_worker_cinema_content_stale_venues{country="pl"}` counts venues whose last content-bearing scrape is more
than 7 days old (`CinemaContentCensus.StaleAfter`). It climbed one or two at a time (1, 1, 2, 2, 3, 3, 3, 3, 4, 6, 6,
7, 7, 8). The gauge has no venue label. These own-site hosts have failed persistently in worker-pl's log:

- `kinogornik.eurobilet.pl` (Kino Górnik): failing since 10-08; TLS certificate expired since 10-18 13:13.
- `restapi.cinemaone.pl` (Cinema1): connection errors since 10-16; certificate expired since 10-18 18:01.
- `kdkkoscierzyna.pl`: failing since 10-13.
- `artkino.rckp.krosno.pl`: HTTP errors 10-07 → 10-15.
- `www.awangarda.olsztyn.pl`: connection refused since 10-19 23:16.
- `biletyick.miastoilawa.pl`: errors 10-07 → 10-16.

Together they cover the likely members of the 8. Multikino's 403s, biletyna's 403s and Helios' errors repeat every
day of the window and did not move pl showtimes.

## Recommended actions

1. **pl coverage (row 1)**: let today's TMDB backfill finish, then re-read `with_tmdb_id`. If it stays below about
   66%, go through the roughly 20 real films with no id (list above), and fold the strand-prefixed copies ("Kino na
   obcasos:", "WSP:", "KLAPS:") into their film. Consider measuring coverage over films only (excluding
   event/relay/festival-block titles) in every country: the uk and es ratios now measure the event mix, not
   matching quality.
2. **pl stale venues (row 8)**: name the 8 venues from the read model (the census has no label; adding a venue list
   to `/admin` or a log line on each newly stale venue would make this a one-step check). For Kino Górnik and Cinema1,
   whose upstream certificates have expired, decide between a filmweb/aggregator fallback and waiting for the venue
   to renew. Retire any that have closed.
3. **us (row 6)**: no action. Re-check on 10-27; a further drop beyond about 3% a week, or a fall in the share with
   ids, would warrant a look per venue.
4. **Traffic (row 7)**: if bot traffic matters (SEO), check Search Console or Bing for the 10-09 drop in browse and
   film pages. Consider logging the user-agent class on the web tier so the next drop can be attributed.
5. **Proxy auth blip (10-20 00:00–00:20)**: one-off. Watch for a repeat at the next credential rotation.
