# Germany's corpus decline, 2026-10-12 → 2026-10-19

Investigated 2026-10-20, read-only, before that day's release (`aa334f3`, worker-de rolled out 06:58 UTC). All times
are UTC.

## Summary

Germany's (de) film corpus fell from 5,033 films (peak at 2026-10-12 07:00; the 10-12 figure was quoted as 5,014) to
4,744–4,751 (2026-10-18 20:00 to 10-19 08:00). Films with a TMDB id fell from 3,250 (10-14 15:00) to 3,144 (10-15
15:00) and then to 3,088–3,095 (10-19 03:00–08:00). Both recovered once the new cinema week's programmes arrived:
4,806 films and 3,125 with a TMDB id at 10-20 03:00.

**Verdict: mostly expected, with one deliberate change (not an outage).** Cinemas did not drop out, and TMDB and IMDb
did not fail. The decline has three causes:

| Cause | Films (`total`) | Films with a TMDB id | Expected? |
|---|---:|---:|---|
| 1. Identity settling after the 10-11/12 roster expansion (listings regrouped and merged into fewer films) | **−223** (79% of the −282 net) | +23 | yes: a one-off |
| 2. Weekly listing horizon (the week's programme runs out before next Thursday's is published) | net **≈ −59** (21%): +94 on 10-13/14, −130 over 10-15→10-19 | **≈ −50** (32% of −157) | yes: weekly |
| 3. Identity rule tightening deployed 10-14/15 (46 worker-de builds, ending with `8b75b70`): listings refused or denied their TMDB film | ≈ −22 (rows are kept, only their TMDB id is removed) | **≈ −105** (67% of −157) | intended, but includes some false negatives |
| Cinemas dropping out | 0 | 0 | — |
| TMDB / IMDb outage | 0 | 0 | — |
| Unscreened cleanup (rows with no current screenings) | −6 | ≈ −6 | yes |

## Timeline

Daily values at 03:00 UTC. Sources: `kinowo_worker_corpus_movies{country="de"}` (`total`, `with_tmdb_id`),
`kinowo_worker_identity_cutover_listings`, `sum(kinowo_worker_showtimes)`, `sum(kinowo_worker_movies_served{scope="all"})`,
`kinowo_worker_cinema_never_content`, and the 24h increase of `kinowo_worker_identity_regroupings_total` by `kind`.

| Day (03:00) | Weekday | Films | With TMDB id | Listings | Upcoming showtimes | Served (city×film) | Venues never with content | Retired (24h) | Merged (24h) | Fresh + split (24h) |
|---|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 10-08 | Thu | 1,949 | 1,904 | 19,619 | 97,750 | 9,737 | 326 | 75 | 1 | 59 |
| 10-09 | Fri | 1,962 | 1,915 | 19,571 | 89,180 | 9,891 | 326 | 56 | 1 | 70 |
| 10-10 | Sat | 1,942 | 1,894 | 19,736 | 78,810 | 9,918 | 326 | 68 | 2 | 50 |
| 10-11 | Sun | 1,914 | 1,866 | 19,287 | 64,952 | 9,566 | 326 | 72 | 1 | 45 |
| 10-12 | Mon | 4,054 | 2,786 | 27,183 | 75,163 | 21,651 | 314 | 386 | 95 | 2,622 |
| 10-13 | Tue | 4,817 | 3,223 | 33,837 | 139,122 | 27,153 | 124 | 664 | 117 | 1,501 |
| 10-14 | Wed | 4,865 | 3,256 | 34,659 | 145,784 | 27,643 | 123 | 147 | 51 | 231 |
| 10-15 | Thu | 4,860 | 3,181 | 33,921 | 138,702 | 27,253 | 122 | 168 | 38 | 196 |
| 10-16 | Fri | 4,837 | 3,129 | 34,212 | 129,199 | 27,645 | 122 | 140 | 48 | 149 |
| 10-17 | Sat | 4,845 | 3,153 | 34,364 | 117,475 | 27,734 | 121 | 111 | 143 | 254 |
| 10-18 | Sun | 4,759 | 3,102 | 33,662 | 103,352 | 27,037 | 121 | 124 | 20 | 49 |
| 10-19 | Mon | 4,751 | 3,095 | 31,834 | 89,274 | 25,742 | 121 | 149 | 9 | 150 |
| 10-20 | Tue | 4,806 | 3,125 | 33,738 | 138,219 | 27,257 | 121 | 107 | 41 | 191 |

The 10-12 03:00 row was taken mid-expansion: the first projection of the grown roster built the corpus from 3,489 films
(00:00) to 5,033 (07:00), with 30,531 listings.

Key hourly points:

- 2026-10-12 07:00: 5,033 films, the peak (30,531 listings).
- 2026-10-13 03:00: 4,809 films while listings *rose* to 33,937. This is settling (cause 1).
- 2026-10-14 15:00: 4,903 films, 3,250 with a TMDB id.
- 2026-10-15 11:00→12:00: with a TMDB id 3,179 → 3,130 (−49 in one hour, after build `2900ea1` rolled out at 11:16).
- 2026-10-15 15:00: 4,881 films, 3,144 with a TMDB id.
- 2026-10-18 20:00: 4,744 films and 3,088 with a TMDB id, the low; 10-19 03:00 was 4,751 / 3,095 with 31,831 listings.
- 2026-10-20 03:00: 4,804 films, 3,125 with a TMDB id, 33,795 listings (the new week's programmes are in).

## Causes and evidence

### Accounting check

The identity projection changes the film count only by regrouping:
Δfilms ≈ fresh + split − merge − retire (`kinowo_worker_identity_regroupings_total`). This identity matches the corpus
gauge in every window, and the gap comes from counter resets across the 46 pod restarts:

| Window | fresh | split | merge | retire | move | predicted Δ | actual Δ films | Δ with TMDB id | Δ listings |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 10-12 08:00 → 10-13 03:00 | 209 | 33 | 58 | 419 | 809 | −235 | −216 | +25 | +3,400 |
| 10-13 03:00 → 10-14 15:00 | 296 | 52 | 74 | 205 | 615 | +69 | +85 | +26 | +700 |
| 10-14 15:00 → 10-15 15:00 | 124 | 30 | 34 | 160 | 86 | −40 | −27 | −105 | −620 |
| 10-15 15:00 → 10-19 03:00 | 352 | 168 | 201 | 473 | 429 | −154 | −124 | −49 | −2,193 |
| 10-19 03:00 → 10-20 06:00 | 176 | 30 | 45 | 118 | 187 | +43 | +58 | +29 | +1,960 |

Film rows were removed almost entirely by the projection. From the removal-audit log (`logger:"kinowo.removal-audit"`,
`country:de`), 10-13 → 10-20 09:00: 861 `identity.projection … retired-by-overlap` deletions, 6
`unscreened-cleanup … no-current-screenings`, 1 `cache.applyDelete`. The removal-audit log has no DE retire lines
before 10-13 22:57, the first build to carry the `build` label. The read-model card prunes
(`kinowo_worker_readmodel_films_pruned_total`) were negligible apart from 353 `variant-gone` cards on 10-12, which
come from the same expansion.

### 1. Identity settling after the roster expansion: −223 films (79%)

- On 10-11 Germany moved to per-venue sources. `b726cda86` added KinoheldClient, `5a0cd62d9` added the kinoheld
  harvest, and `8a73e77a9` switched to town pages. On 10-12, `2611689ae` retired closed venues and `792a54d33` noted
  "Germany's per-venue sources fill its corpus". Cities with showtimes went from 151 to 437, and venues that had never
  had content went from 326 to 124.
- The first projection over the grown roster put 30.5k listings into 5,033 films. In the next 19 hours listings grew
  by another 3,400, yet the film count fell by 223. Over that window there were 809 listing **moves**, 58
  **merges**, and 419 **retirements**. A retirement here is a film whose listings all moved to another film; it is
  not a film leaving the schedule, because the listing count was rising. This is deduplication. The cold
  projection split titles that the larger body of evidence later put back together.
- The TMDB count *rose* (+25) in this window, so the merged rows were mostly duplicate unresolved stubs.

### 2. Weekly listing horizon: ≈ −59 net films, ≈ −50 TMDB films (≈ 32% of the TMDB drop)

- German cinema weeks run Thursday to Wednesday. Most venues publish the next week's programme on Monday or Tuesday,
  so a venue's listing horizon is shortest from Sunday to Monday morning. Upcoming showtimes follow a clean sawtooth:
  97.7k (10-08, Thu) → 65.0k (10-11, Sun); 145.8k (10-14, Wed) → 89.3k (10-19, Mon) → 138.2k (10-20, Tue). Listings
  went 34,659 → 31,834 → 33,738.
- In 10-15 15:00 → 10-19 03:00, listings fell by 2,193 (−6.4%) and the projection retired 473 films against 352
  fresh ones. Net merges were small (201 merges vs 168 splits, −33). Films without a TMDB id fell by 81 and films
  with one by 49. The resolved share actually *rose*, from 64.4% to 65.1%, which is what churn at the
  unresolved tail looks like.
- The pre-expansion weeks show the same shape: 2,047 (10-02, Fri) → 1,922 (10-04), and 1,962 (10-09) → 1,914 (10-11).
  That is a 2–6% weekly swing. On the grown roster the swing is 4,903 → 4,744 (−3.2%).
- It reversed on schedule. From 10-19 03:00 to 10-20 06:00 listings grew by 1,960 and films by 58, with 29 more
  carrying a TMDB id.

### 3. Identity rule tightening on 10-14/15: ≈ −105 TMDB films (67% of the TMDB drop)

- worker-de went through 46 builds between 10-13 22:57 and 10-15 23:43. The ReplicaSet history shows `8b75b70`
  (`main-20261015233458-8b75b70`) as the last of them, and it ran until `aa334f3` at 10-20 06:58. The 10-16→10-20
  freeze was the GitHub suspension, not a choice. There are 503 commits in `89ced660a..8b75b704b`. The ones that
  matter are a run of resolver tightenings: "Take no film …", "Deny …", "Refuse a bare take …". Examples are
  `147c581d6` (take a search's only film only when the billing touches each part of its title by half or more),
  `e40e9a4af`, `b88faf7ea`, `6f48b2038`, `9a0a1c26c`, `aa10200b8`, `90c14090c`, `138b55d52`, `7864e9058`,
  `dcc3eab6d` (a festival billed by its own name is not a film), and `94a47eaba`.
- In 10-14 15:00 → 10-15 15:00 the TMDB count fell by 105 while the film count fell by only 27, so rows stayed and
  lost their TMDB id. `kinowo_worker_identity_take_changes_total{kind="lost"}` was 150, 150, 75 and 142 per 12h
  from 10-13 15:00 to 10-15 15:00, against 4–15 per 12h after `8b75b70` settled. Logged take changes
  (`identity take change: lost …`) between 10-14 12:00 and 10-15 16:00 named 136 distinct TMDB films. 98 of them
  were not regained by any listing by 10-19 03:00, which matches the −105.
- Lost takes by build: `0690fe7` 77, `2900ea1` 64, `83a8abd` 47, `7db2cf5` 40, `26e4889` 21. Lost takes by reason:
  `BelowThreshold` 108+, `Vetoed` 102, and `Event: event, not a film: esports …` 63 (correct: e-sports screenings).
  There is also a long tail of `no film: … one piece of the billing … is billed beside …` (a guest name or series
  banner next to the film).
- Some of the losses are false negatives worth checking: CineStar Dortmund "Lieblingsmenschen" (tmdb 788385 →
  none, BelowThreshold), Capitol Albstadt "Met Opera 2026/27: … LA FANCIULLA DEL WEST" (tmdb 1703629 → none),
  CinemaxX Essen "ENHYPEN WORLD TOUR 'BLOOD SAGA'" (tmdb 1785968 → none), and "Frühstück – Heimsuchung" /
  "Visitation".

### Ruled out

- **Cinemas dropping out.** ScrapeCinema tasks finished 3,656–4,309 per day as `done`, with 0 `failed`, 0
  `exhausted`, and 0–27 `skipped`. `cinema_content_stale_venues` was 0 from 10-13 (49 on 10-12, before the new
  venues' first scrapes). `cinema_scrape_oldest_age_seconds` was flat at about 10h, which is the cadence.
  `scrape_listing_incomplete_streak_venues` was 0 and `cinema_scrape_never_scraped` was 0. Cities with showtimes
  stayed at 433–438 every day. The 121 venues with zero listings (`cinema_never_content`) were flat from 10-16 on
  (124 on 10-13). That is a backlog of venues that never produced content, not new drop-outs. Breadth and depth
  scrape-guard rejects were 1–4 per day.
- **TMDB / IMDb outage.** Enrich-phase HTTP (`kinowo_worker_http_total{phase="enrich"}`) had 77k–209k successes per
  day, 2–10 5xx per day, 2–146 timeouts per day (peak 10-13), and 429s of 402 and 172 on 10-12/13 (the expansion
  backlog) and ≤30 per day after that. The 404 rate held at about 10% (search misses). Breaker opens were 16–18 per
  day on 10-12/13 and 1–2 per day afterwards. Nothing lines up with the TMDB drop.
- **Unscreened cleanup.** 6 films in the whole window.
- **Merges as such.** Net merges after settling (merge − split) were −33 over 10-15→10-19, about 12% of the decline
  over that window, which is ordinary identity churn.

## Verdict

- The total-film decline is **expected**. About 80% of it is a one-off: identity settling after the 10-11/12 German
  roster expansion. The other ≈20% is the weekly horizon trough, which reversed on 10-19/20.
- About a third of the TMDB-id decline is the weekly horizon. About two thirds is the **intended** effect of the
  10-14/15 identity rule tightening (`8b75b70` and its predecessors). That part is not a cinema or upstream problem.
  It is a precision-for-recall trade: some of it is correct (e-sports, festival billings, guest-titled events), and
  some is likely false negatives on real films (above).
- Today's release (`aa334f3`) carries identity changes too, for example `aa6d8e682` ("key a venue's rows as its
  listings are keyed, credits from the title included") and the 10-19 V3f film-ness changes. Judge it against these
  baselines: about 4,800 films and 3,125 with a TMDB id on a Tuesday.

## Recommended actions

1. **Triage the 98 TMDB films lost and not regained.** Re-run the take-change extraction (LogsQL
   `namespace:kinowo country:de "identity take change:" lost`, 10-14 12:00 → 10-15 16:00) and judge the
   `BelowThreshold` and `Vetoed` cases with `scripts/identity-slice`. Starting cases: Lieblingsmenschen (788385), the
   Met Opera relays, and Enhypen. File each real film as a pinned case.
2. **Alert on take losses, not corpus size.** A burst of `identity_take_changes_total{kind="lost"}` above about 100
   per 12h with `total` flat is the signature of a rule change de-resolving films. The corpus total alone cannot tell
   that apart from the weekly trough.
3. **Compare week over week, same weekday.** Any future DE "decline" check should compare Monday 03:00 with the
   previous Monday 03:00. Thursday-to-Monday falls of 3–6% in films and about 40% in upcoming showtimes are normal.
4. **Batch identity deploys.** 46 worker-de restarts in 49 hours reset the counters, which made the accounting
   above approximate, and replayed projections each time. Batching makes changes in take losses attributable to a
   single build.
5. **Watch today's release.** Re-check `with_tmdb_id` and the lost-take rate 12h and 24h after the 06:58 rollout.
   Expect 3,125±30 on 10-20/21, rising to its weekly high by Wednesday.
