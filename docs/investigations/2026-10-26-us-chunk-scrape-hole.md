# US chunked scrapes go idle for ~3 h every ~3 days (2026-10-26), open for pickup

**Status.** Diagnosed down to the scheduler, but the root cause is NOT proven. Nothing user-visible is broken. This
note is a brief for whoever picks it up. Everything below was read from Prometheus (`monitoring-1`, `10.20.0.11:9090`)
and VictoriaLogs (`10.20.0.11:9428`, via `ssh mongo-1 "curl ..."`). Nothing in prod was changed.

## What was seen

The trigger was worker-diagnostics panel 33 ("Read-model venues per projection — rebuilt vs reused"). The US `reused`
count fell from ~250k to ~0 per 15 min, 2026-10-26 11:40 → 15:00 UTC, and recovered by ~17:00. The other countries
rose in the same window.

The chain, link by link:

| layer | before (per 15 min unless noted) | 12:15–15:00 | source |
|---|---|---|---|
| venue projections, reused | ~250k | 0–80k | `kinowo_worker_readmodel_venue_projections_total{country="us"}` |
| change applies | ~200 | 0–40 | `kinowo_worker_change_apply_total` |
| `ScrapeChunk` finished, per hour | ~8,600 | ~330 | `kinowo_worker_tasks_finished_total{task_type="ScrapeChunk"}` |
| chunks planned, per 10 min | ~1,400 | 0–3 | `ChunkScrapePlanner` "enqueued N/N chunk task(s)" log lines |
| EnrichDetails / ratings | flat | flat | same metric |

- The projection path was healthy. The periodic project calls kept running, and rebuilt never tracked reused.
- The worker was up and there were no WARN/ERROR lines about scraping. The queue was nearly EMPTY (waiting ~0), so the
  `maxOutstandingScrapeTasks` budget was not what held admission back.
- The `ScrapeReaper` found almost nothing DUE: ~1 venue a minute. So the pool idled because no chunked venue was due.
- It is not a deploy: the stall began at ~11:40, before the 11:5x shutdown, and the 13:22 restart did not change it.
- Freshness did not suffer: `kinowo_worker_cinema_scrape_oldest_age_seconds{country="us"}` stayed ~73–76k s.

## It recurs

Hours where US `ScrapeChunk` throughput fell below 30% of its 24 h average, from 10-12 to 10-26:

- 10-20 12:00–15:00
- 10-23 10:00–13:00
- 10-26 13:00–16:00

All three are ~3 days apart, around midday UTC, US only (DE showed one unrelated dip on 10-12). Each looks the same:
normal planning, a short partial slot, ~2.5 h of near-zero chunk planning, then a ramp back up over ~1 h.

10-26 had one extra feature. At 11:30–11:50 ~600 of the ~840 UNCHUNKED US venues (US has 4,999 venues; 4,161 are
chunked) came due at once: 596 "Refreshed X: N entries in ~10ms" lines from `CinemaScrapeRunner`, against ~40 per
10 min normally. That came right after the hourly `ScrapePhasePlanner` replan at 11:32:17 (typical cost
16.4 → 16.2 tasks). So in the same window one cohort's due times moved EARLIER (a burst) and the chunked cohort's moved
LATER (a hole).

## Hypothesis (unproven): a replan shifts a block of phases, and nearest-boundary counting turns the shift into a hole

- `CostSpacedPhaseOffset.fractions` gives each venue a phase equal to the running share of `cost / period` of every
  venue ahead of it in hashed order. A change in one venue's cost or period therefore moves the phase of EVERY venue
  after it. Every hourly replan (`ScrapePhasePlanner`, `ReplanInterval = 1.hour`) installs a fresh plan. Inputs that
  move: `ScrapeCostStore.RecentRuns = 5` runs per venue, and `VenueCadenceStore` overrides. The overrides are NOT
  persisted, so a boot replans with every thin venue on the country default (US 840 min) and later replans relearn them.
- `DueBoundary.NearestBoundary` counts a scrape toward the boundary NEAREST it. Move a venue's phase later by Δ < P/2
  just after it was scraped, and that scrape now counts toward the new, upcoming boundary, so the venue is not due
  until the boundary after it. Shift a contiguous block later by Δ and its scrapes stop for Δ (a hole). Shift it
  earlier and it falls due at once (a burst). Both shapes were seen on 10-26.
- The ~3-day spacing is unexplained. Two coincidences to test: 5 recent runs × 14 h period ≈ 70 h, and the weekly
  shape of US chain horizons (one chunk per advertised day, so every venue's fan-out changes with the weekday).

## What to do

1. **Prove or kill the hypothesis (do this first).** Recompute successive plans and look for a large, contiguous phase
   jump at the replans just before 11:40 on 10-26 (and 10-23 ~10:00, 10-20 ~11:20). Options:
   - Rebuild the plans offline. Feed `CostSpacedPhaseOffset.fractions` the costs from `scrape_costs` in the local prod
     mirror (`kinowous_prod_mirror` on `:28017`, READ-ONLY, after `scripts/local-mirror/freshness us`). The mirror
     holds only the current costs, so this shows the plan's sensitivity, not the history.
   - Or instrument the replan (cheap, and worth landing either way): log or export, per replan, the median and max
     |Δphase| in minutes, and how many venues moved more than 10 min, earlier and later. One replan with a block of
     ~hundreds of venues shifted by ~hours would confirm it.
   - Also find which venues were due at 11:30–11:50 (the burst) and which became due at ~14:00–15:00 (the recovery).
     If they sit at contiguous positions in the hashed order, the shift came from one venue's cost or period change
     ahead of them.
2. **If confirmed, damp the shift.** Candidates, smallest first:
   - Keep each venue's previous fraction unless the new one differs by more than a threshold, or move each phase at
     most X min per replan. This bounds Δ, and with it the hole.
   - Persist the `VenueCadenceStore` overrides (or leave them out of the phase loads), so a boot's replan does not
     move phases that the next replans move back.
   - Re-check the `NearestBoundary` choice for chunked venues: is a 1.5-period worst-case gap
     (`longestGapPeriods = 1.5`) acceptable on a 14 h cadence?

   Whatever changes needs a unit test that installs plan A, scrapes, installs a shifted plan B, and asserts no venue
   goes undue for longer than the bound. Extend the `CostSpacedPhaseOffset` / `DueWindow` specs.
3. **Visibility (small, independent).** Panel 33 dropping read as a projection problem, and it took ~20 queries to
   show it was "no scrape work due". Add a reaper gauge for the size of the due set (and for admitted vs held by
   budget). Put it, or US `ScrapeChunk` enqueues, next to panel 33, and mention it in panel 33's description. Also add
   a scrape-age p50/p90: the oldest-age gauge alone (~21 h on a 14 h cadence, presumably parked or gone venues) cannot
   tell "everything fresh" from "a cohort aging".

## Not the cause (ruled out)

- The read-model projection or its input hash: rebuilt stayed low and the sweeps ran.
- The worker being down, deploys, or queue budget starvation: the queue was empty.
- Upstream fetch failures: no scrape WARN/ERROR lines and no guard rejects.
- Time of day: on 10-24 and 10-25 the change-apply rate at the same hours was normal.
