package services.tasks

import scala.concurrent.duration._

/**
 * Default cadence knobs for the cinema-scrape reaper, in one place so the
 * `WorkerWiring` Env defaults and the sustainability guard
 * ([[ScrapeCadenceSustainabilitySpec]]) can't drift apart.
 *
 * The reaper ticks once a minute and enqueues the cinemas due under the shared
 * [[DueWindow]] (period = `Freshness.defaultScrapeTtl`, the scrape freshness
 * window). The ~1150-cinema corpus (PL + the nationwide UK Flicks roster + DE) is
 * phase-spread across that window, so the steady-state due rate is
 * `corpus / ticksPerWindow` (~19/tick at the 60-min window). The caps only bite
 * under a backlog (cold boot, or a throttle episode
 * that parked work); in steady state far fewer than either cap is due.
 *
 * The THROTTLED cap is the lever this object exists to size correctly. While the
 * CPU-credit safety net is engaged (`ScrapeThrottleSignal`) the reaper backs off
 * to it instead of the healthy cap. It must still drain the whole catalogue
 * within one freshness window — otherwise a throttle blip parks the corpus stale
 * (a cap of 3 cleared ~290 cinemas only every ~97 ticks, i.e. ~1.5h out of date,
 * blowing freshness for every cinema). It stays well below the healthy cap so the
 * small worker pool still earns idle to rebuild credit — backing off, not keeping
 * full pace.
 */
object ScrapeCadence {
  /** ScrapeReaper tick cadence (also the phase-spread granularity). */
  val ReaperTickInterval: FiniteDuration = 1.minute

  /** Healthy per-tick enqueue cap (`KINOWO_SCRAPE_MAX_ENQUEUE_PER_TICK`). Sized so
   *  the whole catalogue clears within one freshness window with ≥1.5× headroom
   *  (`cap × ticksPerWindow ≥ corpus × 1.5`); rose 25→30 when the full nationwide
   *  UK Flicks roster took the corpus past ~1150 (60 ticks × 30 = 1800 ≥ 1159×1.5). */
  val MaxEnqueuePerTick: Int = 30

  /** Throttled cap for [[ScrapeReaper]] (`KINOWO_SCRAPE_THROTTLED_MAX_ENQUEUE_PER_TICK`).
   *  ScrapeReaper treats this as a bound on the OUTSTANDING waiting-scrape backlog
   *  (not just per-tick additions): while throttled it tops the waiting set up to
   *  this many, so the credit-starved pool drains it to near-empty and idles
   *  between ticks, rebuilding credit. Set to the SMALLEST cap the sustainability
   *  guard allows (`cap × ticksPerWindow ≥ corpus`, ~20 at the 60-min window — rose
   *  5→6 when the corpus passed 300 with the first UK + Germany cinemas, 6→8 when the
   *  Greater-London + Manchester Flicks roster took it past 450, then 8→20 when the
   *  full nationwide UK Flicks roster (~850 venues) took the corpus past 1150), so a
   *  credit-starved pool — which Fly caps to baseline, clearing only a scrape or two
   *  per minute — reaches idle as fast as the freshness guard permits. The old cap
   *  of 15 let a standing backlog of 15 keep the throttled pool pinned busy, so
   *  credit sat at the floor through active hours (measured 2026-07-03: a fresh
   *  ~17k post-deploy grant drained back to the floor in ~30 min under daytime
   *  load). Freshness during a sustained throttle is deliberately sacrificed for
   *  recovery; it catches up once the throttle clears and the full
   *  [[MaxEnqueuePerTick]] resumes. */
  val ThrottledMaxEnqueuePerTick: Int = 20

  /** How many staggered sub-slices each (non-throttled) ScrapeReaper tick enqueues
   *  the due batch in (`KINOWO_SCRAPE_ENQUEUE_SPREAD_SLICES`). The tick's clump of
   *  due cinemas otherwise fetches in parallel and PARSES together — a CPU spike that
   *  floors the shared-CPU credit balance. Spreading the SAME batch across the 1-min
   *  interval in this many groups keeps the parses from landing together (lower peak,
   *  unchanged total work and freshness — a sub-minute stagger is nothing against the
   *  60-min scrape window). 4 slices → the batch enqueues at 0/15/30/45s. 1 disables
   *  the spread. */
  val EnqueueSpreadSlices: Int = 4

  /** Per-tick enqueue cap for the SECONDARY reapers (detail/rating/tmdb-retry)
   *  while throttled (`KINOWO_THROTTLED_ENQUEUE_PER_TICK`). Their TTLs are 4–6h, so
   *  even 3/tick drains the corpus many times over within a window — and quieting
   *  them further (from 5) matters because credit only rebuilds when the WHOLE pool
   *  idles: scrapes backing off alone left the detail/rating reapers feeding it. */
  val ThrottledSecondaryEnqueuePerTick: Int = 3
}
