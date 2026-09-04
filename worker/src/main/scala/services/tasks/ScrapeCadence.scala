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
 * under a backlog (a cold boot, or an episode that parked work); in steady state
 * far fewer than either cap is due.
 *
 * The caps below are sized so that, even at their most conservative, the reaper
 * still clears the whole catalogue within one freshness window.
 */
object ScrapeCadence {
  /** ScrapeReaper tick cadence (also the phase-spread granularity). */
  val ReaperTickInterval: FiniteDuration = 1.minute

  /** Healthy per-tick enqueue cap (`KINOWO_SCRAPE_MAX_ENQUEUE_PER_TICK`). Sized so
   *  the largest PER-COUNTRY catalogue clears within one freshness window with
   *  ≥1.5× headroom (`cap × ticksPerWindow ≥ corpus × 1.5`). Post the per-country
   *  worker split each machine scrapes ONE country, so the binding corpus is the
   *  largest country, not the global sum. Rose 25→30 for the nationwide UK Flicks
   *  roster, then 30→40 when the full German Filmstarts roster (1,529 cinemas) made
   *  Germany the largest country (60 ticks × 40 = 2400 ≥ 1529 × 1.5 = 2294). */
  val MaxEnqueuePerTick: Int = 40

  /** How many staggered sub-slices each ScrapeReaper tick enqueues
   *  the due batch in (`KINOWO_SCRAPE_ENQUEUE_SPREAD_SLICES`). The tick's clump of
   *  due cinemas otherwise fetches in parallel and PARSES together — a CPU spike that
   *  floors the shared-CPU credit balance. Spreading the SAME batch across the 1-min
   *  interval in this many groups keeps the parses from landing together (lower peak,
   *  unchanged total work and freshness — a sub-minute stagger is nothing against the
   *  60-min scrape window). 4 slices → the batch enqueues at 0/15/30/45s. 1 disables
   *  the spread. */
  val EnqueueSpreadSlices: Int = 4

  /** Spread window for a chunked venue's `ScrapeChunk` fan-out
   *  (`KINOWO_SCRAPE_CHUNK_SPREAD_MINUTES`). A chunked venue coming due enqueues one
   *  `ScrapeChunk` per advertised day of its horizon — up to ~200 for a full-horizon
   *  UK Flicks venue — which, enqueued all at one instant, becomes claimable in a
   *  single burst that pins the whole worker pool under the strict oldest-first claim
   *  and starves the evenly-enqueued rating refreshes behind it (head-of-line
   *  blocking). Staggering each chunk's `nextEligibleAt` evenly across this window
   *  bounds how many of a venue's chunks are claimable at once, so free workers fall
   *  through to ratings between them — the chunk-level counterpart to the venue-level
   *  spread [[ScrapeReaper]] already applies via [[DueWindow]] + `EnqueueSpreadSlices`.
   *  Kept well under the chunk-run stale timeout ([[ChunkScrapePlanner.DefaultRunTimeout]],
   *  15min) — the planner clamps it to a third of that — so every chunk still becomes
   *  eligible AND drains before the run is abandoned to a partial reduce. */
  val ChunkEnqueueSpread: FiniteDuration = 5.minutes

  /** Ceiling on outstanding scrape TASKS — ScrapeCinema + ScrapeChunk +
   *  ScrapeChunkReduce — that the [[ScrapeReaper]] will let build up
   *  (`KINOWO_SCRAPE_MAX_OUTSTANDING_TASKS`). The only cap here counted in units of
   *  WORK rather than units of VENUE, and the one that holds on the healthy path.
   *
   *  It exists because a venue is not a unit of work on a chunked country. A UK Flicks
   *  venue fans out ~36 `ScrapeChunk` fetches, so a healthy tick at
   *  [[MaxEnqueuePerTick]]=40 is ~1,440 tasks — about 20 minutes of work for a
   *  4-worker pool paced at 5 req/s — enqueued at a single instant. The TOTAL is
   *  comfortable (UK needs ~72 tasks/min against a ~300/min ceiling, a 24% duty cycle);
   *  it is purely the burst that floors the credit balance. Restarts made it
   *  self-perpetuating: a restart cleared the accounting, so the worker read healthy,
   *  dumped a full batch, and was already carrying it when the box fell over again.
   *
   *  150 caps a burst at ~30s of pool work, leaving the rest of the tick idle for
   *  credit to rebuild, while sitting far above the ~72/min the corpus actually needs —
   *  so it bounds bursts without constraining steady-state throughput or freshness. */
  val MaxOutstandingScrapeTasks: Int = 150
}
