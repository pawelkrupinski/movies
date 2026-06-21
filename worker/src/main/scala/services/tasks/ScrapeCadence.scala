package services.tasks

import scala.concurrent.duration._

/**
 * Default cadence knobs for the cinema-scrape reaper, in one place so the
 * `WorkerWiring` Env defaults and the sustainability guard
 * ([[ScrapeCadenceSustainabilitySpec]]) can't drift apart.
 *
 * The reaper ticks once a minute and enqueues the cinemas due under the shared
 * [[DueWindow]] (period = `Freshness.defaultScrapeTtl`, the scrape freshness
 * window). The ~290-cinema corpus is phase-spread across that window, so the
 * steady-state due rate is `corpus / ticksPerWindow` (~10/tick at the 30-min
 * window). The caps only bite under a backlog (cold boot, or a throttle episode
 * that parked work); in steady state far fewer than either cap is due.
 *
 * The THROTTLED cap is the lever this object exists to size correctly. While the
 * CPU-credit safety net is engaged (`ScrapeThrottleSignal`) the reaper backs off
 * to it instead of the healthy cap. It must still drain the whole catalogue
 * within one freshness window — otherwise a throttle blip parks the corpus stale
 * (the old cap of 3 cleared ~290 cinemas only every ~97 ticks, i.e. ~1.5h out of
 * date, blowing freshness for every cinema). It stays well below the healthy cap
 * so the small worker pool still earns idle to rebuild credit — backing off, not
 * keeping full pace.
 */
object ScrapeCadence {
  /** ScrapeReaper tick cadence (also the phase-spread granularity). */
  val ReaperTickInterval: FiniteDuration = 1.minute

  /** Healthy per-tick enqueue cap (`KINOWO_SCRAPE_MAX_ENQUEUE_PER_TICK`). */
  val MaxEnqueuePerTick: Int = 25

  /** Per-tick enqueue cap while CPU-credit throttled
   *  (`KINOWO_SCRAPE_THROTTLED_MAX_ENQUEUE_PER_TICK`). Sized to still drain the
   *  catalogue within one freshness window (15 × 30 ticks = 450 ≥ ~290) so a
   *  throttle episode keeps pace with the freshness setting instead of falling
   *  ~1.5h behind, while staying below [[MaxEnqueuePerTick]] so the pool idles. */
  val ThrottledMaxEnqueuePerTick: Int = 15

  /** Per-tick enqueue cap for the SECONDARY reapers (detail/rating/tmdb-retry)
   *  while throttled (`KINOWO_THROTTLED_ENQUEUE_PER_TICK`). Their TTLs are 4–6h,
   *  so even 5/tick drains the corpus many times over within a window. */
  val ThrottledSecondaryEnqueuePerTick: Int = 5
}
