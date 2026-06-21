package services.tasks

import models.MovieRecord
import play.api.Logging
import services.Stoppable
import services.cinemas.DetailEnricher
import services.events.{EventBus, MovieDetailsComplete}
import services.freshness.{FreshnessKind, FreshnessStore}
import services.movies.{CacheKey, MovieCache}
import services.schedule.{AlwaysClaimScheduledRunStore, OccurrenceKey, ScheduledRunStore}
import tools.DaemonExecutors

import java.time.{Clock, Instant}
import java.util.concurrent.{ScheduledExecutorService, TimeUnit}
import scala.concurrent.duration._
import scala.util.Try

/**
 * Periodically re-enqueues `EnrichDetails` tasks for every deferred cinema's
 * current films whose detail is stale — the refresh/retry backstop the
 * event-driven [[DetailTaskEnqueuer]] can't provide. `CinemaMovieAdded` fires
 * only on a film's first appearance, so without this a detail fetch that failed
 * the first time would never retry, and a cinema whose showtime-bearing fetch is
 * itself deferred (Rialto) would never refresh its showtimes after the first
 * fetch. This is the detail-side analogue of [[ScrapeReaper]] /
 * [[EnrichmentReaper]].
 *
 * It is ALSO the backstop for the `detailPending` gate: a film a deferred cinema
 * scrapes is held back (`detailPending`, out of the read model + the TMDB stage)
 * until its detail lands and `EnrichDetailsHandler` publishes
 * `MovieDetailsComplete`. If that detail can NEVER complete — the page is gone,
 * the row has no deferred slot/`filmUrl` anymore, or the completion event was
 * lost across a restart while the detail is already fresh — the row would
 * otherwise stay invisible forever (the daily TMDB sweep deliberately skips
 * `detailPending` rows). `reapStuckPending` releases any `detailPending` row with
 * no outstanding (enqueueable, not-yet-fresh) detail: it clears the flag and
 * publishes `MovieDetailsComplete`, so the row finally resolves.
 *
 * Walks the cache like `EnrichmentReaper`: for each row carrying a slot for a
 * deferred cinema with a `filmUrl`, enqueue (deduped + freshness-gated, so a
 * film already fresh — or already waiting/working — isn't re-queued).
 */
class DetailReaper(
  enrichers: Seq[DetailEnricher],
  cache:     MovieCache,
  queue:     TaskQueue,
  freshness: FreshnessStore,
  bus:       EventBus,
  // The shared per-row refresh schedule, phase-spread across its period (6h, the
  // DetailEnrich TTL) exactly like [[EnrichmentReaper]] / [[ScrapeReaper]]. The
  // SAME instance must back [[EnrichDetailsHandler]] so this enqueue gate and that
  // pickup gate agree on "due" — otherwise the reaper re-enqueues every tick a
  // task the handler skips as still-fresh, churning the queue (see [[DueWindow]]).
  // The phase offset (hashed from each row's dedup key) is what stops a
  // synchronized cohort — a re-key / title-rule wave that orphans a whole batch's
  // freshness stamps at once — from all coming due in the SAME tick. Before this,
  // DetailReaper gated on a raw rolling TTL with no phase offset, so such a cohort
  // dumped its whole backlog in one tick (~1k `EnrichDetails` observed in prod),
  // and each completion cascaded into `ResolveTmdb` + rating tasks, spiking the
  // shared-CPU credit balance to zero.
  dueWindow: DueWindow = new DueWindow(6.hours),
  // How often the reaper wakes to enqueue the now-due slice — the spread
  // granularity (smaller = flatter trickle, at the cost of cheap in-memory scans).
  // BY-NAME + a self-rescheduling tick so an `/admin/config` interval flip applies
  // mid-flight on the next cycle, without a restart.
  tickInterval: => FiniteDuration = DetailReaper.DefaultTickInterval,
  // A small spacing before the first tick (0 in tests that drive `tick` directly).
  initialDelay: FiniteDuration = 0.seconds,
  // Cap on enqueues per tick — the backstop the phase spread can't provide for a
  // COLD cohort (every re-keyed row is "never refreshed" → due at once). Bounds
  // that recovery burst the way every other reaper does; the leftover stays due
  // and drains over the next ticks. Default unbounded so tests driving `tick` are
  // unaffected; the wiring sets a finite cap. BY-NAME: read live each tick.
  maxEnqueuePerTick: => Int = Int.MaxValue,
  // While the worker is CPU-credit throttled, cap enqueue to this trickle so this
  // reaper stops feeding the pool — credit only rebuilds when the WHOLE pipeline
  // quiets and the pool idles (see ScrapeThrottleSignal.cap). Default unbounded.
  throttledMaxEnqueuePerTick: => Int = Int.MaxValue,
  throttle: ScrapeThrottleSignal = ScrapeThrottleSignal.AlwaysHealthy,
  runStore:  ScheduledRunStore = AlwaysClaimScheduledRunStore,
  clock:     Clock = Clock.systemUTC()
) extends Stoppable with Logging {

  private val scheduler: ScheduledExecutorService = DaemonExecutors.scheduler("detail-reaper")

  def start(): Unit = {
    if (enrichers.isEmpty) { logger.info("DetailReaper: no deferred cinemas; not starting."); return }
    scheduleNext(initialDelay)
    logger.info(s"DetailReaper started over ${enrichers.size} deferred cinema(s): each detail refreshed once per " +
                s"${dueWindow.period.toHours}h, phase-spread over ticks every ${tickInterval.toSeconds}s.")
  }

  /** Self-rescheduling tick: run, then schedule the next reading `tickInterval`
   *  afresh, so an interval flip applies on the next cycle. */
  private def scheduleNext(delay: FiniteDuration): Unit = {
    scheduler.schedule(new Runnable {
      def run(): Unit = { Try(tickIfClaimed()); scheduleNext(tickInterval) }
    }, delay.toMillis, TimeUnit.MILLISECONDS)
    ()
  }

  /** Run the detail tick + stuck-pending release only if this machine wins the
   *  current window's occurrence claim — otherwise another machine is handling
   *  this window, so skip. Returns the number of detail tasks enqueued (0 when
   *  the claim was lost). Package-private so tests can drive it directly. */
  private[tasks] def tickIfClaimed(): Int = {
    val key = OccurrenceKey.at("detail", clock.millis(), tickInterval, 0.seconds)
    if (runStore.claim(key)) { val n = tick(); reapStuckPending(); n } else 0
  }

  /** Enqueue every now-due `(deferred-cinema, film)` detail, keyed off the row's
   *  CURRENT CacheKey (so it's robust to a row that was re-keyed since its
   *  `CinemaMovieAdded` fired), up to `maxEnqueuePerTick`. Public so tests / the
   *  fixture harness can drive one pass directly, with an injectable `nowMillis`
   *  so tests can advance time. Returns how many tasks were enqueued. */
  def tick(nowMillis: Long = clock.millis()): Int = {
    val now      = Instant.ofEpochMilli(nowMillis)
    val cap      = ScrapeThrottleSignal.cap(throttle, maxEnqueuePerTick, throttledMaxEnqueuePerTick)
    var enqueued = 0
    val rows = cache.entries.iterator
    while (rows.hasNext && enqueued < cap) {
      val (key, record) = rows.next()
      val es = enrichers.iterator
      while (es.hasNext && enqueued < cap) {
        val e = es.next()
        e.nativeDetailRef(record).foreach { ref =>
          if (EnrichDetailsTasks.enqueueIfDue(queue, freshness, dueWindow, e, key, ref, now)) enqueued += 1
        }
      }
    }
    if (enqueued > 0) logger.info(s"DetailReaper enqueued $enqueued due detail(s).")
    enqueued
  }

  /** Release any `detailPending` row that has no outstanding detail to fetch —
   *  its detail is already fresh (the completion event was lost) or it has no
   *  deferred slot/`filmUrl` to enrich at all (orphaned flag). Clears the flag
   *  and re-triggers TMDB via `MovieDetailsComplete`, so the row stops being held
   *  out of the read model. Returns how many were released. Scheduled (not run by
   *  the fixture harness's `enrichDetailsSync`, which only calls `tick`). */
  def reapStuckPending(): Int = {
    var released = 0
    cache.entries.foreach { case (key, record) =>
      if (record.detailPending && !detailOutstanding(key, record)) {
        cache.putIfPresent(key, _.copy(detailPending = false))
        val row = cache.get(key)
        bus.publish(MovieDetailsComplete(
          key.cleanTitle, key.year,
          row.flatMap(_.cinemaOriginalTitle),
          row.map(_.director).filter(_.nonEmpty).map(_.mkString(", "))))
        released += 1
      }
    }
    if (released > 0) logger.info(s"DetailReaper released $released detail-pending row(s) with no outstanding detail.")
    released
  }

  /** True when a deferred cinema still owes this row a detail fetch — it has a
   *  native (fetchable) `filmUrl` slot whose detail isn't fresh yet. While true
   *  the row legitimately stays `detailPending` (and `tick` keeps the fetch
   *  enqueued). A Filmweb-fallback row's filmweb.pl URL is NOT native, so such a
   *  row is never "outstanding" and `reapStuckPending` releases it. */
  private def detailOutstanding(key: CacheKey, record: MovieRecord): Boolean =
    enrichers.exists { e =>
      e.nativeDetailRef(record).isDefined &&
        !freshness.isFresh(EnrichDetailsTasks.dedupKey(e.detailGroup, key), FreshnessKind.DetailEnrich)
    }

  override def stop(): Unit = { scheduler.shutdown(); () }
}

object DetailReaper {
  /** How often the reaper wakes to enqueue the now-due slice of the corpus. At
   *  1min over a 6h period the deferred-cinema corpus spreads across ~360 ticks,
   *  so each tick enqueues only a sliver — a flat per-minute trickle rather than
   *  the ~5min-wide bursts a coarser cadence dumps in one tick (the `EnrichDetails`
   *  spikes on the `kinowo_worker_tasks` panel). The walk is a cheap in-memory
   *  corpus scan, so the finer cadence costs little. */
  val DefaultTickInterval: FiniteDuration = 1.minute

  /** Default per-tick enqueue cap — the bound on a COLD cohort's burst (every
   *  never-fresh row is due at once: `DueWindow.isDue` returns true with no
   *  last-fetch stamp, so the phase spread can't smear them). A cold cohort is
   *  exactly an onboarding wave (a cinema NEWLY opting into `DetailEnricher`, so
   *  none of its films have a `DetailEnrich` stamp yet — e.g. Kino Pionier's ~48
   *  `/event/<slug>` films) or a re-key / title-rule wave that orphans a batch's
   *  stamps at once.
   *
   *  Kept the LOWEST of the reaper caps (scrape is 25) because detail is the most
   *  AMPLIFYING: each `EnrichDetails` that lands cascades into a `ResolveTmdb` and
   *  then a fan of rating tasks, so a burst here multiplies ~5× downstream and is
   *  what pins the shared-CPU credit. At 10/tick (1min) a ~48-film onboarding
   *  trickles over ~5min instead of dumping in one tick (the `EnrichDetails` spike
   *  on the `kinowo_worker_tasks` panel). Day-to-day new films don't wait on this
   *  trickle — they're enqueued promptly by the event-driven [[DetailTaskEnqueuer]]
   *  (off `CinemaMovieAdded`), which the cap doesn't touch; only cold cohorts do.
   *  Flippable live via `KINOWO_DETAIL_MAX_ENQUEUE_PER_TICK`. */
  val DefaultMaxEnqueuePerTick: Int = 10
}
