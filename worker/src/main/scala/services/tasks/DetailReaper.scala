package services.tasks

import services.movies.LatestTitleYear

import settings.{DetailMaxEnqueuePerTick, DetailTickInterval}

import services.freshness.{FreshnessKind, FreshnessStore}
import tools.{DaemonExecutors, ScheduledTick}
import models.{Cinema, MovieRecord, Source}
import play.api.Logging
import services.schedule.{AlwaysClaimScheduledRunStore, OccurrenceKey, ScheduledRunStore}
import services.movies.{CacheKey, MovieCache}
import services.Stoppable
import services.cinemas.common.DetailEnricher

import java.time.{Clock, Instant}
import java.util.concurrent.{ScheduledExecutorService, TimeUnit}
import scala.concurrent.duration._

/**
 * Periodically enqueues `EnrichDetails` tasks for every deferred cinema's
 * current films whose detail is missing or stale — the first fetch, its retry
 * after a failure, and the refresh a cinema whose showtime-bearing fetch is
 * itself deferred (Rialto) needs. This is the detail-side analogue of [[ScrapeReaper]] /
 * [[EnrichmentReaper]].
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
  // and each completion cascaded into follow-up tasks, spiking the shared-CPU
  // credit balance to zero.
  dueWindow: DueWindow = new DueWindow(6.hours),
  // How often the reaper wakes to enqueue the now-due slice — the spread
  // granularity (smaller = flatter trickle, at the cost of cheap in-memory scans).
  // BY-NAME + a self-rescheduling tick so an `/admin/config` interval flip applies
  // mid-flight on the next cycle, without a restart.
  tickInterval: => DetailTickInterval = DetailTickInterval(DetailReaper.DefaultTickInterval),
  // A small spacing before the first tick (0 in tests that drive `tick` directly).
  initialDelay: DetailReaper.InitialDelay = DetailReaper.InitialDelay(0.seconds),
  // Cap on enqueues per tick — the backstop the phase spread can't provide for a
  // COLD cohort (every re-keyed row is "never refreshed" → due at once). Bounds
  // that recovery burst the way every other reaper does; the leftover stays due
  // and drains over the next ticks. Default unbounded so tests driving `tick` are
  // unaffected; the wiring sets a finite cap. BY-NAME: read live each tick.
  maxEnqueuePerTick: => DetailMaxEnqueuePerTick = DetailMaxEnqueuePerTick(Int.MaxValue),
  runStore:  ScheduledRunStore = AlwaysClaimScheduledRunStore,
  clock:     Clock,
  // Which of a row's pages it asks for, under which key (`DetailPages`): one per venue and film for
  // the pipeline, every page a venue slot names for a cut-over country's identity model.
  pages:     DetailPages = DetailPages.PerVenue
) extends Stoppable with Logging {
  // The latest year a title may name, read off this class's clock at each ask (`LatestTitleYear`).
  private given LatestTitleYear = LatestTitleYear(clock)

  private val scheduler: ScheduledExecutorService = DaemonExecutors.scheduler("detail-reaper")

  /** Enrichers indexed by the venue they enrich, so a row is matched against only the
   *  venues that actually carry it instead of against every enricher there is.
   *
   *  `enrichers` is one instance PER VENUE (Cineworld alone is 87; 185 across the
   *  catalogue), while a row carries a handful of cinema slots — 0.34 on average
   *  across the live UK corpus. Asking every enricher about every row was therefore
   *  ~296k questions per tick to answer ~540 real ones, and since `nativeDetailRef`
   *  rebuilds `cinemaData` per call it paid a sort + Map build for each. That loop
   *  was 7.93cc of the UK worker's 12.46cc — 64% of its CPU — to produce 0.3
   *  EnrichDetails/min.
   *
   *  A Seq per cinema, not a single enricher: `toMap` would silently drop a second
   *  enricher on the same venue, quietly changing behaviour rather than preserving
   *  the original loop's "ask them all". */
  private val enrichersByCinema: Map[models.Cinema, Seq[DetailEnricher]] =
    enrichers.groupBy(_.cinema)

  def start(): Unit = {
    if (enrichers.isEmpty) { logger.info("DetailReaper: no deferred cinemas; not starting."); return }
    scheduleNext(initialDelay.value)
    logger.info(s"DetailReaper started over ${enrichers.size} deferred cinema(s): each detail refreshed once per " +
                s"${dueWindow.period.toHours}h, phase-spread over ticks every ${tickInterval.value.toSeconds}s.")
  }

  /** Self-rescheduling tick: run, then schedule the next reading `tickInterval`
   *  afresh, so an interval flip applies on the next cycle. */
  private def scheduleNext(delay: FiniteDuration): Unit = {
    scheduler.schedule(new Runnable {
      def run(): Unit = { ScheduledTick.logged("DetailReaper", logger)(tickIfClaimed()); scheduleNext(tickInterval.value) }
    }, delay.toMillis, TimeUnit.MILLISECONDS)
    ()
  }

  /** Run the detail tick only if this machine wins the
   *  current window's occurrence claim — otherwise another machine is handling
   *  this window, so skip. Returns the number of detail tasks enqueued (0 when
   *  the claim was lost). Package-private so tests can drive it directly. */
  private[tasks] def tickIfClaimed(): Int = {
    // Hold ALL ticks until the detail freshness stamps have hydrated from Mongo.
    // They load in the rest phase (after the scrape stamps), so a tick against the
    // not-yet-hydrated mirror reads every detail as never-fresh and re-enqueues the
    // whole deferred-detail corpus on EVERY deploy (the recurring post-deploy spike).
    // The scrape analogue is ScrapeReaper.awaitReadyThenStart; an in-memory /
    // Mongo-less store is ready at once.
    if (!freshness.isReady(FreshnessKind.DetailEnrich)) return 0
    val key = OccurrenceKey.at("detail", clock.millis(), tickInterval.value, 0.seconds)
    if (runStore.claim(key)) { val n = tick(); forgetGone(); n } else 0
  }

  /** Enqueue every now-due `(deferred-cinema, film)` detail, keyed off the row's
   *  CURRENT CacheKey (so it's robust to a row that was re-keyed since it was
   *  scraped), up to `maxEnqueuePerTick`. Public so tests / the
   *  fixture harness can drive one pass directly, with an injectable `nowMillis`
   *  so tests can advance time. Returns how many tasks were enqueued. */
  def tick(nowMillis: Long = clock.millis()): Int = {
    val now      = Instant.ofEpochMilli(nowMillis)
    val cap      = maxEnqueuePerTick.value
    var enqueued = 0
    val rows = cache.entries.iterator
    while (rows.hasNext && enqueued < cap) {
      val (key, record) = rows.next()
      // Drive off the row's OWN venues, not the whole enricher list — see
      // `enrichersByCinema` — remembered per row while the cache holds the same record: asking
      // every film each tick re-derived its venues (`cinemaData`, a sort and a Map rebuild) though
      // almost none had changed — 2.6% of the UK worker's CPU (JFR 2026-10-01).
      val asks = asksOf(key, record).iterator
      while (asks.hasNext && enqueued < cap) {
        val (e, ref, dk) = asks.next()
        if (EnrichDetailsTasks.enqueueIfDueAs(queue, freshness, dueWindow, e, key, ref, dk, now)) enqueued += 1
      }
    }
    if (enqueued > 0) logger.info(s"DetailReaper enqueued $enqueued due detail(s).")
    enqueued
  }

  // Each row's detail asks, with the record they were derived from; ticks run one at a time.
  private val asksByRow = new java.util.concurrent.ConcurrentHashMap[CacheKey, (MovieRecord, Seq[(DetailEnricher, String, String)])]()

  /** `pages.of` for `key`, reused while the cache still holds `record` itself — records are immutable,
   *  a changed row is a new one. A row the cache no longer holds is forgotten when next replaced or
   *  by [[forgetGone]]. */
  private def asksOf(key: CacheKey, record: MovieRecord): Seq[(DetailEnricher, String, String)] =
    Option(asksByRow.get(key)).collect { case (seen, asks) if seen eq record => asks }.getOrElse {
      val asks = pages.of(key, record, enrichersByCinema)
      asksByRow.put(key, record -> asks)
      asks
    }

  /** Drop the remembered asks of rows the cache no longer holds (merged, re-keyed, removed). */
  private def forgetGone(): Unit = {
    val live = cache.entries.iterator.map(_._1).toSet
    asksByRow.keySet.removeIf(key => !live.contains(key))
    ()
  }

  override def stop(): Unit = { scheduler.shutdown(); () }
}

object DetailReaper {

  /** How long after `start()` the first tick runs. */
  final case class InitialDelay(value: FiniteDuration) extends AnyVal

  /** How often the reaper wakes to enqueue the now-due slice of the corpus. At
   *  1min over a 6h period the deferred-cinema corpus spreads across ~360 ticks,
   *  so each tick enqueues only a sliver — a flat per-minute trickle rather than
   *  the ~5min-wide bursts a coarser cadence dumps in one tick (the `EnrichDetails`
   *  spikes on the `kinowo_worker_tasks` panel). The walk is a cheap in-memory
   *  corpus scan, so the finer cadence costs little. */
  val DefaultTickInterval: FiniteDuration = 1.minute
}

/** Which detail pages of a film row the [[DetailReaper]] asks for, each with the key it is due,
 *  deduplicated and stamped under. */
trait DetailPages {
  def of(key: CacheKey, record: MovieRecord, enrichersByCinema: Map[Cinema, Seq[DetailEnricher]])(using LatestTitleYear): Seq[(DetailEnricher, String, String)]
}

object DetailPages {

  /** The pipeline's: one page per venue and film — the page its venue slots, merged, name — keyed by
   *  the film row. `cinemaData` is computed once per row: it sorts and rebuilds a Map per call. */
  object PerVenue extends DetailPages {
    def of(key: CacheKey, record: MovieRecord, enrichersByCinema: Map[Cinema, Seq[DetailEnricher]])(using LatestTitleYear): Seq[(DetailEnricher, String, String)] = {
      val cinemaData = record.cinemaData
      cinemaData.keys.toSeq.flatMap(c => enrichersByCinema.getOrElse(c, Nil)).flatMap(e =>
        e.nativeDetailRefIn(cinemaData).map(ref => (e, ref, EnrichDetailsTasks.dedupKey(e.detailGroup, key))))
    }
  }

  /** A cut-over country's: every page any venue slot of the row names, keyed by the PAGE — so which
   *  pages the identity model gets does not depend on how its films were gathered over time. */
  object PerPage extends DetailPages {
    def of(key: CacheKey, record: MovieRecord, enrichersByCinema: Map[Cinema, Seq[DetailEnricher]])(using LatestTitleYear): Seq[(DetailEnricher, String, String)] =
      record.data.toSeq.flatMap { case (source, slot) =>
        for {
          cinema <- Source.cinemaOf(source).toSeq
          e      <- enrichersByCinema.getOrElse(cinema, Nil)
          ref    <- DetailEnricher.nativeRefOf(slot).toSeq
        } yield (e, ref, EnrichDetailsTasks.pageDedupKey(e.detailGroup, ref))
      }.distinctBy(_._3)
  }
}

