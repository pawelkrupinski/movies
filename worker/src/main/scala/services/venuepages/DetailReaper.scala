package services.venuepages

import java.time.{Clock, Instant}

import scala.concurrent.duration.*

import models.CacheKey
import models.{Cinema, MovieRecord, Source}
import play.api.Logging
import services.cinemas.common.DetailEnricher
import services.evidence.LatestTitleYear
import services.movies.MovieCacheReader
import services.tasks.Enqueued
import settings.DetailMaxEnqueuePerTick

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
 *
 * Each row's detail refreshes once per the [[DetailSchedule]]'s period (6h, the DetailEnrich TTL), phase-spread across
 * it exactly like [[EnrichmentReaper]] / [[ScrapeReaper]]: the phase offset, hashed from each row's dedup key, is what
 * stops a synchronized cohort — a re-key / title-rule wave that orphans a whole batch's freshness stamps at once — from
 * all coming due in the SAME tick. Before it, DetailReaper gated on a raw rolling TTL with no phase offset, so such a
 * cohort dumped its whole backlog in one tick (~1k `EnrichDetails` observed in prod), and each completion cascaded into
 * follow-up tasks, spiking the shared-CPU credit balance to zero.
 *
 * Its schedule — a tick every `DetailTickInterval` (the spread granularity), one machine a window, held until the
 * detail stamps hydrated ([[ready]]) — is the [[services.tasks.ClaimedEnqueueReaper]] the composition root runs [[run]]
 * on ([[DetailReaper.Occurrence]], recorded as [[DetailReaper.Tick]]).
 */
class DetailReaper(
  cache:     MovieCacheReader,
  // The due schedule, SAME instance as the handler's (see [[DetailSchedule]]).
  schedule:  DetailSchedule,
  // Which of a row's pages it asks for, under which key (`DetailPages`): one per venue and film for
  // the pipeline, every page a venue slot names for a cut-over country's identity model.
  pages:     DetailPages,
  // Cap on enqueues per tick — the backstop the phase spread can't provide for a
  // COLD cohort (every re-keyed row is "never refreshed" → due at once). Bounds
  // that recovery burst the way every other reaper does; the leftover stays due
  // and drains over the next ticks. Default unbounded so tests driving `tick` are
  // unaffected; the wiring sets a finite cap. BY-NAME: read live each tick.
  maxEnqueuePerTick: => DetailMaxEnqueuePerTick = DetailMaxEnqueuePerTick(Int.MaxValue),
  clock:     Clock
) extends Logging {
  // The latest year a title may name, read off this class's clock at each ask (`LatestTitleYear`).
  private given LatestTitleYear = LatestTitleYear(clock)

  /** Whether a tick may run: the detail stamps have hydrated ([[DetailSchedule.ready]]). The scrape analogue is
   *  ScrapeReaper.awaitReadyThenStart. */
  def ready: Boolean = schedule.ready

  /** One scheduled run: the tick, then the remembered asks of rows the cache no longer holds forgotten. What the
   *  tick's enqueues came to. */
  def run(): Enqueued = { val n = tick(); forgetGone(); n }

  /** Enqueue every now-due `(deferred-cinema, film)` detail, keyed off the row's
   *  CURRENT CacheKey (so it's robust to a row that was re-keyed since it was
   *  scraped), up to `maxEnqueuePerTick`. Public so tests / the
   *  fixture harness can drive one pass directly, with an injectable `nowMillis`
   *  so tests can advance time. Returns how many tasks were enqueued, and how many
   *  enqueues failed (a run with any is not a completed one). */
  def tick(nowMillis: Long = clock.millis()): Enqueued = {
    val now      = Instant.ofEpochMilli(nowMillis)
    val cap      = maxEnqueuePerTick.value
    var enqueued = Enqueued.Zero
    val rows = cache.entries.iterator
    while (rows.hasNext && enqueued.added < cap) {
      val (key, record) = rows.next()
      // Drive off the row's OWN venues, not the whole enricher list — see
      // `DetailPages` — remembered per row while the cache holds the same record: asking
      // every film each tick re-derived its venues (`cinemaData`, a sort and a Map rebuild) though
      // almost none had changed — 2.6% of the UK worker's CPU (JFR 2026-10-01).
      val asks = asksOf(key, record).iterator
      while (asks.hasNext && enqueued.added < cap) {
        val ask = asks.next()
        enqueued += schedule.enqueueIfDue(ask.dedupKey, ask.payload(key), now)
      }
    }
    if (enqueued.added > 0) logger.info(s"DetailReaper enqueued ${enqueued.added} due detail(s).")
    if (enqueued.failed > 0) logger.warn(s"DetailReaper: ${enqueued.failed} detail enqueue(s) failed this tick.")
    enqueued
  }

  // Each row's detail asks, with the record they were derived from; ticks run one at a time.
  private val asksByRow = new java.util.concurrent.ConcurrentHashMap[CacheKey, (MovieRecord, Seq[DetailPages.Ask])]()

  /** `pages.of` for `key`, reused while the cache still holds `record` itself — records are immutable,
   *  a changed row is a new one. A row the cache no longer holds is forgotten when next replaced or
   *  by [[forgetGone]]. */
  private def asksOf(key: CacheKey, record: MovieRecord): Seq[DetailPages.Ask] =
    Option(asksByRow.get(key)).collect { case (seen, asks) if seen eq record => asks }.getOrElse {
      val asks = pages.of(key, record)
      asksByRow.put(key, record -> asks)
      asks
    }

  /** Drop the remembered asks of rows the cache no longer holds (merged, re-keyed, removed). */
  private def forgetGone(): Unit = {
    val live = cache.entries.iterator.map(_._1).toSet
    asksByRow.keySet.removeIf(key => !live.contains(key))
    ()
  }
}

object DetailReaper {

  /** The tick window's occurrence claim, one machine a window. */
  val Occurrence = "detail"

  /** What each completed run is recorded as (`ScheduledTickStale`) and its scheduler named. */
  val Tick = "detail-reaper"

  /** How often the reaper wakes to enqueue the now-due slice of the corpus. At
   *  1min over a 6h period the deferred-cinema corpus spreads across ~360 ticks,
   *  so each tick enqueues only a sliver — a flat per-minute trickle rather than
   *  the ~5min-wide bursts a coarser cadence dumps in one tick (the `EnrichDetails`
   *  spikes on the `kinowo_worker_tasks` panel). The walk is a cheap in-memory
   *  corpus scan, so the finer cadence costs little. */
  val DefaultTickInterval: FiniteDuration = 1.minute
}

/** Which detail pages of a film row the [[DetailReaper]] asks for, each with the key it is due,
 *  deduplicated and stamped under. Over the venues' enrichers, indexed by the venue they enrich, so a
 *  row is matched against only the venues that actually carry it instead of against every enricher there is.
 *
 *  The enrichers are one instance PER VENUE (Cineworld alone is 87; 185 across the
 *  catalogue), while a row carries a handful of cinema slots — 0.34 on average
 *  across the live UK corpus. Asking every enricher about every row was therefore
 *  ~296k questions per tick to answer ~540 real ones, and since asking each with a fresh
 *  `cinemaData` rebuilt it per call it paid a sort + Map build for each. That loop
 *  was 7.93cc of the UK worker's 12.46cc — 64% of its CPU — to produce 0.3
 *  EnrichDetails/min.
 *
 *  A Seq per cinema, not a single enricher: `toMap` would silently drop a second
 *  enricher on the same venue, quietly changing behaviour rather than preserving
 *  the original loop's "ask them all". */
trait DetailPages {
  def of(key: CacheKey, record: MovieRecord)(using LatestTitleYear): Seq[DetailPages.Ask]
}

object DetailPages {

  private def byCinema(enrichers: Seq[DetailEnricher]): Map[Cinema, Seq[DetailEnricher]] = enrichers.groupBy(_.cinema)

  /** One page to ask: `ref` of `enricher`'s venue, due, deduplicated and stamped under `dedupKey`. */
  final case class Ask(enricher: DetailEnricher, ref: String, dedupKey: String) {
    /** The `EnrichDetails` task's payload asking this page for the row at `key`. */
    def payload(key: CacheKey): Map[String, String] = EnrichDetailsTasks.payload(enricher, key, ref)
  }

  /** The pipeline's: one page per venue and film — the page its venue slots, merged, name — keyed by
   *  the film row. `cinemaData` is computed once per row: it sorts and rebuilds a Map per call. */
  final class PerVenue(enrichers: Seq[DetailEnricher]) extends DetailPages {
    private val enrichersByCinema = byCinema(enrichers)

    def of(key: CacheKey, record: MovieRecord)(using LatestTitleYear): Seq[Ask] = {
      val cinemaData = record.cinemaData
      cinemaData.keys.toSeq.flatMap(c => enrichersByCinema.getOrElse(c, Nil)).flatMap(e =>
        e.nativeDetailRefIn(cinemaData).map(ref => Ask(e, ref, EnrichDetailsTasks.dedupKey(e.detailGroup, key))))
    }
  }

  /** A cut-over country's: every page any venue slot of the row names, keyed by the PAGE — so which
   *  pages the identity model gets does not depend on how its films were gathered over time. */
  final class PerPage(enrichers: Seq[DetailEnricher]) extends DetailPages {
    private val enrichersByCinema = byCinema(enrichers)

    def of(key: CacheKey, record: MovieRecord)(using LatestTitleYear): Seq[Ask] =
      record.data.toSeq.flatMap { case (source, slot) =>
        for {
          cinema <- Source.cinemaOf(source).toSeq
          e      <- enrichersByCinema.getOrElse(cinema, Nil)
          ref    <- DetailEnricher.nativeRefOf(slot).toSeq
        } yield Ask(e, ref, EnrichDetailsTasks.pageDedupKey(e.detailGroup, ref))
      }.distinctBy(_.dedupKey)
  }
}
