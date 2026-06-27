package services.tasks

import java.time.Instant
import scala.concurrent.duration.FiniteDuration
import scala.util.hashing.MurmurHash3

/**
 * A single, deterministic per-key refresh schedule, shared by a periodic *reaper*
 * (which enqueues due keys) and its *handler* (which re-checks at pickup). Both
 * MUST decide "due" with this one function. Used for rating refresh
 * ([[EnrichmentReaper]] + [[RatingHandler]], period = the 4h rating TTL) and for
 * cinema scraping ([[ScrapeReaper]] + [[ScrapeCinemaHandler]], period = the scrape
 * freshness window).
 *
 * The reaper and handler MUST share one instance, or they disagree: when the
 * reaper enqueued on a phase-window boundary while the handler re-gated on a
 * rolling `period` TTL (`FreshnessStore.isFresh`), a key refreshed just before its
 * boundary was due again just after it yet still within the rolling TTL — so the
 * handler skipped a task the reaper kept enqueueing every tick, churning the queue
 * (insert + skip + delete) without ever refreshing it. With one shared definition
 * the handler skips a task iff the reaper would no longer enqueue it, so a due task
 * is always acted on.
 *
 * Each key refreshes once per its `period`, at a deterministic phase offset in
 * `[0, period)` hashed from its dedup key — stable across restarts and needing no
 * storage — so a synchronized corpus is spread evenly across the period instead of
 * bursting at the boundary. Keys embed their source/cinema, so they land on
 * independent phases.
 *
 * The period is resolved PER KEY via `periodFor`, so a schedule can adapt to a
 * key's own history (the rating reaper feeds the per-film adaptive interval from
 * [[services.cadence.RatingCadence]]). A constant schedule (scraping) uses the
 * fixed-period auxiliary constructor.
 */
class DueWindow(periodFor: String => FiniteDuration, val period: FiniteDuration) {

  /** Fixed schedule: every key shares `period` (scrape cadence, tests). */
  def this(period: FiniteDuration) = this(_ => period, period)

  /** Per-key phase offset in `[0, period)`, deterministic from the dedup key. */
  private def phaseMillis(dedupKey: String, periodMillis: Long): Long =
    Math.floorMod(MurmurHash3.stringHash(dedupKey).toLong, periodMillis)

  /** Which period-length window (counted from this key's own phase) `atMillis`
   *  falls in. A new window means the key is due for another refresh. */
  private def windowIndex(atMillis: Long, dedupKey: String, periodMillis: Long): Long =
    Math.floorDiv(atMillis - phaseMillis(dedupKey, periodMillis), periodMillis)

  /** Due iff never refreshed, or `now` has entered a new period-window since the
   *  last refresh (the key's personal phase boundary has passed). The key's
   *  current period is resolved once here, so both window indices compare on the
   *  same boundary even as the adaptive period shifts between calls. */
  def isDue(dedupKey: String, lastFetchedAt: Option[Instant], now: Instant): Boolean =
    lastFetchedAt match {
      case None    => true
      case Some(t) =>
        val periodMillis = periodFor(dedupKey).toMillis
        windowIndex(now.toEpochMilli, dedupKey, periodMillis) > windowIndex(t.toEpochMilli, dedupKey, periodMillis)
    }
}
