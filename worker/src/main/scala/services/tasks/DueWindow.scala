package services.tasks

import java.time.Instant
import services.cadence.DueBoundary

import scala.concurrent.duration.FiniteDuration

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
 * Each key refreshes once per its `period`, at a phase offset in `[0, period)` chosen
 * by its [[PhaseOffset]] — by default hashed from its dedup key, stable across
 * restarts and needing no storage — so a synchronized corpus is spread across the
 * period instead of bursting at the boundary. The scrape schedule spaces the phases
 * by each cinema's measured cost instead ([[CostSpacedPhaseOffset]]), and since those
 * phases move as costs are re-measured it counts a refresh toward its NEAREST
 * boundary — see [[services.cadence.DueBoundary]] for both countings.
 *
 * The period is resolved PER KEY via `periodFor`, so a schedule can adapt to a
 * key's own history (the rating reaper feeds the per-film adaptive interval from
 * [[services.cadence.RatingCadence]]). A constant schedule uses the fixed-period
 * auxiliary constructor.
 */
class DueWindow(
  periodFor:  String => FiniteDuration,
  val period: FiniteDuration,
  phase:      PhaseOffset = HashedPhaseOffset,
  counting:   DueBoundary.Counting = DueBoundary.PrecedingBoundary
) {

  /** Fixed schedule: every key shares `period` (tests, fixed-period reapers). */
  def this(period: FiniteDuration) = this(_ => period, period)

  /** Due iff never refreshed, or a boundary after the one its last refresh counts
   *  toward has passed. The key's current period is resolved once here, so both
   *  sides compare on the same boundaries even as the adaptive period shifts. */
  def isDue(dedupKey: String, lastFetchedAt: Option[Instant], now: Instant): Boolean =
    lastFetchedAt match {
      case None    => true
      case Some(t) =>
        val periodMillis = periodFor(dedupKey).toMillis
        DueBoundary.isDue(t.toEpochMilli, now.toEpochMilli, phase.millis(dedupKey, periodMillis), periodMillis, counting)
    }
}

/** Where in its period a key's refresh boundary sits. */
trait PhaseOffset {
  /** The key's offset in `[0, periodMillis)`. */
  def millis(dedupKey: String, periodMillis: Long): Long
}

/** The default: an offset hashed from the key, so keys spread evenly by COUNT. */
object HashedPhaseOffset extends PhaseOffset {
  def millis(dedupKey: String, periodMillis: Long): Long = DueBoundary.hashedPhaseMillis(dedupKey, periodMillis)
}
