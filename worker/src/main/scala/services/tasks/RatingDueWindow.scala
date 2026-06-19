package services.tasks

import java.time.Instant
import scala.concurrent.duration.FiniteDuration
import scala.util.hashing.MurmurHash3

/**
 * The single, deterministic per-`(row, source)` rating-refresh schedule, shared
 * by the [[EnrichmentReaper]] (which enqueues due rows) and [[RatingHandler]]
 * (which re-checks at pickup). Both MUST decide "due" with this one function.
 *
 * They used to disagree: the reaper enqueued on a phase-window boundary while the
 * handler re-gated on a rolling `period` TTL (`FreshnessStore.isFresh`). A row
 * refreshed just before its boundary is due again just after it, yet is still
 * within the rolling TTL — so the handler skipped a task the reaper kept
 * enqueueing every tick, churning the queue (insert + skip + delete) without ever
 * refreshing it. With one shared definition the handler skips a task iff the
 * reaper would no longer enqueue it, so a due task is always acted on.
 *
 * Each `(row, source)` refreshes once per `period`, at a deterministic phase
 * offset in `[0, period)` hashed from its dedup key — stable across restarts and
 * needing no storage — so a synchronized corpus is spread across the period
 * instead of bursting. The dedup key embeds the source label, so a film's four
 * sources land on independent phases.
 */
class RatingDueWindow(val period: FiniteDuration) {
  private val periodMillis: Long = period.toMillis

  /** Per-key phase offset in `[0, period)`, deterministic from the dedup key. */
  private def phaseMillis(dedupKey: String): Long =
    Math.floorMod(MurmurHash3.stringHash(dedupKey).toLong, periodMillis)

  /** Which period-length window (counted from this key's own phase) `atMillis`
   *  falls in. A new window means the key is due for another refresh. */
  private def windowIndex(atMillis: Long, dedupKey: String): Long =
    Math.floorDiv(atMillis - phaseMillis(dedupKey), periodMillis)

  /** Due iff never refreshed, or `now` has entered a new period-window since the
   *  last refresh (the key's personal phase boundary has passed). */
  def isDue(dedupKey: String, lastFetchedAt: Option[Instant], now: Instant): Boolean =
    lastFetchedAt match {
      case None    => true
      case Some(t) => windowIndex(now.toEpochMilli, dedupKey) > windowIndex(t.toEpochMilli, dedupKey)
    }
}
