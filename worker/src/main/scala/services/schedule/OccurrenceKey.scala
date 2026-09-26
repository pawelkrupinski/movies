package services.schedule

import scala.concurrent.duration.FiniteDuration

/**
 * Canonical identifier for a single *occurrence* of a recurring job — the unit
 * an occurrence-claim is taken on (see [[ScheduledRunStore]]).
 *
 * A recurring job fires every `period`, `offset` past the epoch-aligned
 * boundary (e.g. "every 4h, 1h past the boundary"). `at` maps any instant to
 * the boundary of the window it falls in, so two worker machines whose timers
 * fire at slightly different moments within the same window compute the *same*
 * id — which is what lets a claim on that id be won exactly once cluster-wide
 * without aligning the timers themselves.
 */
object OccurrenceKey {

  /** The occurrence id active at `nowMillis` for a job firing every `period`,
   *  `offset` past the epoch-aligned boundary. Stable across machines for any
   *  instant in the same window. */
  def at(job: String, nowMillis: Long, period: FiniteDuration, offset: FiniteDuration): String =
    s"$job@${java.time.Instant.ofEpochMilli(windowStart(nowMillis, period, offset))}"

  /** The start, in epoch millis, of the window `nowMillis` falls in. */
  def windowStart(nowMillis: Long, period: FiniteDuration, offset: FiniteDuration): Long =
    Math.floorDiv(nowMillis - offset.toMillis, period.toMillis) * period.toMillis + offset.toMillis
}
