package services.cadence

import java.time.{Duration => JDuration, Instant}
import scala.concurrent.duration._

/**
 * Per-(rating source, film) state that drives the ADAPTIVE refresh interval.
 *
 * Each rating source is re-fetched on a schedule that stretches the longer a
 * film's DISPLAYED value stays put: a fresh or actively-changing film is checked
 * every [[RatingCadence.BaseInterval]]; one whose shown value hasn't moved across
 * several refreshes backs off, doubling each time, up to [[RatingCadence.MaxInterval]].
 * The signal is "did the value the user sees change since the previous scrape"
 * (display precision, not a sub-decimal vote drift — see `RatingDisplay`).
 *
 *  - `unchangedStreak` — consecutive refreshes with no visible change; the backoff
 *    exponent. Reset to 0 on any change.
 *  - `windowChecks` / `windowChanges` — how many refreshes ran, and how many moved
 *    the value, within the current ~7-day tumbling window ([[RatingCadence.Window]]).
 *    Kept as the "how volatile is this rating lately" history; not used by the
 *    interval directly (the streak is), but surfaced for observability.
 *  - `windowStartedAt` / `lastCheckedAt` — window anchor and last-refresh stamp.
 */
case class RatingChangeStats(
  unchangedStreak: Int,
  windowChecks:    Int,
  windowChanges:   Int,
  windowStartedAt: Instant,
  lastCheckedAt:   Instant
)

object RatingCadence {
  /** Fastest cadence — a fresh or just-changed film is rechecked this often. */
  val BaseInterval: FiniteDuration = 2.hours

  /** Slowest cadence — a film whose value hasn't visibly moved in many refreshes
   *  is rechecked no less often than this. */
  val MaxInterval: FiniteDuration = 4.days

  /** Rolling window over which we keep change/check counts ("last week of data"). */
  val Window: FiniteDuration = 7.days

  /** Fold one refresh outcome into the stats. `changed` is whether the displayed
   *  value differs from the previous scrape. The streak grows on no-change (→
   *  longer waits) and resets on change; the window counters roll over once the
   *  window is older than [[Window]] so the volatility view tracks ~the last week. */
  def record(prev: Option[RatingChangeStats], changed: Boolean, now: Instant): RatingChangeStats = {
    val live    = prev.filter(s => withinWindow(s.windowStartedAt, now))
    val streak  = if (changed) 0 else live.map(_.unchangedStreak).getOrElse(0) + 1
    val checks  = live.map(_.windowChecks).getOrElse(0) + 1
    val changes = live.map(_.windowChanges).getOrElse(0) + (if (changed) 1 else 0)
    val anchor  = live.map(_.windowStartedAt).getOrElse(now)
    RatingChangeStats(streak, checks, changes, anchor, now)
  }

  /** Interval before this film's next refresh of this source. Never-seen (None)
   *  starts at the base; otherwise base doubled per consecutive no-change, capped. */
  def intervalFor(stats: Option[RatingChangeStats]): FiniteDuration =
    stats.map(s => intervalForStreak(s.unchangedStreak)).getOrElse(BaseInterval)

  /** base × 2^streak, clamped to [base, max]. The shift is bounded before it can
   *  overflow a Long; the clamp to `MaxInterval` makes anything past ~6 a no-op. */
  private[cadence] def intervalForStreak(streak: Int): FiniteDuration = {
    val shift  = math.min(math.max(streak, 0), 20)
    val millis = math.min(BaseInterval.toMillis * (1L << shift), MaxInterval.toMillis)
    FiniteDuration(millis, MILLISECONDS)
  }

  private def withinWindow(windowStart: Instant, now: Instant): Boolean =
    JDuration.between(windowStart, now).toMillis <= Window.toMillis
}
