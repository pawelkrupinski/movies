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
 *  - `lastChange` / `prevChange` — the two most recent times the displayed value
 *    moved, with what it moved TO. Survive the window roll (they're the last two
 *    changes ever, for the cadence debug page), unlike the volatility counters.
 */
case class RatingChangeStats(
  unchangedStreak: Int,
  windowChecks:    Int,
  windowChanges:   Int,
  windowStartedAt: Instant,
  lastCheckedAt:   Instant,
  lastChange:      Option[RatingChange] = None,
  prevChange:      Option[RatingChange] = None
)

/** One observed change of a rating's DISPLAYED value: when it happened and the
 *  value it became (the badge text — "7.1", "85", "93%"). */
case class RatingChange(at: Instant, value: String)

object RatingCadence {
  /** Fastest cadence — a fresh or just-changed film is rechecked this often. */
  val BaseInterval: FiniteDuration = 2.hours

  /** Slowest cadence — a film whose value hasn't visibly moved in many refreshes
   *  is rechecked no less often than this. */
  val MaxInterval: FiniteDuration = 4.days

  /** Rolling window over which we keep change/check counts ("last week of data"). */
  val Window: FiniteDuration = 7.days

  /** Fold one refresh outcome into the stats. `reportedValue` is `Some(displayValue)`
   *  when this refresh (re)set the row's displayed badge text, or `None` for a
   *  no-change / failed refresh.
   *
   *  A reported value counts as a CHANGE only when it differs from the last
   *  recorded displayed value. The per-row refresh reports `Some(value)` whenever
   *  it writes the row's rating field — including when a re-keyed / re-resolved row
   *  goes `None → 7.3` and lands on the SAME value the user already saw (e.g. a
   *  title-rule fold re-keys the cache row while the cadence stays tmdbId-keyed).
   *  That isn't a visible change, so we dedup against `lastChange.value`; otherwise
   *  every re-resolution would log a phantom "7.3 → 7.3" change and pin the film to
   *  the base interval forever.
   *
   *  The streak grows on no-change (→ longer waits) and resets on a real change;
   *  the window counters roll over once the window is older than [[Window]] so the
   *  volatility view tracks ~the last week; a real change shifts `lastChange` →
   *  `prevChange` and records the new one. */
  def record(prev: Option[RatingChangeStats], reportedValue: Option[String], now: Instant): RatingChangeStats = {
    val live       = prev.filter(s => withinWindow(s.windowStartedAt, now))
    val priorValue = prev.flatMap(_.lastChange).map(_.value)
    val changed    = reportedValue.exists(v => !priorValue.contains(v))
    val streak     = if (changed) 0 else live.map(_.unchangedStreak).getOrElse(0) + 1
    val checks     = live.map(_.windowChecks).getOrElse(0) + 1
    val changes    = live.map(_.windowChanges).getOrElse(0) + (if (changed) 1 else 0)
    val anchor     = live.map(_.windowStartedAt).getOrElse(now)
    // Change history carries from `prev` (NOT `live`) so it survives the window
    // roll — it's the last two changes ever, not last-week.
    val (lastCh, prevCh) =
      if (changed) (reportedValue.map(RatingChange(now, _)), prev.flatMap(_.lastChange))
      else         (prev.flatMap(_.lastChange), prev.flatMap(_.prevChange))
    RatingChangeStats(streak, checks, changes, anchor, now, lastCh, prevCh)
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
