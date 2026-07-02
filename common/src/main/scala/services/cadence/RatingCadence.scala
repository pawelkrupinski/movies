package services.cadence

import java.time.{Duration => JDuration, Instant}
import scala.concurrent.duration._

/**
 * Per-(rating source, film) state that drives the ADAPTIVE refresh interval.
 *
 * Each rating source is re-fetched on a schedule that stretches the longer a
 * film's DISPLAYED value stays put. Backing OFF is deliberately CONSERVATIVE — it
 * takes [[RatingCadence.StepUpAfter]] consecutive no-change refreshes to lengthen
 * the interval by one doubling; tightening is PROGRESSIVE — a single visible change
 * drops the interval one doubling (not all the way back to the base). So a film
 * cools down slowly and warms up quickly, doubling/halving between
 * [[RatingCadence.BaseInterval]] and [[RatingCadence.MaxInterval]]. The signal is
 * "did the value the user sees change since the previous scrape" (display precision,
 * not a sub-decimal vote drift — see `RatingDisplay`).
 *
 *  - `backoffLevel` — the backoff exponent; the interval is `base × 2^level`, capped.
 *    Climbs by one every [[RatingCadence.StepUpAfter]] no-change refreshes, drops by
 *    one on each change (floored at 0).
 *  - `unchangedStreak` — consecutive refreshes with no visible change. Reset to 0 on
 *    any change; counts toward the next step-up (a step-up fires when it reaches a
 *    multiple of [[RatingCadence.StepUpAfter]]).
 *  - `windowChecks` / `windowChanges` — how many refreshes ran, and how many moved
 *    the value, within the current ~7-day tumbling window ([[RatingCadence.Window]]).
 *    Kept as the "how volatile is this rating lately" history; not used by the
 *    interval directly (the level is), but surfaced for observability.
 *  - `windowStartedAt` / `lastCheckedAt` — window anchor and last-refresh stamp.
 *  - `lastChange` / `prevChange` — the two most recent times the displayed value
 *    moved, each carrying what it moved FROM and TO. Survive the window roll
 *    (they're the last two changes ever, for the cadence debug page), unlike the
 *    volatility counters.
 */
case class RatingChangeStats(
  unchangedStreak: Int,
  windowChecks:    Int,
  windowChanges:   Int,
  windowStartedAt: Instant,
  lastCheckedAt:   Instant,
  lastChange:      Option[RatingChange] = None,
  prevChange:      Option[RatingChange] = None,
  backoffLevel:    Int = 0
)

/** One observed change of a rating's DISPLAYED value: when it happened, the badge
 *  text it moved away FROM, and the one it became — both as shown ("7.1", "85",
 *  "93%"). `from` is empty for the very first value ever recorded (nothing to
 *  move away from), so the debug page renders that one with no arrow. */
case class RatingChange(at: Instant, from: String, to: String)

object RatingCadence {
  /** Fastest cadence — a fresh or just-changed film is rechecked this often. */
  val BaseInterval: FiniteDuration = 2.hours

  /** Slowest cadence — a film whose value hasn't visibly moved in many refreshes
   *  is rechecked no less often than this. */
  val MaxInterval: FiniteDuration = 4.days

  /** Rolling window over which we keep change/check counts ("last week of data"). */
  val Window: FiniteDuration = 7.days

  /** Consecutive no-change refreshes required to lengthen the cadence by one step
   *  (one doubling). Backing off is conservative: we want several quiet checks in a
   *  row before trusting the value enough to wait longer. Tightening is the opposite
   *  — a single visible change drops the cadence one step (see [[record]]). */
  val StepUpAfter: Int = 3

  /** Highest backoff exponent worth holding: the smallest level whose interval
   *  already reaches [[MaxInterval]]. Climbing past it would only re-clamp to the
   *  cap, but it would also make the progressive tighten take more changes to
   *  recover, so we stop the level here (base 2h, cap 4d ⇒ level 6). */
  private[cadence] val MaxLevel: Int =
    Iterator.from(0).find(l => BaseInterval.toMillis * (1L << l) >= MaxInterval.toMillis).getOrElse(20)

  /** Fold one refresh outcome into the stats. `reportedValue` is `Some(displayValue)`
   *  when this refresh (re)set the row's displayed badge text, or `None` for a
   *  no-change / failed refresh.
   *
   *  A reported value counts as a CHANGE only when it differs from the last
   *  recorded displayed value. The per-row refresh reports `Some(value)` whenever
   *  it writes the row's rating field — including when a re-keyed / re-resolved row
   *  goes `None → 7.3` and lands on the SAME value the user already saw (e.g. a
   *  title-rule fold re-keys the cache row while the cadence stays tmdbId-keyed).
   *  That isn't a visible change, so we dedup against `lastChange.to`; otherwise
   *  every re-resolution would log a phantom "7.3 → 7.3" change and pin the film to
   *  the base interval forever.
   *
   *  The backoff LEVEL drives the interval: it climbs one step only after
   *  [[StepUpAfter]] consecutive no-change refreshes (conservative lengthening) and
   *  drops one step on each visible change (progressive tightening, floored at 0).
   *  The `unchangedStreak` counts the consecutive no-change refreshes feeding that
   *  step-up and resets on a change; the window counters roll over once the window
   *  is older than [[Window]] so the volatility view tracks ~the last week; a real
   *  change shifts `lastChange` → `prevChange` and records the new one. */
  def record(prev: Option[RatingChangeStats], reportedValue: Option[String], now: Instant): RatingChangeStats = {
    val live       = prev.filter(s => withinWindow(s.windowStartedAt, now))
    val priorValue = prev.flatMap(_.lastChange).map(_.to)
    val changed    = reportedValue.exists(v => !priorValue.contains(v))
    val priorLevel = live.map(_.backoffLevel).getOrElse(0)
    val streak     = if (changed) 0 else live.map(_.unchangedStreak).getOrElse(0) + 1
    val level      =
      if (changed)                        math.max(0, priorLevel - 1)         // progressive tighten: one step per change
      else if (streak % StepUpAfter == 0) math.min(MaxLevel, priorLevel + 1)  // conservative lengthen: one step per StepUpAfter quiet checks
      else                                priorLevel
    val checks     = live.map(_.windowChecks).getOrElse(0) + 1
    val changes    = live.map(_.windowChanges).getOrElse(0) + (if (changed) 1 else 0)
    val anchor     = live.map(_.windowStartedAt).getOrElse(now)
    // Change history carries from `prev` (NOT `live`) so it survives the window
    // roll — it's the last two changes ever, not last-week.
    val (lastCh, prevCh) =
      if (changed) (reportedValue.map(v => RatingChange(now, priorValue.getOrElse(""), v)), prev.flatMap(_.lastChange))
      else         (prev.flatMap(_.lastChange), prev.flatMap(_.prevChange))
    RatingChangeStats(streak, checks, changes, anchor, now, lastCh, prevCh, level)
  }

  /** Interval before this film's next refresh of this source. Never-seen (None)
   *  starts at the base; otherwise base doubled per backoff level, capped. */
  def intervalFor(stats: Option[RatingChangeStats]): FiniteDuration =
    stats.map(s => intervalForLevel(s.backoffLevel)).getOrElse(BaseInterval)

  /** True when this film is on the fastest (base ~2h) cadence — backoff level 0,
   *  or never seen. The rating confirmation deadband only engages here: the
   *  A→B→A rounding-boundary flap concentrates at the base interval (each flip
   *  re-tightens the cadence), while a film that has backed off is stable and a
   *  genuine change on it shouldn't be held for the multi-day interval. */
  def atBaseInterval(stats: Option[RatingChangeStats]): Boolean =
    intervalFor(stats) <= BaseInterval

  /** base × 2^level, clamped to [base, max]. The shift is bounded before it can
   *  overflow a Long; the clamp to `MaxInterval` makes anything past [[MaxLevel]]
   *  a no-op. */
  private[cadence] def intervalForLevel(level: Int): FiniteDuration = {
    val shift  = math.min(math.max(level, 0), 20)
    val millis = math.min(BaseInterval.toMillis * (1L << shift), MaxInterval.toMillis)
    FiniteDuration(millis, MILLISECONDS)
  }

  private def withinWindow(windowStart: Instant, now: Instant): Boolean =
    JDuration.between(windowStart, now).toMillis <= Window.toMillis
}
