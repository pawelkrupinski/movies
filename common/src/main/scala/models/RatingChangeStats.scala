package models

import java.time.Instant

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
