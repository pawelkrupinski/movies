package services.tasks

import java.time.Instant
import scala.concurrent.duration._

/** The fixture harness's hand on the reapers' `private[tasks]` ticks.
 *
 *  A harness has no scheduler, so "what production does over time" has to be driven
 *  explicitly — and for the TMDB re-try sweep that means a whole PERIOD of ticks, not one:
 *  each row is due at its own hashed phase within the 24h window, so a single tick re-tries
 *  only the ~1/288 slice whose boundary fell in it. Sweeping every tick of one period
 *  offers each eligible row exactly once, which is what a fixpoint pass needs — the
 *  question is whether re-trying a row that has nothing new to find writes anything. */
object ReaperSweeps {

  /** Every tick of one due period of `reaper`, starting after `from`. Returns how many
   *  rows it re-tried. */
  def unresolvedTmdbPeriod(reaper: UnresolvedTmdbReaper, from: Instant,
                           period: FiniteDuration = 24.hours,
                           interval: FiniteDuration = UnresolvedTmdbReaper.DefaultTickInterval): Int =
    (1L to period.toMillis / interval.toMillis).iterator
      .map(k => reaper.tick(from.toEpochMilli + k * interval.toMillis)).sum
}

