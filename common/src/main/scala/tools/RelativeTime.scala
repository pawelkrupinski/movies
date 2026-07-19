package tools

import java.time.{Duration, Instant}

/**
 * Human phrasing of the gap between two instants — "14m ago", "in 3h", "now".
 *
 * A plain `Duration.between(when, now)` is NEGATIVE for a future instant, so a
 * naive "${d.toMinutes}m ago" renders a next-due timestamp as "-225m ago": wrong
 * sign, and phrased as the past when it's the future. The direction is the whole
 * point of this helper, which is why it lives here as a tested function rather
 * than inline in a template where neither case could be asserted.
 */
object RelativeTime {

  /** Under this, both directions read as "now" — below the resolution anyone
   *  cares about on a debug page, and it avoids "in 0m". */
  private val NowThreshold = Duration.ofSeconds(30)

  /** `when` relative to `reference`. Past → "3h ago"; future → "in 3h". */
  def of(when: Instant, reference: Instant = Instant.now()): String = {
    val gap = Duration.between(reference, when)
    if (gap.abs.compareTo(NowThreshold) < 0) "now"
    else if (gap.isNegative) s"${magnitude(gap.abs)} ago"
    else s"in ${magnitude(gap)}"
  }

  /** Coarsest unit that keeps the number small: minutes < 1h, hours < 48h, else days. */
  private def magnitude(gap: Duration): String =
    if (gap.toMinutes < 60) s"${gap.toMinutes}m"
    else if (gap.toHours < 48) s"${gap.toHours}h"
    else s"${gap.toDays}d"
}
