package services.alerts

import java.time.{Clock, Instant}
import scala.concurrent.duration.FiniteDuration

/** How many pages a [[BurstLimitedNotifier]] lets through per `window`. */
final case class AlertBurst(max: Int, window: FiniteDuration)

/**
 * Caps a per-venue page stream at `burst.max` messages per `burst.window`.
 *
 * Per-venue pages are right one venue at a time and useless in bulk: when a
 * whole aggregator fails, every venue on it hands over to its fallback at once
 * (~1,135 German venues on Filmstarts), and each would page ENTER and later
 * RECOVERED. The first page past the limit says pages are being held, once; the
 * rest wait silently until the window turns, when the count of what was held
 * goes out ahead of the next page. The aggregate is what `/uptime` and the
 * fallback-saturation alert are for.
 */
final class BurstLimitedNotifier(deliver: String => Unit, burst: AlertBurst, clock: Clock, kind: String) {
  private var windowStart: Instant = Instant.MIN
  private var sent: Int            = 0
  private var held: Int            = 0

  def send(message: String): Unit = {
    val toDeliver = synchronized {
      val now = clock.instant()
      val carried =
        if (windowStart == Instant.MIN || !now.isBefore(windowStart.plusMillis(burst.window.toMillis))) {
          val report = Option.when(held > 0)(s"ℹ️ $held $kind page(s) were held back in the last window of ${burst.window}.")
          windowStart = now; sent = 0; held = 0
          report.toList
        } else Nil
      if (sent < burst.max) { sent += 1; carried :+ message }
      else {
        held += 1
        if (held == 1) carried :+ s"⚠️ More than ${burst.max} $kind pages within ${burst.window} — holding the " +
          "rest. Many venues failing at once usually means their aggregator is down: see /uptime."
        else carried
      }
    }
    toDeliver.foreach(deliver)
  }
}

/** One [[BurstLimitedNotifier]] per KIND of page, so an outage's flood of one kind
 *  (ENTER) cannot spend the budget of the pages that close it out (RECOVERED) or of
 *  a rare one-off (the gone-venue page). */
final class BurstLimitedPager(deliver: String => Unit, burst: AlertBurst, clock: Clock) {
  private val limiters = new java.util.concurrent.ConcurrentHashMap[String, BurstLimitedNotifier]()

  def pagerFor(kind: String): String => Unit =
    limiters.computeIfAbsent(kind, k => new BurstLimitedNotifier(deliver, burst, clock, k)).send
}
