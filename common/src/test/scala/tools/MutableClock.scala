package tools

import java.time.{Clock, Duration, Instant, ZoneId, ZoneOffset}

/** A [[java.time.Clock]] a spec moves by hand, so anything that measures an AGE or an
 *  expiry can be asserted to the second instead of raced against wall time. */
final class MutableClock(start: Instant) extends Clock {
  @volatile private var now: Instant = start
  def advance(d: Duration): Unit          = now = now.plus(d)
  def advanceSeconds(seconds: Long): Unit = advance(Duration.ofSeconds(seconds))
  override def instant(): Instant         = now
  override def getZone: ZoneId            = ZoneOffset.UTC
  override def withZone(zone: ZoneId): Clock = this
}
