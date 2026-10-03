package tools

import com.github.benmanes.caffeine.cache.Ticker

import java.time.{Clock, Duration, Instant, ZoneId, ZoneOffset}

/** A [[java.time.Clock]] a spec moves by hand, so anything that measures an AGE or an
 *  expiry can be asserted to the second instead of raced against wall time — the ONE
 *  hand-moved clock every module's specs share (`NoSleepInTestsSpec` refuses another). */
final class MutableClock(start: Instant) extends Clock {
  @volatile private var now: Instant = start
  def advance(d: Duration): Unit          = now = now.plus(d)
  def advanceSeconds(seconds: Long): Unit = advance(Duration.ofSeconds(seconds))
  def advanceMillis(millis: Long): Unit   = advance(Duration.ofMillis(millis))
  /** Jump to `at` — backwards too, for a spec proving a rewound clock changes nothing. */
  def setTo(at: Instant): Unit            = now = at
  override def instant(): Instant         = now
  override def getZone: ZoneId            = ZoneOffset.UTC
  override def withZone(zone: ZoneId): Clock = this

  /** The same moved-by-hand time as a Caffeine [[Ticker]], for a cache that expires entries
   *  by `expireAfterWrite`: advancing this clock ages them. Nanoseconds since `start`. */
  val ticker: Ticker = () => Duration.between(start, now).toNanos
}
