package tools

import java.time.Instant

import scala.concurrent.duration.*

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The worker's Filmweb fallback ids were a lazy val over a read whose failure read as "none": one failed boot read
 *  removed every Filmweb fallback for the process's life. */
class RetriedLookupSpec extends AnyFlatSpec with Matchers {

  "A retried lookup" should "stand in its fallback after a failed read and read again once the wait is over" in {
    val clock = new MutableClock(Instant.parse("2026-11-21T06:00:00Z"))
    var reads = 0
    val lookup = new RetriedLookup[Int]("ids", () => { reads += 1; if (reads == 1) throw new java.io.IOException("reset") else 42 },
      fallback = 0, retryAfter = 10.minutes, clock)

    lookup.current shouldBe 0
    lookup.current shouldBe 0
    reads shouldBe 1                       // not re-read on every ask while it waits
    clock.advance(java.time.Duration.ofMinutes(10))
    lookup.current shouldBe 42
    clock.advance(java.time.Duration.ofDays(1))
    lookup.current shouldBe 42
    reads shouldBe 2                       // kept once read
  }
}
