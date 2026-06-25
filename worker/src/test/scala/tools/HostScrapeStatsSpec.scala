package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration._

class HostScrapeStatsSpec extends AnyFlatSpec with Matchers {

  "HostScrapeStats.median" should "take the middle of an odd-length set and average the two middles of an even one" in {
    HostScrapeStats.median(Array(5L, 1L, 3L)) shouldBe 3.0
    HostScrapeStats.median(Array(1L, 2L, 3L, 4L)) shouldBe 2.5
    HostScrapeStats.median(Array.empty[Long]) shouldBe 0.0
  }

  "deadlineFor" should "hand back the ceiling while a host is still warming up (< minSamples)" in {
    val stats = new HostScrapeStats(minSamples = 5, ceiling = 30.seconds)
    (1 to 4).foreach(_ => stats.record("h", 1000))
    stats.deadlineFor("h") shouldBe 30.seconds
  }

  it should "cut at 2× the rolling median once it has enough samples" in {
    val stats = new HostScrapeStats(minSamples = 5, multiplier = 2.0, floor = 1.second, ceiling = 60.seconds)
    (1 to 5).foreach(_ => stats.record("h", 5000)) // median 5s → 2× = 10s
    stats.deadlineFor("h") shouldBe 10.seconds
  }

  it should "clamp UP to the floor for a very fast host (jitter headroom)" in {
    val stats = new HostScrapeStats(minSamples = 3, multiplier = 2.0, floor = 8.seconds, ceiling = 45.seconds)
    (1 to 3).foreach(_ => stats.record("fast", 500)) // 2×500ms = 1s < floor
    stats.deadlineFor("fast") shouldBe 8.seconds
  }

  it should "clamp DOWN to the ceiling for a chronically slow host" in {
    val stats = new HostScrapeStats(minSamples = 3, multiplier = 2.0, floor = 8.seconds, ceiling = 45.seconds)
    (1 to 3).foreach(_ => stats.record("slow", 30000)) // 2×30s = 60s > ceiling
    stats.deadlineFor("slow") shouldBe 45.seconds
  }

  it should "only consider the most recent `window` samples" in {
    val stats = new HostScrapeStats(window = 4, minSamples = 1, multiplier = 1.0, floor = 1.milli, ceiling = 1.hour)
    (1 to 10).foreach(_ => stats.record("h", 100)) // old, slow-ish
    (1 to 4).foreach(_ => stats.record("h", 10))   // last 4 evict the 100s
    stats.deadlineFor("h") shouldBe 10.millis        // median of {10,10,10,10}
  }

  it should "track each host independently" in {
    val stats = new HostScrapeStats(minSamples = 2, multiplier = 1.0, floor = 1.milli, ceiling = 1.hour)
    (1 to 2).foreach(_ => stats.record("a", 1000))
    (1 to 2).foreach(_ => stats.record("b", 9000))
    stats.deadlineFor("a") shouldBe 1000.millis
    stats.deadlineFor("b") shouldBe 9000.millis
  }

  it should "ignore a negative duration rather than poisoning the median" in {
    val stats = new HostScrapeStats(minSamples = 1, multiplier = 1.0, floor = 1.milli, ceiling = 1.hour)
    stats.record("h", -5)
    stats.deadlineFor("h") shouldBe 1.hour // still warming up — the bad sample was dropped
    stats.record("h", 2000)
    stats.deadlineFor("h") shouldBe 2000.millis
  }
}
