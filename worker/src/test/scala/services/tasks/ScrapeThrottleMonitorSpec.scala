package services.tasks

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.metrics.WorkerTaskMetrics

class ScrapeThrottleMonitorSpec extends AnyFlatSpec with Matchers {

  private val scrape = Task("id", TaskType.ScrapeCinema, "k", Map.empty, attempts = 1)
  private def feed(m: ScrapeThrottleMonitor, millis: Long, n: Int): Unit =
    (1 to n).foreach(_ => m.onFinished(scrape, WorkerTaskMetrics.Outcome.Done, millis))

  // alpha=0.5 makes the EWMA move fast enough to assert in a few samples.
  private def monitor = new ScrapeThrottleMonitor(throttleMillis = 12000, recoverMillis = 6000, alpha = 0.5)

  "ScrapeThrottleMonitor" should "stay healthy under fast scrapes" in {
    val m = monitor
    feed(m, 3000, 10)
    m.isThrottled shouldBe false
  }

  it should "trip throttled once the duration EWMA crosses the enter line" in {
    val m = monitor
    feed(m, 3000, 3)
    m.isThrottled shouldBe false
    feed(m, 30000, 10) // sustained slow → EWMA climbs past the 12s enter line
    m.isThrottled shouldBe true
  }

  it should "hold throttled through the hysteresis band, then recover below the exit line" in {
    val m = monitor
    feed(m, 30000, 10)
    m.isThrottled shouldBe true
    // A scrape between the exit (6s) and enter (12s) lines must NOT clear it.
    feed(m, 9000, 1)
    m.isThrottled shouldBe true
    // Fast scrapes pull the EWMA below the exit line → recovered.
    feed(m, 2000, 10)
    m.isThrottled shouldBe false
  }

  it should "ignore non-scrape task types and non-done outcomes" in {
    val m = monitor
    val enrich = Task("id", TaskType.EnrichDetails, "k", Map.empty, attempts = 1)
    (1 to 20).foreach(_ => m.onFinished(enrich, WorkerTaskMetrics.Outcome.Done, 60000)) // slow, but not a scrape
    (1 to 20).foreach(_ => m.onFinished(scrape, WorkerTaskMetrics.Outcome.Failed, 60000)) // a scrape, but not done
    m.isThrottled shouldBe false
  }
}
