package services.tasks

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.metrics.WorkerTaskMetrics

class ScrapeThrottleMonitorSpec extends AnyFlatSpec with Matchers {

  private val scrape = Task("id", TaskType.ScrapeCinema, "k", Map.empty, attempts = 1)
  private def feed(m: ScrapeThrottleMonitor, millis: Long, n: Int): Unit =
    (1 to n).foreach(_ => m.onFinished(scrape, WorkerTaskMetrics.Outcome.Done, millis))

  // slow=12s, window of 6, trip at 4 slow, recover at <=1 slow.
  private def monitor =
    new ScrapeThrottleMonitor(slowMillis = 12000, windowSize = 6, enterCount = 4, exitCount = 1)

  "ScrapeThrottleMonitor" should "stay healthy under fast scrapes" in {
    val m = monitor
    feed(m, 3000, 10)
    m.isThrottled shouldBe false
  }

  it should "trip throttled once a majority of recent scrapes are slow" in {
    val m = monitor
    feed(m, 3000, 6)
    m.isThrottled shouldBe false
    feed(m, 30000, 4) // 4 of the last 6 now slow → broadly slow → trip
    m.isThrottled shouldBe true
  }

  // The 2026-06-21 false-positive: a single ~57s host (slow TLS / a hung detail
  // fetch) among otherwise-fast scrapes spiked the old EWMA past its enter line and
  // tripped a needless backoff while real credit sat at ~48k. A lone outlier is a
  // minority of the window, so the population signal must NOT throttle on it.
  it should "NOT trip on a single slow outlier among fast scrapes" in {
    val m = monitor
    feed(m, 3000, 5)
    m.onFinished(scrape, WorkerTaskMetrics.Outcome.Done, 57000) // one stalled host
    feed(m, 3000, 3)
    m.isThrottled shouldBe false
  }

  it should "NOT trip even on intermittent slow outliers below the enter count" in {
    val m = monitor
    // 3 of 6 slow, scattered — still under the 4-of-6 enter count.
    Seq(3000L, 57000L, 3000L, 57000L, 3000L, 57000L).foreach(d =>
      m.onFinished(scrape, WorkerTaskMetrics.Outcome.Done, d))
    m.isThrottled shouldBe false
  }

  it should "hold throttled through the hysteresis band, then recover once scrapes are fast again" in {
    val m = monitor
    feed(m, 30000, 6)
    m.isThrottled shouldBe true
    // Still 2 slow in the window (> exit count 1) → must hold.
    feed(m, 3000, 4)
    m.isThrottled shouldBe true
    // Now only fast scrapes remain → slow count drops to/below 1 → recovered.
    feed(m, 3000, 2)
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
