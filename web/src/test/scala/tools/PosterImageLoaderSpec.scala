package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.util.concurrent.atomic.AtomicInteger

class PosterImageLoaderSpec extends AnyFlatSpec with Matchers {

  /** Tracks how many `bytes` calls are ever in flight at once, and always fails
   *  to decode (so `loadFirst` walks every fallback candidate rather than
   *  stopping early). All URLs are `multikino.pl` — a `PosterProxy` `SkipHosts`
   *  entry, so `PosterImageLoader.load`'s proxy fallback never fires a SECOND
   *  `bytes` call per candidate, keeping the concurrency count meaningful. */
  private class TrackingFetch(delayMillis: Long) extends PosterFetch {
    private val inFlight = new AtomicInteger(0)
    val peakConcurrent = new AtomicInteger(0)
    def bytes(url: String): Option[Array[Byte]] = {
      val now = inFlight.incrementAndGet()
      peakConcurrent.updateAndGet(prev => math.max(prev, now))
      Thread.sleep(delayMillis)
      inFlight.decrementAndGet()
      None
    }
  }

  // web-pl OOM kills, 2026-09-17: racing every fallback candidate at once
  // multiplies peak memory (a full image download + decode per candidate) by
  // the candidate count. This caps it, trading some of the latency win
  // `ba49e065c` was for back for a bounded worst case.
  "loadFirst" should "never race more than a small bounded number of fallback candidates at once" in {
    val fetch = new TrackingFetch(delayMillis = 80)
    val loader = new PosterImageLoader(fetch)
    val candidates = (1 to 6).map(i => s"https://www.multikino.pl/poster/$i.jpg")

    val result = loader.loadFirst(candidates)

    result shouldBe None
    fetch.peakConcurrent.get() should be <= 3
  }
}
