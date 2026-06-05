package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.util.concurrent.TimeoutException
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.duration._

class ParallelDetailFetchSpec extends AnyFlatSpec with Matchers {

  "ParallelDetailFetch" should "return fetched results keyed by URL" in {
    val result = ParallelDetailFetch("test-happy", Seq("a", "b", "c"), 5.seconds) { url =>
      url.toUpperCase
    }
    result shouldBe Map("a" -> "A", "b" -> "B", "c" -> "C")
  }

  it should "return an empty map for empty URLs without creating an EC" in {
    val result = ParallelDetailFetch("test-empty", Seq.empty[String], 5.seconds) { _ =>
      fail("should not be called")
    }
    result shouldBe empty
  }

  it should "throw TimeoutException when a fetch exceeds the timeout" in {
    a[TimeoutException] should be thrownBy {
      ParallelDetailFetch("test-timeout", Seq("fast", "slow"), 500.millis) { url =>
        if (url == "slow") Thread.sleep(5000)
        url
      }
    }
  }

  it should "propagate exceptions from the fetch function" in {
    an[Exception] should be thrownBy {
      ParallelDetailFetch("test-error", Seq("ok", "boom"), 5.seconds) { url =>
        if (url == "boom") throw new RuntimeException("kaboom")
        url
      }
    }
  }

  it should "cap concurrent fetches at maxConcurrent" in {
    // Without the cap a cinema with many films spins up one parsing thread per
    // film at once, spiking the (single) vCPU on a cold-start scrape.
    val active    = new AtomicInteger(0)
    val maxActive = new AtomicInteger(0)
    val urls      = (1 to 8).map(_.toString)
    val result = ParallelDetailFetch("test-cap", urls, 10.seconds, maxConcurrent = 2) { url =>
      val cur = active.incrementAndGet()
      maxActive.updateAndGet(m => math.max(m, cur))
      try Thread.sleep(40) finally active.decrementAndGet()
      url
    }
    result.size  shouldBe 8
    maxActive.get should be <= 2
  }
}
