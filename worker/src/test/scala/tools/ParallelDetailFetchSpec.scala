package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

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

  it should "wait for a slow fetch instead of cutting it off (no batch timeout)" in {
    // The `timeout` arg is now ignored — each fetch runs to completion (bounded
    // only by the HTTP layer), so a fetch slower than the old timeout still
    // returns its result instead of throwing.
    val result = ParallelDetailFetch("test-slow", Seq("fast", "slow"), 100.millis) { url =>
      if (url == "slow") Thread.sleep(400)
      url.toUpperCase
    }
    result shouldBe Map("fast" -> "FAST", "slow" -> "SLOW")
  }

  it should "default the concurrency cap to 2" in {
    val active    = new AtomicInteger(0)
    val maxActive = new AtomicInteger(0)
    ParallelDetailFetch("test-default-cap", (1 to 8).map(_.toString), 5.seconds) { url =>
      val cur = active.incrementAndGet()
      maxActive.updateAndGet(m => math.max(m, cur))
      try Thread.sleep(40) finally active.decrementAndGet()
      url
    }
    maxActive.get should be <= 2
  }

  it should "run strictly serially at maxConcurrent = 1 (the day-axis setting)" in {
    // The two-axis clients set maxConcurrent = 1 on their day/event fetch so days
    // are fetched one at a time. Lock that 1 means truly serial — never two at
    // once — since that's the guarantee those call sites now depend on.
    val active    = new AtomicInteger(0)
    val maxActive = new AtomicInteger(0)
    ParallelDetailFetch("test-serial", (1 to 6).map(_.toString), 5.seconds, maxConcurrent = 1) { url =>
      val cur = active.incrementAndGet()
      maxActive.updateAndGet(m => math.max(m, cur))
      try Thread.sleep(20) finally active.decrementAndGet()
      url
    }
    maxActive.get shouldBe 1
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

  // `keyed` is the shape every cinema client uses: keep the caller's domain key
  // (film id / slug / date) while fetching a derived URL under the same cap.
  "ParallelDetailFetch.keyed" should "key results by the domain key, not the URL" in {
    val result = ParallelDetailFetch.keyed("test-keyed", Seq("aftersun", "tar"), 5.seconds)(slug => s"https://x/movies/$slug") { url =>
      url.length
    }
    result shouldBe Map("aftersun" -> 25, "tar" -> 20)
  }

  it should "fetch each distinct key only once" in {
    val fetched = new AtomicInteger(0)
    val result = ParallelDetailFetch.keyed("test-keyed-dedup", Seq("a", "a", "b"), 5.seconds)(k => k) { _ =>
      fetched.incrementAndGet()
      "ok"
    }
    result shouldBe Map("a" -> "ok", "b" -> "ok")
    fetched.get shouldBe 2
  }

  it should "cap concurrent fetches at maxConcurrent regardless of key count" in {
    val active    = new AtomicInteger(0)
    val maxActive = new AtomicInteger(0)
    val keys      = (1 to 30).map(i => s"film-$i")
    val result = ParallelDetailFetch.keyed("test-keyed-cap", keys, 10.seconds, maxConcurrent = 6)(k => s"https://x/$k") { _ =>
      val cur = active.incrementAndGet()
      maxActive.updateAndGet(m => math.max(m, cur))
      try Thread.sleep(20) finally active.decrementAndGet()
      "ok"
    }
    result.size shouldBe 30
    maxActive.get should be <= 6
  }
}
