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

  // `timeout` was accepted and then discarded (the wait was `Duration.Inf`), so one
  // fetch hung past every HTTP bound held the whole batch — and with it the scrape
  // permit the caller was sitting on. It now bounds EACH fetch from its own start: a
  // fetch that overruns loses only its own key, the rest of the batch returns.
  it should "drop only the key of a fetch that overruns the timeout and return the rest" in {
    val result = ParallelDetailFetch("test-slow", Seq("fast", "slow"), 100.millis) { url =>
      if (url == "slow") Thread.sleep(2000)
      url.toUpperCase
    }
    result shouldBe Map("fast" -> "FAST")
  }

  it should "time each fetch from its own start, not the batch's (a queued fetch is not charged for the wait)" in {
    // Serial (maxConcurrent = 1), each fetch 60ms, timeout 100ms: the fifth starts
    // ~240ms after the batch did. A batch deadline would cut it off; a per-fetch
    // one lets every one finish.
    val result = ParallelDetailFetch("test-per-fetch", (1 to 5).map(_.toString), 100.millis, maxConcurrent = 1) { url =>
      Thread.sleep(60)
      url
    }
    result.keySet shouldBe (1 to 5).map(_.toString).toSet
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

  it should "fail only the key whose fetch overran the timeout" in {
    val result = ParallelDetailFetch.keyed("test-keyed-timeout", Seq("hangs", "answers"), 100.millis)(k => s"https://x/$k") { url =>
      if (url.endsWith("hangs")) Thread.sleep(2000)
      url.length
    }
    result shouldBe Map("answers" -> 17)
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
