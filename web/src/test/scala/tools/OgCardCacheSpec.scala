package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * The cache is bounded in BYTES, and this is the test that says so.
 *
 * It used to be bounded by entry count (`maximumSize(1000)`), which reads like a
 * bound and is not one: the values are rendered share cards, and a card's size
 * is set by the poster it composites. On 2026-09-04 a crawler swept the share
 * cards and filled that cache with hundreds of megabytes of live byte arrays --
 * the old-gen floor on web-uk went from 29% to 71% of a 384 MiB cap in two
 * hours, and web-us reached 82% on a JVM that had OOMed the day before.
 *
 * Under a count bound the assertions below are unreachable: 40 cards is well
 * inside 1000, so nothing is ever evicted no matter how large they are.
 */
class OgCardCacheSpec extends AnyFlatSpec with Matchers {

  /** Caffeine defers eviction to an executor, and `cleanUp()` skips the work it
   *  cannot take the lock for — so under a full parallel `testUnit` run the
   *  bound below was asserted before anything had been evicted, and only then.
   *  Running maintenance on the calling thread makes `cleanUp()` mean what every
   *  assertion here reads it as. */
  private def cacheOf(maxBytes: Long) = new OgCardCache(maxBytes, (r: Runnable) => r.run())

  private val OneMiB = 1024 * 1024

  private def card(n: Int): Array[Byte] = Array.fill(OneMiB)(n.toByte)

  private def fill(cache: OgCardCache, count: Int): Unit =
    (0 until count).foreach { i => cache.getOrRender(s"film-$i")((card(i), true)) }

  "OgCardCache" should "hold no more than its byte bound however many cards it is given" in {
    val cache = cacheOf(8L * OneMiB)
    fill(cache, 40)
    cache.cleanUp()
    cache.weight should be <= (8L * OneMiB)
  }

  it should "evict the cards it took in first, so a sweep cannot pin the tier's heap" in {
    val cache = cacheOf(4L * OneMiB)
    fill(cache, 40)
    cache.cleanUp()
    // The earliest card is gone; asking for it renders again rather than hitting.
    var rendered = false
    cache.getOrRender("film-0") { rendered = true; (card(0), true) }
    rendered shouldBe true
  }

  it should "still serve a card it is holding without re-rendering it" in {
    val cache = cacheOf(64L * OneMiB)
    cache.getOrRender("film")((card(1), true))
    var rendered = false
    cache.getOrRender("film") { rendered = true; (card(2), true) }
    rendered shouldBe false
  }

  it should "not freeze an incomplete card, so the next share retries the poster" in {
    val cache = cacheOf(64L * OneMiB)
    cache.getOrRender("film")((card(1), false))
    var rendered = false
    cache.getOrRender("film") { rendered = true; (card(1), true) }
    rendered shouldBe true
  }

  /** Runs `callers` getOrRender calls for `key` at once, all released together, each rendering
   *  through `render`. Returns what every caller got back. */
  private def concurrently(cache: OgCardCache, callers: Int, key: String)(render: => (Array[Byte], Boolean)): Seq[Array[Byte]] = {
    val pool  = java.util.concurrent.Executors.newFixedThreadPool(callers)
    val start = new java.util.concurrent.CountDownLatch(1)
    try {
      val futures = (1 to callers).map { _ =>
        pool.submit(() => { start.await(); cache.getOrRender(key)(render) })
      }
      start.countDown()
      futures.map(_.get(30, java.util.concurrent.TimeUnit.SECONDS))
    } finally pool.shutdownNow()
  }

  // A crawler, or one link pasted into a busy group chat, asks for the SAME card many times at once.
  // Every one of those used to miss the cache together and render -- a poster fetch and decode each,
  // the native-memory cost that OOM-killed web-pl on 2026-09-21. Identical concurrent requests share
  // the one render in flight.
  it should "render a card once for identical concurrent requests, and hand every caller its bytes" in {
    val cache   = cacheOf(8L * OneMiB)
    val renders = new java.util.concurrent.atomic.AtomicInteger(0)

    val results = concurrently(cache, callers = 8, key = "film-x") {
      renders.incrementAndGet(); Thread.sleep(300); (card(7), true)
    }

    renders.get() shouldBe 1
    results.foreach(_.head shouldBe 7.toByte)
  }

  // An INCOMPLETE card (its poster failed to load) is shared by the callers already waiting on it --
  // they asked at the same moment and would have hit the same failing origin -- but it is still not
  // frozen: the next request after it renders afresh, so a transient poster failure heals.
  it should "share an uncacheable render with concurrent callers without caching it for later ones" in {
    val cache   = cacheOf(8L * OneMiB)
    val renders = new java.util.concurrent.atomic.AtomicInteger(0)
    def render  = { renders.incrementAndGet(); Thread.sleep(300); (card(1), false) }

    concurrently(cache, callers = 6, key = "film-y")(render)
    renders.get() shouldBe 1

    cache.getOrRender("film-y")(render)
    renders.get() shouldBe 2
  }

  // A render that THROWS must fail every caller sharing it, and must not wedge the key: the next
  // request renders again rather than waiting forever on a flight that has already crashed.
  it should "propagate a failed render to its waiters and let the next request retry" in {
    val cache    = cacheOf(8L * OneMiB)
    val attempts = new java.util.concurrent.atomic.AtomicInteger(0)

    val failure = intercept[java.util.concurrent.ExecutionException] {
      concurrently(cache, callers = 4, key = "film-z") {
        attempts.incrementAndGet(); Thread.sleep(300); throw new IllegalStateException("renderer broke")
      }
    }
    failure.getCause shouldBe an[IllegalStateException]
    attempts.get() shouldBe 1

    cache.getOrRender("film-z")((card(3), true)).head shouldBe 3.toByte
  }
}
