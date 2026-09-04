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
}
