package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BoundedCacheSpec extends AnyFlatSpec with Matchers {

  private def settled(cache: com.github.benmanes.caffeine.cache.Cache[?, ?]): Long = { cache.cleanUp(); cache.estimatedSize() }

  "A size-bounded cache" should "hold at most its bound" in {
    val cache = BoundedCache.ofSize(10).executor((r: Runnable) => r.run()).build[Integer, String]()
    (0 until 100).foreach(i => cache.put(i, "x"))
    settled(cache) should be <= 10L
  }

  // Caffeine never evicts a zero-weight entry: a weigher answering 0 for an empty value made the
  // byte bound bound nothing, and every such entry pinned its key for the life of the process.
  "A weight-bounded cache" should "evict entries whose value weighs nothing" in {
    val cache = BoundedCache.ofWeight[Integer, String](maxWeight = 10L * BoundedCache.MinEntryOverhead)((_, v) => v.length)
      .executor((r: Runnable) => r.run()).build[Integer, String]()
    (0 until 100).foreach(i => cache.put(i, ""))
    settled(cache) should be <= 10L
  }

  it should "charge every entry its overhead on top of the weigher, saturating rather than overflowing" in {
    BoundedCache.weight(64, 0) shouldBe 64
    BoundedCache.weight(64, -5) shouldBe 64
    BoundedCache.weight(64, 100) shouldBe 164
    BoundedCache.weight(64, Int.MaxValue) shouldBe Int.MaxValue
  }

  it should "refuse an overhead that would let entries weigh almost nothing" in {
    an[IllegalArgumentException] should be thrownBy BoundedCache.ofWeight[String, String](100, entryOverhead = 0)((_, _) => 0)
  }
}
