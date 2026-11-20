package tools

import com.github.benmanes.caffeine.cache.{Cache, Caffeine}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CacheOccupancySpec extends AnyFlatSpec with Matchers {

  /** A cache holding two values, its maintenance run on the calling thread so its sizes are exact at once. */
  private def holdingTwo(builder: Caffeine[Object, Object]): Cache[String, String] = {
    val cache = builder.executor(_.run()).build[String, String]()
    cache.put("a", "12345"); cache.put("b", "123")
    cache.cleanUp()
    cache
  }

  "a cache's occupancy" should "report a weighed cache's maximum and held weight as bytes, never as entries" in {
    val weighed = holdingTwo(Caffeine.newBuilder().maximumWeight(100).weigher[Object, Object]((_, v) => v.toString.length))
    CacheOccupancy.of(weighed, weighted = true) shouldBe CacheOccupancy(entries = 2, maxBytes = Some(100), heldBytes = Some(8))
  }

  it should "report a count-bounded cache's maximum as entries, with no bytes" in {
    val counted = holdingTwo(Caffeine.newBuilder().maximumSize(10))
    CacheOccupancy.of(counted, weighted = false) shouldBe CacheOccupancy(entries = 2, maxEntries = Some(10))
  }

  it should "report no maximum for an unbounded cache, weighed or not" in {
    val unbounded = holdingTwo(Caffeine.newBuilder())
    CacheOccupancy.of(unbounded, weighted = false) shouldBe CacheOccupancy(entries = 2)
    CacheOccupancy.of(unbounded, weighted = true) shouldBe CacheOccupancy(entries = 2)
  }

  it should "report evictions and the hit ratio only where stats are recorded and asked" in {
    val recording = holdingTwo(Caffeine.newBuilder().maximumSize(10).recordStats())
    CacheOccupancy.of(recording, weighted = false).hitRatio shouldBe None
    recording.getIfPresent("a"); recording.getIfPresent("z")
    val read = CacheOccupancy.of(recording, weighted = false)
    read.evictions shouldBe Some(0L)
    read.hitRatio shouldBe Some(0.5)
  }
}
