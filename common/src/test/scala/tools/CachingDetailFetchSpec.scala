package tools

import com.github.benmanes.caffeine.cache.Ticker
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration._

class CachingDetailFetchSpec extends AnyFlatSpec with Matchers {

  /** Counts underlying calls; each URL maps to a thunk so a response can flip. */
  private class CountingFetch(responses: Map[String, () => String]) extends GetOnlyHttpFetch {
    var calls = 0
    override def get(url: String): String = { calls += 1; responses(url)() }
  }

  private class FakeTicker extends Ticker {
    @volatile var nanos = 0L
    override def read(): Long = nanos
  }

  "CachingDetailFetch" should "fetch once and serve the cached body on repeat within the TTL" in {
    val under = new CountingFetch(Map("u" -> (() => "BODY")))
    val c = new CachingDetailFetch(under, ttl = 1.hour)
    c.get("u") shouldBe "BODY"
    c.get("u") shouldBe "BODY"
    under.calls shouldBe 1
  }

  it should "re-fetch once the TTL has elapsed" in {
    val tick = new FakeTicker
    val under = new CountingFetch(Map("u" -> (() => "BODY")))
    val c = new CachingDetailFetch(under, ttl = 1.hour, ticker = tick)
    c.get("u")
    tick.nanos = 2.hours.toNanos
    c.get("u")
    under.calls shouldBe 2
  }

  it should "NOT cache a failed fetch, so a transient blip isn't pinned for the TTL" in {
    var fail = true
    val under = new CountingFetch(Map("u" -> (() => if (fail) throw new RuntimeException("boom") else "OK")))
    val c = new CachingDetailFetch(under, ttl = 1.hour)
    a[RuntimeException] should be thrownBy c.get("u")
    fail = false
    c.get("u") shouldBe "OK" // retried — not serving a cached failure
    under.calls shouldBe 2
  }

  it should "cache each URL independently" in {
    val under = new CountingFetch(Map("a" -> (() => "A"), "b" -> (() => "B")))
    val c = new CachingDetailFetch(under)
    c.get("a"); c.get("b"); c.get("a"); c.get("b")
    under.calls shouldBe 2
  }
}
