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

  /** A 404/410 says something permanent about the URL, unlike a blip. Kinoteka and
   *  Cinema City between them serve 98 permanently-missing detail pages in the Polish
   *  corpus; with every failure re-tried, each one is re-fetched on EVERY scrape pass,
   *  forever, and the film it belongs to never gets the year/director its TMDB
   *  resolution is gated on. Same {404, 410} rule `HttpStatusException.isDurable` draws. */
  it should "remember a 404, so a permanently-missing detail page is fetched once" in {
    val under = new CountingFetch(Map("gone" -> (() => throw new HttpStatusException(404, "GET", "gone", None))))
    val c = new CachingDetailFetch(under, ttl = 1.hour)
    a [HttpStatusException] should be thrownBy c.get("gone")
    a [HttpStatusException] should be thrownBy c.get("gone")
    a [HttpStatusException] should be thrownBy c.get("gone")
    under.calls shouldBe 1
  }

  it should "remember a 410 the same way" in {
    val under = new CountingFetch(Map("gone" -> (() => throw new HttpStatusException(410, "GET", "gone", None))))
    val c = new CachingDetailFetch(under, ttl = 1.hour)
    a [HttpStatusException] should be thrownBy c.get("gone")
    a [HttpStatusException] should be thrownBy c.get("gone")
    under.calls shouldBe 1
  }

  it should "re-raise a remembered 404 with its status intact, not a bare failure" in {
    val under = new CountingFetch(Map("gone" -> (() => throw new HttpStatusException(404, "GET", "gone", None))))
    val c = new CachingDetailFetch(under, ttl = 1.hour)
    a [HttpStatusException] should be thrownBy c.get("gone")
    the [HttpStatusException] thrownBy c.get("gone") should have (Symbol("code") (404))
  }

  it should "still retry a 500, which says nothing permanent about the URL" in {
    val under = new CountingFetch(Map("flaky" -> (() => throw new HttpStatusException(500, "GET", "flaky", None))))
    val c = new CachingDetailFetch(under, ttl = 1.hour)
    a [HttpStatusException] should be thrownBy c.get("flaky")
    a [HttpStatusException] should be thrownBy c.get("flaky")
    under.calls shouldBe 2
  }

  /** THE BOUND IS BYTES, NOT ENTRIES. `maximumSize(10000)` counted entries whose
   *  size spans four orders of magnitude, so it never engaged: worker-pl was found
   *  holding 1,015 cached bodies worth 228 MiB — 73% of its 313 MiB old gen — while
   *  9,000 entries below the count bound. Each body here is 400 KiB, so a 1 MiB
   *  budget holds two; asking for a third must evict the least-recently-used one
   *  rather than grow. */
  it should "evict by total body size, so a few large pages cannot fill the heap" in {
    val big   = "x" * (400 * 1024)
    val under = new CountingFetch(Map("a" -> (() => big), "b" -> (() => big), "c" -> (() => big)))
    val c = new CachingDetailFetch(under, ttl = 1.hour, maxBytes = 1024 * 1024,
                                   maintenance = (r: Runnable) => r.run())
    c.get("a"); c.get("b"); c.get("c")
    under.calls shouldBe 3
    c.get("c"); c.get("b")
    under.calls shouldBe 3   // the two most recent still cached
    c.get("a")
    under.calls shouldBe 4   // the oldest was evicted to stay inside the budget
  }

  /** A Polish detail page is stored UTF-16 — `ł`/`ż` are outside Latin-1, so the
   *  compact-String optimisation is off and every char costs two bytes. That is
   *  half of why worker-pl blew its heap where the bigger UK corpus did not, so
   *  the weigher has to charge the bytes actually retained, not the char count. */
  it should "charge a non-Latin-1 body the two bytes per char it actually retains" in {
    val weigh = (s: String) => CachingDetailFetch.RetainedBytes.weigh("url", CachingDetailFetch.Body(s))
    weigh("abc")           shouldBe 3   // Latin-1: compact, one byte per char
    weigh("Kino Miejskie") shouldBe 13
    weigh("łłł")           shouldBe 6   // outside Latin-1: UTF-16, two bytes per char
    weigh("Kino Wisła")    shouldBe 20  // ONE such char makes the WHOLE string UTF-16
  }

  /** Weight 0 reads to Caffeine as "never evict", which would pin remembered
   *  failures for the whole TTL no matter how many a broken host produced. */
  it should "give a remembered failure a non-zero weight" in {
    CachingDetailFetch.RetainedBytes.weigh("url", CachingDetailFetch.Gone(404)) should be > 0
  }

  it should "let a remembered 404 expire with the TTL, so a restored page comes back" in {
    val tick  = new FakeTicker
    var fail  = true
    val under = new CountingFetch(Map("u" -> (() => if (fail) throw new HttpStatusException(404, "GET", "u", None) else "BACK")))
    val c = new CachingDetailFetch(under, ttl = 1.hour, ticker = tick)
    a [HttpStatusException] should be thrownBy c.get("u")
    fail = false
    tick.nanos = 2.hours.toNanos
    c.get("u") shouldBe "BACK"
  }
}
