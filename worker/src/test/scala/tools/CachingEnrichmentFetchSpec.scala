package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * The convergence suite's enrichment cache. The behaviour that matters is not
 * "a second call is faster" but "a second call gets the SAME answer" — including
 * when the first answer was a failure, which is most of a country's corpus.
 */
class CachingEnrichmentFetchSpec extends AnyFlatSpec with Matchers {

  /** A delegate that answers from a script and counts what it was asked, so a
   *  test can assert the live service was consulted exactly once. */
  private class ScriptedHttpFetch(answers: Map[String, () => String]) extends HttpFetch {
    var calls: Int = 0
    var byteCalls: Int = 0
    var bytes: Array[Byte] = Array.emptyByteArray
    override def get(url: String): String = {
      calls += 1
      answers.getOrElse(url, () => throw new NoSuchElementException(url))()
    }
    override def getBytes(url: String): Array[Byte] = { byteCalls += 1; bytes }
    override def post(url: String, body: String, contentType: String): String = {
      calls += 1
      answers.getOrElse(s"$url|$body", () => throw new NoSuchElementException(url))()
    }
  }

  private def cacheOver(delegate: HttpFetch, store: InMemoryEnrichmentCacheStore = new InMemoryEnrichmentCacheStore())
      : (CachingEnrichmentFetch, EnrichmentCache, InMemoryEnrichmentCacheStore) = {
    val cache = new EnrichmentCache(store)
    (new CachingEnrichmentFetch(cache, delegate), cache, store)
  }

  "the enrichment cache" should "ask the live service once and replay the body after" in {
    val delegate = new ScriptedHttpFetch(Map("https://api.themoviedb.org/3/search?q=dune" -> (() => """{"id":1}""")))
    val (fetch, cache, _) = cacheOver(delegate)

    fetch.get("https://api.themoviedb.org/3/search?q=dune") shouldBe """{"id":1}"""
    fetch.get("https://api.themoviedb.org/3/search?q=dune") shouldBe """{"id":1}"""

    delegate.calls shouldBe 1
    cache.statistics.hits shouldBe 1
    cache.statistics.fills shouldBe 1
  }

  it should "write every answer through to the store, so the next run starts warm" in {
    val delegate = new ScriptedHttpFetch(Map("https://example.test/a" -> (() => "body")))
    val (fetch, _, store) = cacheOver(delegate)

    fetch.get("https://example.test/a")

    store.writes shouldBe 1
    store.loadAll() shouldBe Map(CachingEnrichmentFetch.keyOf("GET", "https://example.test/a") -> CachedResponse.Body("body"))
  }

  it should "serve a preloaded entry without touching the live service at all" in {
    val delegate = new ScriptedHttpFetch(Map.empty)
    val store = new InMemoryEnrichmentCacheStore(
      Map(CachingEnrichmentFetch.keyOf("GET", "https://example.test/a") -> CachedResponse.Body("from last run")))
    val (fetch, cache, _) = cacheOver(delegate, store)

    cache.preload() shouldBe 1
    fetch.get("https://example.test/a") shouldBe "from last run"
    delegate.calls shouldBe 0
  }

  // The headline behaviour. A corpus is full of titles TMDB will never match, and
  // re-asking about every one of them on every pass is both the slow half of the
  // sweep and the half most likely to answer differently the second time.
  it should "remember a 404 and replay it as the same typed failure, without re-asking" in {
    val delegate = new ScriptedHttpFetch(Map(
      "https://api.themoviedb.org/3/search?q=nope" ->
        (() => throw new HttpStatusException(404, "GET", "https://api.themoviedb.org/3/search?q=nope", None))))
    val (fetch, _, _) = cacheOver(delegate)

    val live = the [HttpStatusException] thrownBy fetch.get("https://api.themoviedb.org/3/search?q=nope")
    live.code shouldBe 404

    val replayed = the [HttpStatusException] thrownBy fetch.get("https://api.themoviedb.org/3/search?q=nope")
    replayed.code shouldBe 404
    replayed.url  shouldBe "https://api.themoviedb.org/3/search?q=nope"

    withClue("a remembered failure must not re-ask the live service: ") { delegate.calls shouldBe 1 }
  }

  // Callers branch on the STATUS — a 404 is a permanent miss, a 5xx is worth a
  // retry — so a replay that flattened them would make pass 2 behave differently
  // from pass 1, which is the precise thing this cache exists to prevent.
  it should "preserve the status code it remembered, not just the fact of failure" in {
    val delegate = new ScriptedHttpFetch(Map(
      "https://example.test/busy" -> (() => throw new HttpStatusException(503, "GET", "https://example.test/busy", None))))
    val (fetch, _, _) = cacheOver(delegate)

    the [HttpStatusException] thrownBy fetch.get("https://example.test/busy")
    the [HttpStatusException] thrownBy fetch.get("https://example.test/busy") should have (Symbol("code") (503))
    withClue("the 503 must have come from the cache, not a second live call: ") { delegate.calls shouldBe 1 }
  }

  it should "remember a network failure and replay it as a cache failure naming the original" in {
    val delegate = new ScriptedHttpFetch(Map(
      "https://example.test/dead" -> (() => throw new java.net.SocketTimeoutException("read timed out"))))
    val (fetch, _, _) = cacheOver(delegate)

    the [java.net.SocketTimeoutException] thrownBy fetch.get("https://example.test/dead")

    val replayed = the [CachedEnrichmentFailure] thrownBy fetch.get("https://example.test/dead")
    replayed.getMessage should include ("SocketTimeoutException")
    replayed.getMessage should include ("read timed out")
    delegate.calls shouldBe 1
  }

  // IMDb's rating query is one POST URL for every film, distinguished only by the
  // GraphQL body — keyed on the URL alone, the whole corpus would share one answer.
  it should "key a POST by its body, so two films don't share one answer" in {
    val delegate = new ScriptedHttpFetch(Map(
      "https://graphql.imdb.test|{\"id\":\"tt1\"}" -> (() => "rating one"),
      "https://graphql.imdb.test|{\"id\":\"tt2\"}" -> (() => "rating two")))
    val (fetch, _, _) = cacheOver(delegate)

    fetch.post("https://graphql.imdb.test", """{"id":"tt1"}""", "application/json") shouldBe "rating one"
    fetch.post("https://graphql.imdb.test", """{"id":"tt2"}""", "application/json") shouldBe "rating two"
    fetch.post("https://graphql.imdb.test", """{"id":"tt1"}""", "application/json") shouldBe "rating one"

    delegate.calls shouldBe 2
  }

  // The base `getBytes` default round-trips through a UTF-8 decode, which mojibakes
  // a legacy single-byte page; the cache holds base64 so the wire bytes survive.
  it should "round-trip raw bytes losslessly rather than through a UTF-8 decode" in {
    val delegate = new ScriptedHttpFetch(Map.empty)
    delegate.bytes = Array[Byte](0x7A, 0xBF.toByte, 0xE6.toByte, 0x00)
    val (fetch, _, _) = cacheOver(delegate)

    fetch.getBytes("https://example.test/legacy") shouldBe delegate.bytes
    fetch.getBytes("https://example.test/legacy") shouldBe delegate.bytes
    delegate.byteCalls shouldBe 1
  }

  it should "keep a body and its raw bytes on separate keys" in {
    val delegate = new ScriptedHttpFetch(Map("https://example.test/x" -> (() => "text")))
    delegate.bytes = Array[Byte](1, 2, 3)
    val (fetch, _, _) = cacheOver(delegate)

    fetch.get("https://example.test/x") shouldBe "text"
    fetch.getBytes("https://example.test/x") shouldBe Array[Byte](1, 2, 3)
    delegate.calls shouldBe 1
    delegate.byteCalls shouldBe 1
  }

  // TMDB and OMDb authenticate in the query string, and this cache is written to a
  // database that outlives the run.
  it should "never put a credential in the key it stores" in {
    val key = CachingEnrichmentFetch.keyOf("GET", "https://api.themoviedb.org/3/movie/1?api_key=deadbeefsecret&language=pl-PL")
    key should not include "deadbeefsecret"
    key should include ("language=pl-PL")
  }

  it should "still tell two differently-parameterised requests apart after masking" in {
    val one = CachingEnrichmentFetch.keyOf("GET", "https://api.themoviedb.org/3/search?api_key=k&query=dune&year=2021")
    val two = CachingEnrichmentFetch.keyOf("GET", "https://api.themoviedb.org/3/search?api_key=k&query=dune&year=1984")
    one should not be two
  }

  it should "count a failed fill separately, so a rate-limited run is visible" in {
    val delegate = new ScriptedHttpFetch(Map(
      "https://example.test/ok"  -> (() => "fine"),
      "https://example.test/429" -> (() => throw new HttpStatusException(429, "GET", "https://example.test/429", None))))
    val (fetch, cache, _) = cacheOver(delegate)

    fetch.get("https://example.test/ok")
    the [HttpStatusException] thrownBy fetch.get("https://example.test/429")

    cache.statistics.fills shouldBe 2
    cache.statistics.failures shouldBe 1
  }

  // Three concurrent passes all miss on the same key at the same instant. Letting
  // both through would put two calls on the wire, and two live answers that
  // disagree (a 429 for one, a 200 for the other) read as an order-dependent
  // divergence that has nothing to do with ordering.
  it should "put only one live call on the wire when several passes miss at once" in {
    val started = new java.util.concurrent.CountDownLatch(1)
    val calls   = new java.util.concurrent.atomic.AtomicInteger(0)
    val slowDelegate = new HttpFetch {
      override def get(url: String): String = {
        calls.incrementAndGet()
        started.countDown()
        Thread.sleep(200)   // hold the fill open so the others are certain to queue
        "answer"
      }
      override def post(url: String, body: String, contentType: String): String = ???
    }
    val cache = new EnrichmentCache(new InMemoryEnrichmentCacheStore())

    val threads = (1 to 4).map(_ => new Thread(() => {
      new CachingEnrichmentFetch(cache, slowDelegate).get("https://example.test/contended")
      ()
    }))
    threads.foreach(_.start())
    threads.foreach(_.join(10000))

    calls.get() shouldBe 1
    cache.statistics.fills shouldBe 1
  }

  // The order-independence test drives three concurrent passes, each with its own
  // wiring and so its own fetch. They must agree about what the live service said.
  it should "share one set of answers across the several fetches built over it" in {
    val delegate = new ScriptedHttpFetch(Map("https://example.test/shared" -> (() => "answer")))
    val cache = new EnrichmentCache(new InMemoryEnrichmentCacheStore())
    val passOne = new CachingEnrichmentFetch(cache, delegate)
    val passTwo = new CachingEnrichmentFetch(cache, delegate)

    passOne.get("https://example.test/shared") shouldBe "answer"
    passTwo.get("https://example.test/shared") shouldBe "answer"

    delegate.calls shouldBe 1
  }
}
