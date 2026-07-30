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
    // A remembered 404 and a live one used to read identically in the log, which made
    // a fully-cached run indistinguishable from one re-fetching every miss.
    withClue("a replayed failure must say it was replayed: ") {
      replayed.getMessage should include ("remembered")
    }

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

  /** Answers anything, so a test about the STORE isn't limited by a script. */
  private class AlwaysAnswering extends HttpFetch {
    override def get(url: String): String = "answer"
    override def post(url: String, body: String, contentType: String): String = "answer"
  }

  /** `round` keeps the URLs distinct between calls: a repeat of the same URL is an
   *  in-memory HIT and never reaches the store at all, so a second sweep over the
   *  same list would silently assert nothing. */
  private def fill(fetch: CachingEnrichmentFetch, count: Int, round: Int = 1): Unit =
    (1 to count).foreach(index => fetch.get(s"https://example.test/miss-$round-$index"))

  // The failure this exists to prevent, in full: CI pointed the cache at a tunnel
  // that was never started, so every write blocked for the driver's 5s server
  // selection and then failed. The writes are on the fetch path — `remember` holds
  // the key's single-flight lock — so a thousand misses cost well over an hour, and
  // all three convergence legs were cancelled at the 75-minute ceiling having done
  // nothing but wait. A store that cannot be reached has to stop being asked.
  it should "stop writing through to a store that keeps failing, rather than pay its timeout on every miss" in {
    val store = new UnreachableEnrichmentCacheStore
    val fetch = new CachingEnrichmentFetch(new EnrichmentCache(store), new AlwaysAnswering)

    fill(fetch, 40)

    store.attempts shouldBe EnrichmentCache.MaxConsecutiveWriteFailures
  }

  // Degrading to "no cache" is the whole allowance: the run costs live fills, never
  // correctness. It must not also start failing the fetches.
  it should "keep answering from memory while its store is unreachable" in {
    val delegate = new ScriptedHttpFetch(Map("https://example.test/a" -> (() => "body")))
    val fetch = new CachingEnrichmentFetch(new EnrichmentCache(new UnreachableEnrichmentCacheStore), delegate)

    fetch.get("https://example.test/a") shouldBe "body"
    fetch.get("https://example.test/a") shouldBe "body"

    withClue("an unwritable store must not cost the in-memory hit: ") { delegate.calls shouldBe 1 }
  }

  // The tunnel these run over dies and RESTARTS, so tripping permanently would give
  // up a warm cache for the rest of a run over a fault that healed in seconds.
  it should "probe the store again once the suspension has elapsed" in {
    val store = new UnreachableEnrichmentCacheStore
    var now   = 0L
    val fetch = new CachingEnrichmentFetch(new EnrichmentCache(store, () => now), new AlwaysAnswering)

    fill(fetch, 10)
    store.attempts shouldBe EnrichmentCache.MaxConsecutiveWriteFailures

    now += EnrichmentCache.WriteSuspension.toMillis + 1
    fill(fetch, 10, round = 2)

    withClue("one probe after the cooldown, then suspended again: ") {
      store.attempts shouldBe EnrichmentCache.MaxConsecutiveWriteFailures + 1
    }
  }

  // A 404 is a VERDICT about the URL: Rotten Tomatoes has no such slug, and it will
  // not have one tomorrow either. Worth keeping — it is the bulk of what makes a warm
  // run fast.
  it should "persist a 404, which is a verdict about the URL rather than the moment" in {
    val delegate = new ScriptedHttpFetch(Map(
      "https://www.rottentomatoes.com/m/nope" ->
        (() => throw new HttpStatusException(404, "GET", "https://www.rottentomatoes.com/m/nope", None))))
    val (fetch, _, store) = cacheOver(delegate)

    the [HttpStatusException] thrownBy fetch.get("https://www.rottentomatoes.com/m/nope")

    store.writes shouldBe 1
  }

  // A 429 / 503 / socket timeout says nothing about the URL — only about the moment.
  // Persisting one pins a rate-limit as though it were an answer, and every later run
  // replays "this film has no rating" without ever asking again. The corpus quietly
  // loses coverage and the suite still passes, because a remembered failure looks
  // exactly like a remembered verdict.
  private def transientCases: Seq[(String, () => Nothing)] = Seq(
    "rate limited"  -> (() => throw new HttpStatusException(429, "GET", "https://example.test/t", None)),
    "server error"  -> (() => throw new HttpStatusException(503, "GET", "https://example.test/t", None)),
    "blocked"       -> (() => throw new HttpStatusException(403, "GET", "https://example.test/t", None)),
    "timed out"     -> (() => throw new java.net.SocketTimeoutException("read timed out")))

  transientCases.foreach { case (label, boom) =>
    it should s"NOT persist a $label failure, so the next run retries it" in {
      val (fetch, _, store) = cacheOver(new ScriptedHttpFetch(Map("https://example.test/t" -> boom)))

      the [Exception] thrownBy fetch.get("https://example.test/t")

      withClue(s"a '$label' failure must not become a remembered verdict: ") { store.writes shouldBe 0 }
      store.loadAll() shouldBe empty
    }
  }

  // It must still be remembered for THIS run. Three concurrent passes share one cache,
  // and if a rate-limited URL were simply re-asked, one pass could get the 429 and
  // another the 200 — which the order-independence test would report as an
  // order-dependence that isn't one.
  it should "still replay a transient failure within the run it happened in" in {
    val delegate = new ScriptedHttpFetch(Map(
      "https://example.test/t" -> (() => throw new HttpStatusException(429, "GET", "https://example.test/t", None))))
    val (fetch, cache, _) = cacheOver(delegate)

    the [HttpStatusException] thrownBy fetch.get("https://example.test/t")
    the [HttpStatusException] thrownBy fetch.get("https://example.test/t")

    withClue("a transient failure must not be re-asked mid-run: ") { delegate.calls shouldBe 1 }
    cache.statistics.transient shouldBe 1
  }

  // Where a recording fixture tree is on disk, every successful response is already
  // written there — `RecordingHttpFetch` covers get, getBytes and post, and `getAsync`
  // delegates to `get`. Keeping a second copy in the cache tripled a country's tarball
  // (the UK reached 434 MB) for something the fixtures are consulted for FIRST and the
  // cache is therefore never asked about. What only the cache can hold is the verdict a
  // response never arrived for.
  it should "leave successes to the fixture tree when one is recording them" in {
    val store = new InMemoryEnrichmentCacheStore()
    val cache = new EnrichmentCache(store, persistSuccesses = false)
    val delegate = new ScriptedHttpFetch(Map("https://example.test/a" -> (() => "body")))
    val fetch = new CachingEnrichmentFetch(cache, delegate)

    fetch.get("https://example.test/a") shouldBe "body"

    withClue("the tree already holds this response: ") { store.writes shouldBe 0 }
    withClue("but the run must still answer from memory: ") {
      fetch.get("https://example.test/a") shouldBe "body"
      delegate.calls shouldBe 1
    }
  }

  // The whole point of the cache in that arrangement. A 404 produces no response for
  // the recorder to write, so if this isn't persisted nothing is, and every run re-asks
  // every unresolved film's rating slugs.
  it should "still persist a 404 when successes are left to the tree" in {
    val store = new InMemoryEnrichmentCacheStore()
    val cache = new EnrichmentCache(store, persistSuccesses = false)
    val fetch = new CachingEnrichmentFetch(cache, new ScriptedHttpFetch(Map(
      "https://www.rottentomatoes.com/m/nope" ->
        (() => throw new HttpStatusException(404, "GET", "https://www.rottentomatoes.com/m/nope", None)))))

    the [HttpStatusException] thrownBy fetch.get("https://www.rottentomatoes.com/m/nope")

    store.writes shouldBe 1
  }

  it should "resume writing through once the store answers again" in {
    val store = new IntermittentEnrichmentCacheStore(failFirst = EnrichmentCache.MaxConsecutiveWriteFailures - 1)
    val fetch = new CachingEnrichmentFetch(new EnrichmentCache(store), new AlwaysAnswering)

    fill(fetch, 10)

    withClue("a recovered store must be written to, not left tripped by old failures: ") {
      store.writes shouldBe 10 - (EnrichmentCache.MaxConsecutiveWriteFailures - 1)
    }
  }
}
