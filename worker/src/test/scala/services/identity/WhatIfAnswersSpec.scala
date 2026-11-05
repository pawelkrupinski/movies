package services.identity

import models.Country
import org.bson.{BsonDocument, BsonString}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.adapters.{InMemoryTmdbDocuments, TmdbKind}

import java.nio.file.{Files, Path}

/** Where a what-if's answers come from ([[WhatIfAnswers]]): a question the mirror's store lacks is asked of the real
 *  service once, through the worker's own client and parser, filed into the LOCAL store (never the mirror) keyed as
 *  production keys it, and read from there by every later run; offline, it stays the gap it is. */
class WhatIfAnswersSpec extends AnyFlatSpec with Matchers {
  import WhatIfAnswers.{DiskTmdbDocuments, Layered, Throttled}

  private val clock   = java.time.Clock.fixed(java.time.Instant.parse("2026-11-05T12:00:00Z"), java.time.ZoneOffset.UTC)
  private val poland  = Country.all.find(_.code == "pl").get
  private val query   = CandidateQuery.Title("Lalka")
  // TMDB's answer to a title search, as recorded (test/resources/fixtures/tmdb/search_lalka_2026.json)
  private val search  = new String(Files.readAllBytes(Path.of("test/resources/fixtures/tmdb/search_lalka_2026.json")), java.nio.charset.StandardCharsets.UTF_8)
  private def scratch(): Path = Files.createTempDirectory("what-if-answers")

  private def modelLookups(source: WhatIfAnswers.Source): IdentityLookups =
    tools.ProdLookupInputs.lookups(source.docs, Nil, Nil, source.live, poland, clock)

  "a live what-if" should "ask TMDB a search the mirror lacks once, file it in the local store, and answer a later run from there" in {
    val dir    = scratch()
    val mirror = new InMemoryTmdbDocuments
    val wire   = tools.RoutingHttpFetch.getOnly(Seq("/search/movie" -> search))
    val first  = modelLookups(WhatIfAnswers.live(mirror, dir, wire, settings.TmdbApiKey("replay"), poland, clock)).candidates(query)
    first.isKnown shouldBe true
    first.toOption.get should not be empty
    wire.calls.count(_._2.contains("/search/movie")) shouldBe 1
    mirror.size(TmdbKind.Query) shouldBe 0
    // a later run: a fresh process over the same store, every request refused
    val again = modelLookups(WhatIfAnswers.live(mirror, dir, tools.RoutingHttpFetch.dead("tmdb"), settings.TmdbApiKey("replay"), poland, clock))
      .candidates(query)
    again shouldBe first
  }

  "an offline what-if" should "leave a search the mirror lacks unanswered, asking nothing" in {
    modelLookups(WhatIfAnswers.offline(new InMemoryTmdbDocuments, poland)).candidates(query) shouldBe Answer.Unknown
  }

  "a what-if's stage sources" should "ask the country's agreement families live, and none offline" in {
    val live = WhatIfAnswers.live(new InMemoryTmdbDocuments, scratch(), tools.RoutingHttpFetch.dead("sites"), settings.TmdbApiKey("replay"), poland, clock)
    live.stage.sources.keySet shouldBe tools.UnmatchedClusters.familiesOf(poland).toSet
    WhatIfAnswers.offline(new InMemoryTmdbDocuments, poland).stage.sources shouldBe empty
  }

  "the layered store" should "read the mirror first and write only to the local store" in {
    val mirror = new InMemoryTmdbDocuments
    val local  = new InMemoryTmdbDocuments
    mirror.put(TmdbKind.Query, Seq("q1" -> new BsonDocument("from", new BsonString("mirror"))))
    local.put(TmdbKind.Query, Seq("q1" -> new BsonDocument("from", new BsonString("local")), "q2" -> new BsonDocument("from", new BsonString("local"))))
    val layered = new Layered(mirror, local)
    layered.get(TmdbKind.Query, Seq("q1", "q2")).map { case (id, d) => id -> d.getString("from").getValue } shouldBe Map("q1" -> "mirror", "q2" -> "local")
    layered.put(TmdbKind.Query, Seq("q3" -> new BsonDocument()))
    mirror.size(TmdbKind.Query) shouldBe 1
    local.size(TmdbKind.Query) shouldBe 3
  }

  "the local store" should "read back what an earlier run filed, the later write of an id winning, and drop a torn last record" in {
    val dir = scratch()
    val one = new DiskTmdbDocuments(dir)
    one.put(TmdbKind.Family, Seq("imdb|title|lalka" -> new BsonDocument("n", new BsonString("1"))))
    one.put(TmdbKind.Family, Seq("imdb|title|lalka" -> new BsonDocument("n", new BsonString("2")), "rt|title|x" -> new BsonDocument()))
    Files.write(dir.resolve("identity_family_answers.log"), Array[Byte](0, 0, 0, 9, 'h'.toByte), java.nio.file.StandardOpenOption.APPEND)
    val two = new DiskTmdbDocuments(dir)
    two.size(TmdbKind.Family) shouldBe 2
    two.get(TmdbKind.Family, Seq("imdb|title|lalka"))("imdb|title|lalka").getString("n").getValue shouldBe "2"
  }

  "the memoised mirror" should "read each document once, a missing one too, and write through" in {
    val reads = new java.util.concurrent.atomic.AtomicInteger
    val inner = new InMemoryTmdbDocuments
    inner.put(TmdbKind.Family, Seq("a" -> new BsonDocument("n", new BsonString("1"))))
    val counted = new adapters.TmdbDocuments {
      def get(kind: TmdbKind, ids: Seq[String]): Map[String, BsonDocument] = { reads.incrementAndGet(); inner.get(kind, ids) }
      def put(kind: TmdbKind, docs: Seq[(String, BsonDocument)]): Unit = inner.put(kind, docs)
    }
    val memo = new WhatIfAnswers.Memoised(counted)
    memo.warm(TmdbKind.Family, Seq("a", "b"))
    memo.get(TmdbKind.Family, Seq("a", "b")).keySet shouldBe Set("a")
    memo.get(TmdbKind.Family, Seq("b")) shouldBe empty
    reads.get shouldBe 1
    memo.put(TmdbKind.Family, Seq("b" -> new BsonDocument()))
    memo.get(TmdbKind.Family, Seq("b")).keySet shouldBe Set("b")
    inner.size(TmdbKind.Family) shouldBe 2
    reads.get shouldBe 1
  }

  "a mirror read" should "be asked again when it times out, and the timeout thrown once it keeps timing out" in {
    val tries = new java.util.concurrent.atomic.AtomicInteger
    WhatIfAnswers.patient({ if (tries.incrementAndGet() < 2) throw new java.util.concurrent.TimeoutException("Future timed out after [30 seconds]"); "read" },
      times = 3) shouldBe "read"
    tries.set(0)
    a[java.util.concurrent.TimeoutException] should be thrownBy
      WhatIfAnswers.patient[String]({ tries.incrementAndGet(); throw new java.util.concurrent.TimeoutException("again") }, times = 2)
    tries.get shouldBe 2
  }

  "the throttle" should "halve a host's concurrency on a 429 and never ask it more at once than its limit" in {
    val inFlight = new java.util.concurrent.atomic.AtomicInteger
    val most     = new java.util.concurrent.atomic.AtomicInteger
    val refuse   = new java.util.concurrent.atomic.AtomicBoolean(true)
    val wire = new tools.HttpFetch {
      def get(url: String): String = {
        most.accumulateAndGet(inFlight.incrementAndGet(), math.max)
        try {
          Thread.sleep(20)
          if (refuse.getAndSet(false)) throw new tools.HttpStatusException(429, "GET", url, None)
          "{}"
        } finally { inFlight.decrementAndGet(); () }
      }
      def post(url: String, body: String, contentType: String): String = get(url)
    }
    val throttled = new Throttled(wire, start = 4)
    scala.util.Try(throttled.get("https://api.example.org/first"))
    most.set(0)
    val pool = java.util.concurrent.Executors.newFixedThreadPool(8)
    try (1 to 16).map(i => pool.submit(() => throttled.get(s"https://api.example.org/$i"))).foreach(_.get)
    finally pool.shutdownNow()
    most.get shouldBe 2
    throttled.report() should include ("17 live request(s)")
    throttled.report() should include ("halved 1 time(s)")
  }
}
