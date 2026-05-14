package services.enrichment

import clients.TmdbClient
import models.Enrichment
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.events.{EventBus, MovieAdded, TmdbResolved}
import tools.HttpFetch

import scala.collection.mutable

/**
 * Tests for `MetascoreRatings` — mirrors `ImdbRatingsSpec` but for the
 * Metacritic critic-aggregate score.
 */
class MetascoreRatingsSpec extends AnyFlatSpec with Matchers {

  // ── Scaffolding ─────────────────────────────────────────────────────────────

  private class FakeRepo(seed: Seq[(String, Option[Int], Enrichment)] = Seq.empty)
      extends EnrichmentRepo {
    private val store = mutable.LinkedHashMap.empty[(String, Option[Int]), Enrichment]
    val upserts = mutable.ListBuffer.empty[(String, Option[Int], Enrichment)]
    seed.foreach { case (t, y, e) => store.put((t, y), e) }
    override def enabled: Boolean = true
    override def findAll(): Seq[(String, Option[Int], Enrichment)] =
      store.iterator.map { case ((t, y), e) => (t, y, e) }.toSeq
    override def upsert(t: String, y: Option[Int], e: Enrichment): Unit = {
      store.put((t, y), e); upserts.append((t, y, e))
    }
    override def delete(t: String, y: Option[Int]): Unit = { store.remove((t, y)); () }
  }

  // Stub MC client that maps URL → JSON-LD HTML and lets the real parser run.
  private def mcStub(scores: Map[String, Option[Int]]): MetacriticClient = {
    new MetacriticClient(new HttpFetch {
      def get(url: String): String =
        scores.get(url) match {
          case Some(Some(s)) =>
            s"""<html><head><script type="application/ld+json">
               |{"@type":"Movie","aggregateRating":{"@type":"AggregateRating","ratingValue":$s,"bestRating":100,"worstRating":0,"reviewCount":10}}
               |</script></head><body></body></html>""".stripMargin
          case Some(None) =>
            // Page exists but no aggregated score yet — JSON-LD omits aggregateRating.
            """<html><head><script type="application/ld+json">{"@type":"Movie"}</script></head><body></body></html>"""
          case None => throw new RuntimeException(s"unstubbed URL: $url")
        }
    })
  }

  private def mkEnrichment(
    imdbId:        String,
    mcUrl:         Option[String] = None,
    metascore:     Option[Int]    = None
  ): Enrichment =
    Enrichment(
      imdbId        = Some(imdbId), imdbRating = None, metascore = metascore,
      originalTitle = None, tmdbId = Some(42),
      metacriticUrl = mcUrl
    )

  private val Url = "https://www.metacritic.com/movie/the-dark-knight"

  // ── refreshOneSync ──────────────────────────────────────────────────────────

  "refreshOneSync" should "scrape the score and write it back when it differs from the cached value" in {
    val repo  = new FakeRepo(Seq(
      ("The Dark Knight", Some(2008), mkEnrichment("tt0468569", mcUrl = Some(Url), metascore = Some(70)))
    ))
    val cache  = new EnrichmentCache(repo)
    val mc     = mcStub(Map(Url -> Some(85)))
    val rates  = new MetascoreRatings(cache, new TmdbClient(apiKey = None), mc)

    rates.refreshOneSync(cache.keyOf("The Dark Knight", Some(2008)))

    cache.get(cache.keyOf("The Dark Knight", Some(2008))).flatMap(_.metascore) shouldBe Some(85)
  }

  it should "not write back when the score is unchanged (idempotent)" in {
    val repo  = new FakeRepo(Seq(
      ("The Dark Knight", Some(2008), mkEnrichment("tt0468569", mcUrl = Some(Url), metascore = Some(85)))
    ))
    val cache = new EnrichmentCache(repo)
    repo.upserts.clear()
    val rates = new MetascoreRatings(cache, new TmdbClient(apiKey = None), mcStub(Map(Url -> Some(85))))

    rates.refreshOneSync(cache.keyOf("The Dark Knight", Some(2008)))

    repo.upserts shouldBe empty
  }

  it should "not change the cached score when MC has no aggregated score yet (None from parser)" in {
    val repo  = new FakeRepo(Seq(
      ("Indie Film", Some(2025), mkEnrichment("tt9", mcUrl = Some(Url), metascore = Some(70)))
    ))
    val cache = new EnrichmentCache(repo)
    val rates = new MetascoreRatings(cache, new TmdbClient(apiKey = None), mcStub(Map(Url -> None)))

    rates.refreshOneSync(cache.keyOf("Indie Film", Some(2025)))

    // Stays Some(70). We never clear an existing score just because MC's
    // current scrape didn't return one — that's almost always a transient
    // gap rather than MC actively removing a published Metascore.
    cache.get(cache.keyOf("Indie Film", Some(2025))).flatMap(_.metascore) shouldBe Some(70)
  }

  it should "swallow MC fetch failures (network blip, Cloudflare challenge) without throwing" in {
    val repo  = new FakeRepo(Seq(("X", None, mkEnrichment("tt9", mcUrl = Some(Url), metascore = Some(70)))))
    val cache = new EnrichmentCache(repo)
    val brokenMc = new MetacriticClient(new HttpFetch {
      def get(url: String): String = throw new RuntimeException("boom")
    })
    val rates = new MetascoreRatings(cache, new TmdbClient(apiKey = None), brokenMc)

    noException should be thrownBy rates.refreshOneSync(cache.keyOf("X", None))
    cache.get(cache.keyOf("X", None)).flatMap(_.metascore) shouldBe Some(70)
  }

  it should "be a no-op when the row has no metacriticUrl" in {
    val repo  = new FakeRepo(Seq(("X", None, mkEnrichment("tt9", mcUrl = None, metascore = None))))
    val cache = new EnrichmentCache(repo)
    // MC stub throws on any call — proving we never tried to fetch.
    val rates = new MetascoreRatings(cache, new TmdbClient(apiKey = None), new MetacriticClient(new HttpFetch {
      def get(url: String): String = throw new RuntimeException("should not be called")
    }))

    noException should be thrownBy rates.refreshOneSync(cache.keyOf("X", None))
  }

  it should "be a no-op when the cache has no entry for the key" in {
    val cache = new EnrichmentCache(new FakeRepo())
    val rates = new MetascoreRatings(cache, new TmdbClient(apiKey = None), new MetacriticClient(new HttpFetch {
      def get(url: String): String = throw new RuntimeException("should not be called")
    }))

    noException should be thrownBy rates.refreshOneSync(cache.keyOf("Missing", None))
  }

  // ── refreshAll ──────────────────────────────────────────────────────────────

  "refreshAll" should "walk every cached row with an MC URL and update each score that changed" in {
    val url2 = "https://www.metacritic.com/movie/inception"
    val url3 = "https://www.metacritic.com/movie/the-godfather"
    val repo = new FakeRepo(Seq(
      ("A", None, mkEnrichment("tt1", mcUrl = Some(Url),  metascore = Some(70))),
      ("B", None, mkEnrichment("tt2", mcUrl = Some(url2), metascore = Some(74))),
      ("C", None, mkEnrichment("tt3", mcUrl = Some(url3), metascore = Some(100))),
      ("D", None, mkEnrichment("tt4", mcUrl = None,       metascore = None))   // skipped: no URL
    ))
    val cache = new EnrichmentCache(repo)
    val rates = new MetascoreRatings(cache, new TmdbClient(apiKey = None), mcStub(Map(
      Url  -> Some(85),    // changed
      url2 -> Some(74),    // unchanged
      url3 -> Some(100)    // unchanged
    )))

    rates.refreshAll()

    cache.get(cache.keyOf("A", None)).flatMap(_.metascore) shouldBe Some(85)
    cache.get(cache.keyOf("B", None)).flatMap(_.metascore) shouldBe Some(74)
    cache.get(cache.keyOf("C", None)).flatMap(_.metascore) shouldBe Some(100)
    cache.get(cache.keyOf("D", None)).flatMap(_.metascore) shouldBe None
  }

  // ── Event listener ──────────────────────────────────────────────────────────

  "onTmdbResolved" should "trigger a metascore refresh for the resolved row when subscribed on the bus" in {
    val bus   = new EventBus()
    val repo  = new FakeRepo(Seq(
      ("Foo", Some(2024), mkEnrichment("tt1", mcUrl = Some(Url), metascore = None))
    ))
    val cache = new EnrichmentCache(repo)
    val rates = new MetascoreRatings(cache, new TmdbClient(apiKey = None), mcStub(Map(Url -> Some(85))))
    bus.subscribe(rates.onTmdbResolved)

    bus.publish(TmdbResolved("Foo", Some(2024), "tt1"))

    eventually(cache.get(cache.keyOf("Foo", Some(2024))).flatMap(_.metascore) shouldBe Some(85))
  }

  it should "ignore events of other types (PartialFunction.applyOrElse)" in {
    val bus   = new EventBus()
    val cache = new EnrichmentCache(new FakeRepo())
    val rates = new MetascoreRatings(cache, new TmdbClient(apiKey = None), new MetacriticClient(new HttpFetch {
      def get(url: String): String = throw new RuntimeException("should not be called")
    }))
    bus.subscribe(rates.onTmdbResolved)

    noException should be thrownBy bus.publish(MovieAdded("Anything", None))
  }

  private def eventually(check: => org.scalatest.Assertion): org.scalatest.Assertion = {
    val deadline = System.currentTimeMillis() + 2000
    var last: Throwable = null
    while (System.currentTimeMillis() < deadline) {
      try return check
      catch { case t: Throwable => last = t; Thread.sleep(20) }
    }
    throw last
  }
}
