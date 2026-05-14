package services.enrichment

import models.Enrichment
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.events.{EventBus, MovieAdded, TmdbResolved}
import tools.HttpFetch

import scala.collection.mutable

/**
 * Tests for `FilmwebRatings` — the extracted Filmweb stage. Mirrors the
 * other ratings specs (`ImdbRatingsSpec`, `RottenTomatoesRatingsSpec`).
 */
class FilmwebRatingsSpec extends AnyFlatSpec with Matchers {

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

  /** Stub fetch that routes by URL substring. Filmweb's API endpoints:
   *    /live/search?query=...  → search hits
   *    /film/{id}/info         → title + year
   *    /film/{id}/rating       → rate
   */
  private class StubFetch(routes: Map[String, String]) extends HttpFetch {
    override def get(url: String): String =
      routes.collectFirst { case (frag, body) if url.contains(frag) => body }
        .getOrElse(throw new RuntimeException(s"unstubbed URL: $url"))
    override def post(url: String, body: String, contentType: String): String = get(url)
  }

  private val deadFetch = new HttpFetch {
    override def get(url: String): String = throw new RuntimeException(s"unused: $url")
    override def post(url: String, body: String, contentType: String): String = get(url)
  }

  private def mkEnrichment(
    imdbId:        String,
    filmwebUrl:    Option[String] = None,
    filmwebRating: Option[Double] = None
  ): Enrichment =
    Enrichment(
      imdbId        = Some(imdbId), imdbRating = None, metascore = None,
      originalTitle = None, tmdbId = Some(42),
      filmwebUrl    = filmwebUrl,
      filmwebRating = filmwebRating
    )

  // ── refreshOneSync: existing URL → rating-only refresh ─────────────────────

  "refreshOneSync" should "fetch the rating via the stored URL's id and write it back when it changes" in {
    val url = "https://www.filmweb.pl/film/Mortal+Kombat+II-2026-10007434"
    val repo = new FakeRepo(Seq(
      ("Mortal Kombat II", Some(2026), mkEnrichment("tt1", filmwebUrl = Some(url), filmwebRating = Some(6.0)))
    ))
    val cache   = new EnrichmentCache(repo)
    val filmweb = new FilmwebClient(new StubFetch(Map(
      "/film/10007434/rating" -> """{"rate":6.72,"count":1000}"""
    )))
    val ratings = new FilmwebRatings(cache, filmweb)

    ratings.refreshOneSync(cache.keyOf("Mortal Kombat II", Some(2026)))

    cache.get(cache.keyOf("Mortal Kombat II", Some(2026))).flatMap(_.filmwebRating) shouldBe Some(6.72)
  }

  it should "skip search + info when the URL is present (cheap rating-only path)" in {
    // Verify by stubbing ONLY the rating endpoint; any other call would throw
    // "unstubbed URL".
    val url = "https://www.filmweb.pl/film/Title-9999"
    val repo = new FakeRepo(Seq(("X", None, mkEnrichment("tt1", filmwebUrl = Some(url)))))
    val cache = new EnrichmentCache(repo)
    val filmweb = new FilmwebClient(new StubFetch(Map(
      "/film/9999/rating" -> """{"rate":7.5,"count":1}"""
    )))
    val ratings = new FilmwebRatings(cache, filmweb)

    noException should be thrownBy ratings.refreshOneSync(cache.keyOf("X", None))
    cache.get(cache.keyOf("X", None)).flatMap(_.filmwebRating) shouldBe Some(7.5)
  }

  // ── refreshOneSync: missing URL → full lookup ──────────────────────────────

  "refreshOneSync (no stored URL)" should "fall through to filmweb.lookup, populating both URL and rating" in {
    val repo  = new FakeRepo(Seq(("Drama", Some(2024), mkEnrichment("tt1"))))
    val cache = new EnrichmentCache(repo)
    val filmweb = new FilmwebClient(new StubFetch(Map(
      "/live/search"          -> """{"searchHits":[{"id":555,"type":"film","matchedTitle":"Drama"}]}""",
      "/film/555/info"        -> """{"title":"Drama","year":2024}""",
      "/film/555/rating"      -> """{"rate":7.2,"count":500}"""
    )))
    val ratings = new FilmwebRatings(cache, filmweb)

    ratings.refreshOneSync(cache.keyOf("Drama", Some(2024)))

    val after = cache.get(cache.keyOf("Drama", Some(2024))).get
    after.filmwebUrl    should not be empty
    after.filmwebUrl.get should endWith ("-555")
    after.filmwebRating shouldBe Some(7.2)
  }

  // ── Failure handling ───────────────────────────────────────────────────────

  it should "swallow Filmweb fetch failures without throwing" in {
    val url  = "https://www.filmweb.pl/film/Foo-7"
    val repo = new FakeRepo(Seq(("Foo", None, mkEnrichment("tt1", filmwebUrl = Some(url), filmwebRating = Some(6.0)))))
    val cache = new EnrichmentCache(repo)
    val brokenFilmweb = new FilmwebClient(new HttpFetch {
      def get(url: String): String = throw new RuntimeException("boom")
    })
    val ratings = new FilmwebRatings(cache, brokenFilmweb)

    noException should be thrownBy ratings.refreshOneSync(cache.keyOf("Foo", None))
    cache.get(cache.keyOf("Foo", None)).flatMap(_.filmwebRating) shouldBe Some(6.0)
  }

  it should "be a no-op when the cache has no entry for the key" in {
    val cache = new EnrichmentCache(new FakeRepo())
    val ratings = new FilmwebRatings(cache, new FilmwebClient(deadFetch))
    noException should be thrownBy ratings.refreshOneSync(cache.keyOf("Missing", None))
  }

  it should "not write back when the rating is unchanged (idempotent)" in {
    val url = "https://www.filmweb.pl/film/Foo-12"
    val repo = new FakeRepo(Seq(("Foo", None, mkEnrichment("tt1", filmwebUrl = Some(url), filmwebRating = Some(7.5)))))
    val cache = new EnrichmentCache(repo)
    repo.upserts.clear()
    val filmweb = new FilmwebClient(new StubFetch(Map("/film/12/rating" -> """{"rate":7.5,"count":1}""")))
    val ratings = new FilmwebRatings(cache, filmweb)

    ratings.refreshOneSync(cache.keyOf("Foo", None))

    repo.upserts shouldBe empty
  }

  // ── refreshAll ──────────────────────────────────────────────────────────────

  "refreshAll" should "do the cheap rating-only path for rows with a URL and full lookup for rows without" in {
    val urlA = "https://www.filmweb.pl/film/A-1"
    val urlB = "https://www.filmweb.pl/film/B-2"
    val repo = new FakeRepo(Seq(
      ("A", None, mkEnrichment("tt1", filmwebUrl = Some(urlA), filmwebRating = Some(5.0))),  // changed
      ("B", None, mkEnrichment("tt2", filmwebUrl = Some(urlB), filmwebRating = Some(6.0))),  // unchanged
      ("C", None, mkEnrichment("tt3"))                                                       // full lookup
    ))
    val cache = new EnrichmentCache(repo)
    val filmweb = new FilmwebClient(new StubFetch(Map(
      "/film/1/rating"   -> """{"rate":7.4,"count":1}""",
      "/film/2/rating"   -> """{"rate":6.0,"count":1}""",
      "/live/search"     -> """{"searchHits":[{"id":33,"type":"film","matchedTitle":"C"}]}""",
      "/film/33/info"    -> """{"title":"C","year":2024}""",
      "/film/33/rating"  -> """{"rate":8.1,"count":1}"""
    )))
    val ratings = new FilmwebRatings(cache, filmweb)

    ratings.refreshAll()

    cache.get(cache.keyOf("A", None)).flatMap(_.filmwebRating) shouldBe Some(7.4)
    cache.get(cache.keyOf("B", None)).flatMap(_.filmwebRating) shouldBe Some(6.0)
    val c = cache.get(cache.keyOf("C", None)).get
    c.filmwebUrl.get  should endWith ("-33")
    c.filmwebRating shouldBe Some(8.1)
  }

  // ── Event listener ──────────────────────────────────────────────────────────

  "onTmdbResolved" should "trigger a per-row refresh when subscribed on the bus" in {
    val bus   = new EventBus()
    val url   = "https://www.filmweb.pl/film/Foo-99"
    val repo  = new FakeRepo(Seq(("Foo", Some(2024), mkEnrichment("tt1", filmwebUrl = Some(url)))))
    val cache = new EnrichmentCache(repo)
    val filmweb = new FilmwebClient(new StubFetch(Map("/film/99/rating" -> """{"rate":7.4,"count":1}""")))
    val ratings = new FilmwebRatings(cache, filmweb)
    bus.subscribe(ratings.onTmdbResolved)

    bus.publish(TmdbResolved("Foo", Some(2024), "tt1"))

    eventually(cache.get(cache.keyOf("Foo", Some(2024))).flatMap(_.filmwebRating) shouldBe Some(7.4))
  }

  it should "ignore events of other types (PartialFunction.applyOrElse)" in {
    val bus   = new EventBus()
    val cache = new EnrichmentCache(new FakeRepo())
    val ratings = new FilmwebRatings(cache, new FilmwebClient(new HttpFetch {
      def get(url: String): String = throw new RuntimeException("should not be called")
    }))
    bus.subscribe(ratings.onTmdbResolved)

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
