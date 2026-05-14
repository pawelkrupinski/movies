package services.enrichment

import clients.TmdbClient
import models.Enrichment
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.events.{DomainEvent, EventBus, MovieAdded, TmdbResolved}
import tools.HttpFetch

import scala.collection.mutable

/**
 * Tests for the two-stage enrichment pipeline:
 *
 *   1. **TMDB stage**: resolves `(title, year)` → tmdbId + imdbId + originalTitle +
 *      Filmweb + MC + RT URLs. Publishes `TmdbResolved` on success.
 *   2. **IMDb stage**: fetches the IMDb rating for one cache row. Triggered by
 *      `TmdbResolved` via the EventBus, or by the hourly refresh loop.
 *
 * Plus the daily TMDB-retry tick — clears the negative cache and re-schedules
 * the TMDB stage for any cached row with `tmdbId.isEmpty`.
 *
 * `reEnrichSync` is the test surface: it runs both stages on the calling
 * thread, so we don't need to wait on a worker pool.
 */
class EnrichmentPipelineStagesSpec extends AnyFlatSpec with Matchers {

  // ── Stubs ──────────────────────────────────────────────────────────────────

  private class StubFetch(routes: Map[String, String]) extends HttpFetch {
    override def get(url: String): String =
      routes.collectFirst { case (frag, body) if url.contains(frag) => body }
        .getOrElse(throw new RuntimeException(s"unstubbed URL: $url"))
    override def post(url: String, body: String, contentType: String): String = get(url)
  }

  private class FakeRepo(seed: Seq[(String, Option[Int], Enrichment)] = Seq.empty)
      extends EnrichmentRepo {
    private val store = mutable.LinkedHashMap.empty[(String, Option[Int]), Enrichment]
    seed.foreach { case (t, y, e) => store.put((t, y), e) }
    override def enabled: Boolean = true
    override def findAll(): Seq[(String, Option[Int], Enrichment)] =
      store.iterator.map { case ((t, y), e) => (t, y, e) }.toSeq
    override def upsert(t: String, y: Option[Int], e: Enrichment): Unit = { store.put((t, y), e); () }
    override def delete(t: String, y: Option[Int]): Unit = { store.remove((t, y)); () }
  }

  private val deadFetch = new HttpFetch {
    override def get(url: String): String = throw new RuntimeException(s"unused: $url")
    override def post(url: String, body: String, contentType: String): String = get(url)
  }
  private def deadFilmweb()    = new FilmwebClient(http = deadFetch)
  private def deadMetacritic() = new MetacriticClient(http = deadFetch)
  private def deadRt()         = new RottenTomatoesClient(http = deadFetch)

  // TMDB returns "Mortal Kombat II" → tt17490712 / id 931285.
  private val Mk2Search =
    """{"results":[
      |  {"id":931285, "title":"Mortal Kombat II", "original_title":"Mortal Kombat II",
      |   "release_date":"2026-05-06", "popularity":223.66}
      |]}""".stripMargin
  private val Mk2ExternalIds = """{"id":931285, "imdb_id":"tt17490712"}"""
  private val Mk2ImdbGraphql =
    """{"data":{"title":{"ratingsSummary":{"aggregateRating":7.0,"voteCount":17000}}}}"""

  private def tmdbStub() = new TmdbClient(
    http = new StubFetch(Map("/search/movie" -> Mk2Search, "/external_ids" -> Mk2ExternalIds)),
    apiKey = Some("stub")
  )

  // ── TmdbResolved is published by the async TMDB stage (event-driven path) ──

  "the async TMDB stage" should "publish TmdbResolved after a successful resolution" in {
    val bus  = new EventBus()
    val seen = mutable.ListBuffer.empty[DomainEvent]
    bus.subscribe { case e: TmdbResolved => seen.append(e) }

    val cache       = new EnrichmentCache(new FakeRepo())
    val imdb        = new ImdbClient(http = new StubFetch(Map("caching.graphql.imdb.com" -> Mk2ImdbGraphql)))
    val imdbRatings = new ImdbRatings(cache, imdb)
    val svc   = new EnrichmentService(
      cache, bus, imdbRatings, tmdbStub(), deadFilmweb(),
      deadMetacritic(), deadRt()
    )

    // The async path goes through `reEnrich` → worker pool → runTmdbStage,
    // which publishes the event on success.
    svc.reEnrich("Mortal Kombat II", Some(2026))

    eventually(seen.toSeq shouldBe Seq(TmdbResolved("Mortal Kombat II", Some(2026), "tt17490712")))
  }

  it should "NOT publish TmdbResolved when TMDB has no match" in {
    val bus  = new EventBus()
    val seen = mutable.ListBuffer.empty[DomainEvent]
    bus.subscribe { case e: TmdbResolved => seen.append(e) }

    val emptyTmdb = new TmdbClient(
      http = new StubFetch(Map("/search/movie" -> """{"results":[]}""")),
      apiKey = Some("stub")
    )
    val cache       = new EnrichmentCache(new FakeRepo())
    val imdbRatings = new ImdbRatings(cache, new ImdbClient(http = deadFetch))
    val svc   = new EnrichmentService(
      cache, bus, imdbRatings, emptyTmdb, deadFilmweb(),
      deadMetacritic(), deadRt()
    )

    svc.reEnrich("Unknown Title", None)

    // Wait a beat — no event should fire because there's no hit.
    Thread.sleep(200)
    seen shouldBe empty
    // And the row should be negative-cached.
    cache.isNegative(cache.keyOf("Unknown Title", None)) shouldBe true
  }

  it should "NOT publish TmdbResolved when reEnrichSync runs (sync path is event-free to avoid double IMDb fetch)" in {
    // Sync callers (scripts via reEnrichSync) do both stages on the calling
    // thread; firing events would also trigger the bus listener, doubling
    // the IMDb fetch. The sync path skips events deliberately.
    val bus  = new EventBus()
    val seen = mutable.ListBuffer.empty[DomainEvent]
    bus.subscribe { case e: TmdbResolved => seen.append(e) }

    val cache       = new EnrichmentCache(new FakeRepo())
    val imdb        = new ImdbClient(http = new StubFetch(Map("caching.graphql.imdb.com" -> Mk2ImdbGraphql)))
    val imdbRatings = new ImdbRatings(cache, imdb)
    val svc   = new EnrichmentService(
      cache, bus, imdbRatings, tmdbStub(), deadFilmweb(),
      deadMetacritic(), deadRt()
    )

    svc.reEnrichSync("Mortal Kombat II", Some(2026))
    seen shouldBe empty
  }

  // ── reEnrichSync chains both stages on the calling thread ─────────────────

  "reEnrichSync" should "run TMDB stage then IMDb stage and fill the row's rating" in {
    val bus      = new EventBus()
    val imdbHttp = new StubFetch(Map("caching.graphql.imdb.com" -> Mk2ImdbGraphql))
    val cache    = new EnrichmentCache(new FakeRepo())
    val ratings  = new ImdbRatings(cache, new ImdbClient(http = imdbHttp))
    val svc      = new EnrichmentService(
      cache, bus, ratings, tmdbStub(), deadFilmweb(), deadMetacritic(), deadRt()
    )

    val e = svc.reEnrichSync("Mortal Kombat II", Some(2026))

    e.flatMap(_.imdbRating) shouldBe Some(7.0)
    e.flatMap(_.imdbId)     shouldBe Some("tt17490712")
    e.flatMap(_.tmdbId)     shouldBe Some(931285)
  }

  it should "preserve an existing imdbRating when the IMDb GraphQL fetch fails" in {
    // Seed: row already has a rating. Re-enrich runs TMDB stage (succeeds)
    // and IMDb stage (network blip → no update). The cached rating must be
    // preserved (TMDB stage carries it forward).
    val seed = Enrichment(
      imdbId = Some("tt17490712"), imdbRating = Some(7.2),
      metascore = None, originalTitle = None, tmdbId = Some(931285)
    )
    val cache = new EnrichmentCache(new FakeRepo(Seq(
      ("Mortal Kombat II", Some(2026), seed)
    )))
    val brokenImdb = new ImdbClient(http = new HttpFetch {
      override def get(url: String): String = throw new RuntimeException("network blip")
      override def post(url: String, body: String, contentType: String): String = get(url)
    })
    val svc = new EnrichmentService(
      cache, new EventBus(), new ImdbRatings(cache, brokenImdb),
      tmdbStub(), deadFilmweb(), deadMetacritic(), deadRt()
    )

    val e = svc.reEnrichSync("Mortal Kombat II", Some(2026))

    e.flatMap(_.imdbRating) shouldBe Some(7.2)   // unchanged
  }

  // ── Daily TMDB retry ──────────────────────────────────────────────────────

  "retryUnresolvedTmdb" should "clear the negative cache so previously-failed lookups get another shot" in {
    val cache = new EnrichmentCache(new FakeRepo())
    val key   = cache.keyOf("Some Film", Some(2026))
    cache.markMissing(key)
    cache.isNegative(key) shouldBe true

    val svc = new EnrichmentService(
      cache, new EventBus(), new ImdbRatings(cache, new ImdbClient(http = deadFetch)),
      new TmdbClient(http = new StubFetch(Map("/search/movie" -> """{"results":[]}""")), apiKey = Some("stub")),
      deadFilmweb(), deadMetacritic(), deadRt()
    )

    svc.retryUnresolvedTmdb()
    cache.isNegative(key) shouldBe false
  }

  it should "also re-run the TMDB stage for rows missing MC or RT URLs (bypasses the 'already-resolved' short-circuit)" in {
    // Regression: the hourly IMDb refresh used to also re-probe MC/RT URLs.
    // After splitting into stages, rows with tmdbId set but no MC/RT URL got
    // stuck (scheduleTmdbStage skips them, hourly loop doesn't touch them).
    // The daily retry must pick them up too.
    val incomplete = Enrichment(
      imdbId            = Some("tt17490712"),
      imdbRating        = Some(7.0),
      metascore         = None,
      originalTitle     = Some("Mortal Kombat II"),
      tmdbId            = Some(931285),
      metacriticUrl     = None,         // ← missing
      rottenTomatoesUrl = None          // ← missing
    )
    val repo  = new FakeRepo(Seq(("Mortal Kombat II", Some(2026), incomplete)))
    val cache = new EnrichmentCache(repo)
    val bus   = new EventBus()
    val resolved = mutable.ListBuffer.empty[DomainEvent]
    bus.subscribe { case e: TmdbResolved => resolved.append(e) }

    val imdb = new ImdbClient(http = new StubFetch(Map("caching.graphql.imdb.com" -> Mk2ImdbGraphql)))
    val svc  = new EnrichmentService(
      cache, bus, new ImdbRatings(cache, imdb),
      tmdbStub(), deadFilmweb(), deadMetacritic(), deadRt()
    )

    svc.retryUnresolvedTmdb()

    // TMDB stage runs, re-publishes the event, the (still empty) MC/RT URL
    // probes run via the deadMetacritic/deadRt stubs and stay None — but the
    // important thing is the row got REVISITED.
    eventually(resolved.size shouldBe 1)
  }

  it should "schedule the TMDB stage for cached rows whose tmdbId is empty (legacy data)" in {
    // Seed a row that has imdbId but no tmdbId (legacy from before TMDB was
    // always set). The daily retry should re-resolve it.
    val seed = Enrichment(
      imdbId = Some("tt-legacy"), imdbRating = None,
      metascore = None, originalTitle = None, tmdbId = None
    )
    val repo  = new FakeRepo(Seq(("Mortal Kombat II", Some(2026), seed)))
    val cache = new EnrichmentCache(repo)
    val bus   = new EventBus()
    val seenResolved = mutable.ListBuffer.empty[DomainEvent]
    bus.subscribe { case e: TmdbResolved => seenResolved.append(e) }

    val imdb = new ImdbClient(http = new StubFetch(Map("caching.graphql.imdb.com" -> Mk2ImdbGraphql)))
    val svc  = new EnrichmentService(
      cache, bus, new ImdbRatings(cache, imdb),
      tmdbStub(), deadFilmweb(), deadMetacritic(), deadRt()
    )

    svc.retryUnresolvedTmdb()

    // The retry dispatches onto the worker pool — give it a moment.
    eventually(seenResolved.size shouldBe 1)
    seenResolved.head shouldBe TmdbResolved("Mortal Kombat II", Some(2026), "tt17490712")

    // The cache row now carries the resolved tmdbId.
    val key = cache.keyOf("Mortal Kombat II", Some(2026))
    cache.get(key).flatMap(_.tmdbId) shouldBe Some(931285)
  }

  // ── MovieAdded triggers TMDB stage; TMDB skip-when-cached short-circuit ───

  "onMovieAdded" should "schedule TMDB stage when the row isn't cached" in {
    val cache    = new EnrichmentCache(new FakeRepo())
    val bus      = new EventBus()
    val resolved = mutable.ListBuffer.empty[DomainEvent]
    bus.subscribe { case e: TmdbResolved => resolved.append(e) }

    val imdb = new ImdbClient(http = new StubFetch(Map("caching.graphql.imdb.com" -> Mk2ImdbGraphql)))
    val svc  = new EnrichmentService(
      cache, bus, new ImdbRatings(cache, imdb),
      tmdbStub(), deadFilmweb(), deadMetacritic(), deadRt()
    )
    bus.subscribe(svc.onMovieAdded)

    bus.publish(MovieAdded("Mortal Kombat II", Some(2026)))

    eventually(resolved.size shouldBe 1)
    resolved.head shouldBe TmdbResolved("Mortal Kombat II", Some(2026), "tt17490712")
  }

  it should "skip rows that already have a tmdbId (no redundant TMDB call)" in {
    val seed  = Enrichment(
      imdbId = Some("tt17490712"), imdbRating = Some(7.0),
      metascore = None, originalTitle = Some("Mortal Kombat II"),
      tmdbId = Some(931285)
    )
    val cache = new EnrichmentCache(new FakeRepo(Seq(("Mortal Kombat II", Some(2026), seed))))
    val bus   = new EventBus()
    val resolved = mutable.ListBuffer.empty[DomainEvent]
    bus.subscribe { case e: TmdbResolved => resolved.append(e) }

    // dead TMDB / dead Filmweb / dead IMDb — assert no call lands.
    val svc = new EnrichmentService(
      cache, bus, new ImdbRatings(cache, new ImdbClient(http = deadFetch)),
      new TmdbClient(http = deadFetch, apiKey = Some("stub")),
      deadFilmweb(), deadMetacritic(), deadRt()
    )
    bus.subscribe(svc.onMovieAdded)

    bus.publish(MovieAdded("Mortal Kombat II", Some(2026)))

    // Give the worker a beat — but no event should fire.
    Thread.sleep(100)
    resolved shouldBe empty
  }

  // ── onTmdbResolved triggers IMDb stage ─────────────────────────────────────

  "onTmdbResolved" should "fetch IMDb rating for the resolved row" in {
    // Seed a row that already has TMDB data but no IMDb rating yet — simulate
    // the state right after the TMDB stage writes.
    val seed = Enrichment(
      imdbId = Some("tt17490712"), imdbRating = None,
      metascore = None, originalTitle = Some("Mortal Kombat II"),
      tmdbId = Some(931285)
    )
    val cache = new EnrichmentCache(new FakeRepo(Seq(("Mortal Kombat II", Some(2026), seed))))
    val bus   = new EventBus()

    // This test exercises the IMDb stage listener — wire it directly without
    // EnrichmentService, since the TMDB stage doesn't fire here.
    val imdb        = new ImdbClient(http = new StubFetch(Map("caching.graphql.imdb.com" -> Mk2ImdbGraphql)))
    val imdbRatings = new ImdbRatings(cache, imdb)
    bus.subscribe(imdbRatings.onTmdbResolved)

    bus.publish(TmdbResolved("Mortal Kombat II", Some(2026), "tt17490712"))

    val key = cache.keyOf("Mortal Kombat II", Some(2026))
    eventually(cache.get(key).flatMap(_.imdbRating) shouldBe Some(7.0))
  }

  // ── Helper: poll until the assertion holds, or fail after a short timeout.
  private def eventually(check: => org.scalatest.Assertion): org.scalatest.Assertion = {
    val deadline = System.currentTimeMillis() + 2000
    var lastErr: Option[Throwable] = None
    while (System.currentTimeMillis() < deadline) {
      try return check
      catch { case t: Throwable => lastErr = Some(t) }
      Thread.sleep(20)
    }
    throw lastErr.getOrElse(new RuntimeException("eventually timed out"))
  }
}
