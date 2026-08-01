package tools

import models.Country
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.scrapes.InMemoryScrapeArchiveRepository

/**
 * That the convergence suite's wiring actually ROUTES enrichment through the
 * cache — the seam the cache exists for, as opposed to the cache's own behaviour
 * (which `CachingEnrichmentFetchSpec` covers).
 *
 * Worth its own spec because the failure mode is silent in both directions: a
 * wiring that quietly kept `OfflineHttpFetch` would leave every enrichment field
 * `None` and the convergence specs would still pass, having verified nothing new;
 * a wiring that put the cache UNDER the throttle would replay every hit through a
 * rate limiter and turn a warm run into a slow one.
 */
class ArchiveReplayEnrichmentWiringSpec extends AnyFlatSpec with Matchers {

  /** Stands in for the real network at the very bottom of the wiring's enrich-phase
   *  chain, so what a test counts is genuine wire attempts. Records the URLs too, so a
   *  test can ask WHICH service was reached rather than only how many times. */
  private class CountingLeaf extends HttpFetch {
    private val seen = new java.util.concurrent.ConcurrentLinkedQueue[String]()
    def calls: Int = seen.size
    def urls: Seq[String] = seen.toArray(Array.empty[String]).toSeq
    override def get(url: String): String = { seen.add(url); s"body for $url" }
    override def post(url: String, body: String, contentType: String): String = { seen.add(url); "posted" }
  }


  /**
   * Test doubles assembled HERE, in the spec that wants them.
   *
   * This spec is about the enrichment FETCH — which chain answers, and whether TMDB gets
   * a key — so a container would be pure cost. What it deliberately is NOT is a shipped
   * `ConvergenceStorage.inMemory`: that existed as a default the convergence suite could
   * pick up silently, and it did, which is how the order-independence passes ran against
   * a map for months while appearing to cover the pipeline. A unit spec naming its own
   * doubles is explicit; a default that anything can inherit is not.
   */
  private object FetchOnlyStorage extends tools.ConvergenceStorage {
    override val describe = "unit-spec doubles (enrichment fetch only)"
    override lazy val connection  = new services.MongoConnection(uri = None, dbName = "kinowo", required = false)
    override lazy val screenings  = new services.movies.InMemoryScreeningsRepository
    override lazy val slots       = new services.movies.InMemorySlotsRepository
    override lazy val movies      = new services.movies.InMemoryMovieRepository(
      screenings = Some(screenings), slots = Some(slots))
    override lazy val readModel: services.readmodel.ReadModelReader & services.readmodel.ReadModelWriter =
      new services.readmodel.InMemoryReadModelRepository()
    override lazy val staging     = new services.staging.InMemoryStagingRepository()
    override lazy val archive     = new services.scrapes.InMemoryScrapeArchiveRepository
    override lazy val tasks       = new services.tasks.InMemoryTaskQueue
    override lazy val freshness   = new services.freshness.InMemoryFreshnessStore
    override lazy val chunkScrape = new services.tasks.InMemoryChunkScrapeStore()
    override lazy val omdbAttempt = new services.enrichment.InMemoryOmdbAttemptStore
    override def stagingFolder(movieRepository: services.movies.MovieRepository): services.staging.StagingFolder =
      new services.staging.InMemoryStagingFolder(staging, movieRepository)
  }

  private def wiringWith(cache: Option[EnrichmentCache], leaf: HttpFetch): ArchiveReplayWiring =
    new ArchiveReplayWiring(Country.Poland, new InMemoryScrapeArchiveRepository, cache, FetchOnlyStorage) {
      override protected def realHttpLeaf: HttpFetch = leaf
    }

  "the archive replay wiring" should "refuse enrichment HTTP when it has no cache" in {
    val leaf = new CountingLeaf
    val wiring = wiringWith(None, leaf)

    a [UnsupportedOperationException] should be thrownBy wiring.enrichmentFetch.get("https://api.themoviedb.org/3/x")
    leaf.calls shouldBe 0
  }

  it should "keep the SCRAPE side offline even when enrichment is cached" in {
    val leaf = new CountingLeaf
    val wiring = wiringWith(Some(new EnrichmentCache(new InMemoryEnrichmentCacheStore())), leaf)

    // The corpus comes from the archive; a scraper reaching for HTTP is a bug.
    a [UnsupportedOperationException] should be thrownBy wiring.httoFetch.get("https://cinema.test/listing")
    leaf.calls shouldBe 0
  }

  it should "answer a repeated enrichment call from the cache instead of the wire" in {
    val leaf = new CountingLeaf
    val wiring = wiringWith(Some(new EnrichmentCache(new InMemoryEnrichmentCacheStore())), leaf)

    wiring.enrichmentFetch.get("https://api.themoviedb.org/3/search?query=dune") shouldBe
      "body for https://api.themoviedb.org/3/search?query=dune"
    wiring.enrichmentFetch.get("https://api.themoviedb.org/3/search?query=dune")

    leaf.calls shouldBe 1
  }

  // The TMDB client is what every downstream rating source hangs off; if it were
  // left on the offline fetch nothing would enrich however good the cache was.
  it should "point the TMDB client at the cached fetch" in {
    val leaf = new CountingLeaf
    val wiring = wiringWith(Some(new EnrichmentCache(new InMemoryEnrichmentCacheStore())), leaf)

    wiring.cachedEnrichmentFetch should not be empty
    wiring.enrichmentFetch shouldBe wiring.cachedEnrichmentFetch.get
  }

  // CI runs with the fixture tree and NO Mongo cache — the cache URI named a tunnel
  // the job never started, so it was removed. The tree is then the whole determinism
  // mechanism, and it only works if it can GROW: a miss has to reach live and be
  // recorded, or the same miss recurs on every run for ever. Left as it was, "no
  // cache" meant "offline behind the fixtures" and every unrecorded URL simply
  // failed.
  it should "reach live and record it when fixtures are configured but no cache is" in {
    val leaf      = new CountingLeaf
    val directory = s"enrichment-wiring-${ProcessHandle.current().pid()}"
    val root      = java.nio.file.Paths.get(clients.tools.FakeHttpFetch.rootFor(directory))
    System.setProperty("KINOWO_CONVERGENCE_ENRICHMENT_FIXTURES", directory)
    try {
      val wiring = wiringWith(None, leaf)

      wiring.enrichmentFetch.get("https://api.themoviedb.org/3/search?query=dune") shouldBe
        "body for https://api.themoviedb.org/3/search?query=dune"
      withClue("a fixture miss must reach the live leg: ") { leaf.calls shouldBe 1 }

      withClue("and be recorded, so the next run replays it: ") {
        java.nio.file.Files.exists(root) shouldBe true
        java.nio.file.Files.walk(root).filter(java.nio.file.Files.isRegularFile(_)).count() should be > 0L
      }
    } finally {
      System.clearProperty("KINOWO_CONVERGENCE_ENRICHMENT_FIXTURES")
      if (java.nio.file.Files.exists(root))
        java.nio.file.Files.walk(root).sorted(java.util.Comparator.reverseOrder())
          .forEach(path => java.nio.file.Files.deleteIfExists(path))
    }
  }

  // The gate that decides whether TMDB gets a key at all. It asked about the CACHE,
  // so a leg running on the fixture tree alone handed `TmdbClient` `apiKey = None` —
  // and `search` is `authHeader.flatMap`, so every title came back `None` without the
  // fetch being touched. 892 films, 0 resolved, three specs green in 55 seconds.
  it should "give TMDB a real key when fixtures are its source and no cache is" in {
    val directory = s"enrichment-gate-${ProcessHandle.current().pid()}"
    val root      = java.nio.file.Paths.get(clients.tools.FakeHttpFetch.rootFor(directory))
    System.setProperty("KINOWO_CONVERGENCE_ENRICHMENT_FIXTURES", directory)
    try {
      // Germany, so the language actually discriminates the two branches: the enabled
      // one passes the country's locale, the short-circuiting one takes the default.
      val wiring = new ArchiveReplayWiring(Country.Germany, new InMemoryScrapeArchiveRepository, None, FetchOnlyStorage) {
        override protected def realHttpLeaf: HttpFetch = new CountingLeaf
      }

      wiring.enrichmentAvailable shouldBe true
      wiring.tmdbClient.language shouldBe Country.Germany.language
    } finally {
      System.clearProperty("KINOWO_CONVERGENCE_ENRICHMENT_FIXTURES")
      if (java.nio.file.Files.exists(root))
        java.nio.file.Files.walk(root).sorted(java.util.Comparator.reverseOrder())
          .forEach(path => java.nio.file.Files.deleteIfExists(path))
    }
  }

  it should "know it has no enrichment source when it has neither cache nor fixtures" in {
    wiringWith(None, new CountingLeaf).enrichmentAvailable shouldBe false
  }

  it should "know it has an enrichment source when it has a cache" in {
    wiringWith(Some(new EnrichmentCache(new InMemoryEnrichmentCacheStore())), new CountingLeaf)
      .enrichmentAvailable shouldBe true
  }

  /**
   * The id-recovery ladder must survive a drain, because the boot calls one BEFORE the
   * event that needs it is published.
   *
   * `ImdbIdMissing` is how a film TMDB could not identify still gets an id — IMDb's
   * suggestion endpoint, then OMDb / Cinemeta / Wikidata — and the id then resolves
   * TMDB in reverse through `/find`. It is the route prod takes for the whole bare-title
   * long tail: `movies` rows for "Stop Making Sense" and "Złoto", listed by a single
   * cinema under nothing but a title, carry an IMDB slot and a tmdbId that a year-less
   * search could never have produced.
   *
   * In the replay harness the event fires from the STAGING FOLD
   * (`announceResolvedNewMovie`), and `bootCorpus` runs `drainServices()` before
   * `drainStaging()`. `drainServices` was `stop()` — a permanent
   * `ExecutorService.shutdown()` — so every `ImdbIdMissing` published from the first
   * fold onwards was submitted to a dead pool and silently dropped. Poland's leg logged
   * 0 event-driven recoveries against prod's populated IMDB slots, and 42 films prod
   * resolves came out `tmdbNoMatch`.
   */
  /** The control for the test below: with the pools untouched the ladder does reach
   *  IMDb, so a failure there is about the DRAIN and not about the row, the cache or
   *  the event. */
  it should "recover a missing IMDb id off the bus" in {
    val leaf   = new CountingLeaf
    val wiring = wiringWith(Some(new EnrichmentCache(new InMemoryEnrichmentCacheStore())), leaf)
    seedUnidentifiedFilm(wiring)

    wiring.eventBus.publish(
      services.events.ImdbIdMissing("Stop Making Sense", None, "Stop Making Sense"))
    wiring.drainServices()

    withClue(s"URLs fetched: ${leaf.urls.mkString(", ")}: ") {
      leaf.urls.exists(_.contains("imdb")) shouldBe true
    }
  }

  it should "still attempt IMDb-id recovery after the enrichment pools have been drained once" in {
    val leaf   = new CountingLeaf
    val wiring = wiringWith(Some(new EnrichmentCache(new InMemoryEnrichmentCacheStore())), leaf)
    seedUnidentifiedFilm(wiring)

    // What the boot does before the staging fold publishes anything.
    wiring.drainServices()

    wiring.eventBus.publish(
      services.events.ImdbIdMissing("Stop Making Sense", None, "Stop Making Sense"))
    wiring.drainServices()

    withClue(s"the ladder never reached IMDb; the only URLs fetched were ${leaf.urls.mkString(", ")}: ") {
      leaf.urls.exists(_.contains("imdb")) shouldBe true
    }
  }

  /** A row the resolver will act on: known to the cache, with no imdbId to keep. */
  private def seedUnidentifiedFilm(wiring: ArchiveReplayWiring): Unit = {
    wiring.movieRepository.upsert("Stop Making Sense", None, models.MovieRecord())
    wiring.movieCache.rehydrate()
    ()
  }

  // Three concurrent replays each build their own wiring; sharing the cache is what
  // stops them disagreeing about what the live service said.
  it should "share one cache's answers across separate wirings" in {
    val leaf  = new CountingLeaf
    val cache = new EnrichmentCache(new InMemoryEnrichmentCacheStore())

    wiringWith(Some(cache), leaf).enrichmentFetch.get("https://api.themoviedb.org/3/shared")
    wiringWith(Some(cache), leaf).enrichmentFetch.get("https://api.themoviedb.org/3/shared")

    leaf.calls shouldBe 1
  }
}
