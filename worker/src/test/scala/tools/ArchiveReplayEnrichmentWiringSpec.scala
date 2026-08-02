package tools

import models.Country
import org.scalatest.BeforeAndAfterEach
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.scrapes.InMemoryScrapeArchiveRepository

/**
 * That the convergence suite's wiring actually ROUTES enrichment through the
 * cache — the seam the cache exists for, as opposed to the cache's own behaviour
 * (which `CachingEnrichmentFetchSpec` covers).
 *
 * Worth its own spec because the failure mode is silent in both directions: a
 * wiring that quietly enriched from nowhere would leave every field `None` and the
 * convergence specs would still pass, having verified nothing new; a wiring that put
 * the cache UNDER the throttle would replay every hit through a rate limiter and turn
 * a warm run into a slow one.
 */
class ArchiveReplayEnrichmentWiringSpec extends AnyFlatSpec with Matchers with BeforeAndAfterEach {

  /**
   * A scratch tree per test.
   *
   * Every wiring in this spec both replays from a tree and RECORDS into it, and left to
   * its own devices it would pick the country's real one — so a `sbt testUnit` run would
   * write `body for https://…` into the corpus a convergence leg replays. Pointing the
   * spec's own knob at a throwaway name keeps that impossible, and keeps it LOCAL:
   * `KINOWO_FIXTURE_ROOT` would relocate every fixture consumer in the JVM, and suites
   * here run in parallel.
   */
  private var fixtureTree: String = scala.compiletime.uninitialized
  private val trees = scala.collection.mutable.ListBuffer.empty[String]

  /** Point the wirings built from here on at a tree of their own. Called once per test by
   *  [[beforeEach]], and again by any test that needs a SECOND wiring not to see the
   *  first one's recordings. Every tree it hands out is removed in [[afterEach]]. */
  private def useFreshTree(): String = {
    fixtureTree = s"archive-replay-spec-${java.util.UUID.randomUUID()}"
    trees += fixtureTree
    System.setProperty(ArchiveReplayWiring.FixturesVar, fixtureTree)
    fixtureTree
  }

  private def rootOf(tree: String): java.nio.file.Path =
    java.nio.file.Paths.get(clients.tools.FakeHttpFetch.rootFor(tree))

  override def beforeEach(): Unit = {
    trees.clear()
    useFreshTree()
    ()
  }

  override def afterEach(): Unit = {
    System.clearProperty(ArchiveReplayWiring.FixturesVar)
    trees.map(rootOf).filter(java.nio.file.Files.exists(_)).foreach { root =>
      java.nio.file.Files.walk(root).sorted(java.util.Comparator.reverseOrder())
        .forEach(path => java.nio.file.Files.deleteIfExists(path))
    }
  }

  private def recordedFiles: Long = {
    val root = rootOf(fixtureTree)
    if (!java.nio.file.Files.exists(root)) 0L
    else java.nio.file.Files.walk(root).filter(java.nio.file.Files.isRegularFile(_)).count()
  }

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

  // An unconfigured run is the EMPTY case of a configured one, not a second mode. It used
  // to be a second mode — no directory meant a fetch that refused every call, which took
  // TMDB's key away with it — and the leg then ran to completion having enriched nothing.
  "the archive replay directory" should "be named after the country when nothing points it elsewhere" in {
    System.clearProperty(ArchiveReplayWiring.FixturesVar)

    ArchiveReplayWiring.fixtureDirectory(Country.Poland)  shouldBe "enrichment-pl"
    ArchiveReplayWiring.fixtureDirectory(Country.Germany) shouldBe "enrichment-de"
  }

  it should "be whatever a run points it at" in {
    // `beforeEach` set it; that IS the override.
    ArchiveReplayWiring.fixtureDirectory(Country.Poland) shouldBe fixtureTree
  }

  // The scrape side is per-film DETAIL — 25 Polish cinema clients implement `DetailEnricher`
  // — and refusing it cost the suite its enrichment: rows reached TMDB yearless, fell to the
  // tier demanding an exact title match, and Poland resolved 36% against prod's 78%. The
  // LISTINGS still never fetch, but that is enforced by construction (`PreScrapedCinemaScraper`),
  // not by crippling the fetch.
  "the archive replay wiring" should "fetch and record cinema detail pages through the tree" in {
    val leaf = new CountingLeaf
    val wiring = wiringWith(Some(new EnrichmentCache(new InMemoryEnrichmentCacheStore())), leaf)

    wiring.httoFetch.get("https://cinema.test/film/dune") shouldBe "body for https://cinema.test/film/dune"
    leaf.calls shouldBe 1
    withClue("and be recorded, so the next run replays it: ") { recordedFiles should be > 0L }
  }

  it should "answer a repeated enrichment call from the tree instead of the wire" in {
    val leaf = new CountingLeaf
    val wiring = wiringWith(Some(new EnrichmentCache(new InMemoryEnrichmentCacheStore())), leaf)

    wiring.enrichmentFetch.get("https://api.themoviedb.org/3/search?query=dune") shouldBe
      "body for https://api.themoviedb.org/3/search?query=dune"
    wiring.enrichmentFetch.get("https://api.themoviedb.org/3/search?query=dune")

    leaf.calls shouldBe 1
  }

  // CI runs with an empty-or-partial tree and NO Mongo cache — the cache URI named a
  // tunnel the job never started, so it was removed. The tree is then the whole
  // determinism mechanism, and it only works if it can GROW: a miss has to reach live and
  // be recorded, or the same miss recurs on every run for ever. Left as it was, "no cache"
  // meant "offline behind the fixtures" and every unrecorded URL simply failed.
  it should "reach live and record it when the tree holds nothing yet and there is no cache" in {
    val leaf   = new CountingLeaf
    val wiring = wiringWith(None, leaf)

    wiring.enrichmentFetch.get("https://api.themoviedb.org/3/search?query=dune") shouldBe
      "body for https://api.themoviedb.org/3/search?query=dune"
    withClue("a fixture miss must reach the live leg: ") { leaf.calls shouldBe 1 }
    withClue("and be recorded, so the next run replays it: ") { recordedFiles should be > 0L }
  }

  // TMDB used to be gated on having a CACHE, so a leg running on the fixture tree alone
  // handed `TmdbClient` `apiKey = None` — and `search` is `authHeader.flatMap`, so every
  // title came back `None` without the fetch being touched: 892 films, 0 resolved, three
  // specs green in 55 seconds. The gate is gone (a sourceless wiring can't be built), but
  // `TestWiring` still pins a stub key and the DEFAULT language, so the override that
  // beats it has to stay — and the language is what proves this wiring's own is in force.
  it should "give TMDB the country's language even when nothing is configured" in {
    // Nothing configured is where the old gate did its damage: no cache and no fixtures
    // meant `apiKey = None`, and a keyless `TmdbClient` returns `None` from `search`
    // without ever reaching the fetch. Germany, so the locale discriminates — the keyless
    // branch took `TmdbClient.DefaultLanguage` (pl-PL), and so does `TestWiring`'s stub.
    System.clearProperty(ArchiveReplayWiring.FixturesVar)
    val wiring = new ArchiveReplayWiring(Country.Germany, new InMemoryScrapeArchiveRepository, None, FetchOnlyStorage) {
      override protected def realHttpLeaf: HttpFetch = new CountingLeaf
    }

    wiring.tmdbClient.language shouldBe Country.Germany.language
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
  //
  // Separate TREES on purpose, and that is also what makes this the test that the cache
  // is in the chain at all: on one tree the second wiring would replay the recording the
  // first one made and never consult the cache, so the leaf count would prove nothing.
  it should "share one cache's answers across separate wirings" in {
    val leaf  = new CountingLeaf
    val cache = new EnrichmentCache(new InMemoryEnrichmentCacheStore())

    wiringWith(Some(cache), leaf).enrichmentFetch.get("https://api.themoviedb.org/3/shared")
    useFreshTree()
    wiringWith(Some(cache), leaf).enrichmentFetch.get("https://api.themoviedb.org/3/shared")

    leaf.calls shouldBe 1
  }
}
