package tools

import services.movies.SingleCountryNormalizer

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
   * Every wiring in this spec both replays from a tree and RECORDS into it, and handed the
   * country's real one a `sbt testUnit` run would write `body for https://…` into the corpus
   * a convergence leg replays. Each wiring is handed a throwaway tree instead — a value, so
   * it stays LOCAL to that wiring: nothing in the JVM's properties moves, and suites here
   * run in parallel.
   */
  private var fixtureTree: String = scala.compiletime.uninitialized
  private val trees = scala.collection.mutable.ListBuffer.empty[String]

  /** Point the wirings built from here on at a tree of their own. Called once per test by
   *  [[beforeEach]], and again by any test that needs a SECOND wiring not to see the
   *  first one's recordings. Every tree it hands out is removed in [[afterEach]]. */
  private def useFreshTree(): String = {
    fixtureTree = s"archive-replay-spec-${java.util.UUID.randomUUID()}"
    trees += fixtureTree
    fixtureTree
  }

  private def rootOf(tree: String): java.nio.file.Path =
    java.nio.file.Paths.get(settings.FixtureRoot.RepositoryRelative.of(tree))

  override def beforeEach(): Unit = {
    trees.clear()
    useFreshTree()
    ()
  }

  override def afterEach(): Unit = {
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
   * One per wiring, so no test reads repositories another test wrote.
   */
  private final class FetchOnlyStorage extends tools.ConvergenceStorage {
    override val describe = "unit-spec doubles (enrichment fetch only)"
    override lazy val connection  = new services.MongoConnection(uri = None, dbName = settings.MongoDatabaseName("kinowo"), required = services.MongoRequirement.Optional)
    override lazy val screenings  = new services.movies.InMemoryScreeningsRepository
    override lazy val slots       = new services.movies.InMemorySlotsRepository
    override lazy val movies      = new services.movies.InMemoryMovieRepository(
      screenings = Some(screenings), slots = Some(slots), normalizer = SingleCountryNormalizer.titleNormalizer)
    override lazy val readModel: services.readmodel.ReadModelReader & services.readmodel.ReadModelWriter =
      new services.readmodel.InMemoryReadModelRepository()
    override lazy val staging     = new services.staging.InMemoryStagingRepository(normalizer = movies.normalizer)
    override lazy val archive     = new services.scrapes.InMemoryScrapeArchiveRepository
    override lazy val tasks       = new services.tasks.InMemoryTaskQueue
    override lazy val freshness   = new services.freshness.InMemoryFreshnessStore
    override lazy val chunkScrape = new services.tasks.InMemoryChunkScrapeStore()
    override lazy val omdbAttempt = new services.enrichment.InMemoryOmdbAttemptStore
    override def stagingFolder(movieRepository: services.movies.MovieRepository): services.staging.StagingFolder =
      new services.staging.InMemoryStagingFolder(staging, movieRepository, normalizer = movieRepository.normalizer)
  }

  private def wiringWith(cache: Option[EnrichmentCache], leaf: HttpFetch): ArchiveReplayWiring =
    new ArchiveReplayWiring(Country.Poland, new InMemoryScrapeArchiveRepository, cache, new FetchOnlyStorage, fixtureTree, settings.FixtureRoot.RepositoryRelative) {
      override protected def realHttpLeaf: HttpFetch = leaf
    }

  // An unconfigured run is the EMPTY case of a configured one, not a second mode. It used
  // to be a second mode — no directory meant a fetch that refused every call, which took
  // TMDB's key away with it — and the leg then ran to completion having enriched nothing.
  "the archive replay directory" should "be named after the country when nothing points it elsewhere" in {
    ArchiveReplayWiring.fixtureDirectory(Country.Poland, new settings.ProcessConfiguration(Env.of()))  shouldBe "enrichment-pl"
    ArchiveReplayWiring.fixtureDirectory(Country.Germany, new settings.ProcessConfiguration(Env.of())) shouldBe "enrichment-de"
  }

  it should "be whatever a run points it at" in {
    ArchiveReplayWiring.fixtureDirectory(Country.Poland, new settings.ProcessConfiguration(Env.of(ArchiveReplayWiring.FixturesVar -> "enrichment-scratch"))) shouldBe
      "enrichment-scratch"
  }

  "the hermetic switch" should "be on only when a run asks for it" in {
    ArchiveReplayWiring.hermeticIn(new settings.ProcessConfiguration(Env.of(ArchiveReplayWiring.HermeticVar -> "true"))) shouldBe true
    ArchiveReplayWiring.hermeticIn(new settings.ProcessConfiguration(Env.of(ArchiveReplayWiring.HermeticVar -> "false"))) shouldBe false
    ArchiveReplayWiring.hermeticIn(new settings.ProcessConfiguration(Env.of())) shouldBe false
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
    val wiring = new ArchiveReplayWiring(Country.Germany, new InMemoryScrapeArchiveRepository, None, new FetchOnlyStorage, fixtureTree, settings.FixtureRoot.RepositoryRelative) {
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
    wiring.movieRepository.upsert(services.movies.FilmId.legacy("Stop Making Sense", None, wiring.movieRepository.normalizer), "Stop Making Sense", None, models.MovieRecord())
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

  // ── Hermetic replay ────────────────────────────────────────────────────────────────────
  //
  // A verdict leg must not depend on the network: a UK boot made 345 live fills of which 63
  // failed, Cinemeta answered 504s and Metacritic timed out, and each of those was a flake
  // in a suite whose one claim is that the same inputs give the same outputs. Hermetic
  // replaces the WIRE — nothing above it — so the tests below build the hermetic wiring
  // WITHOUT overriding `realHttpLeaf`: the leaf under test is the one the wiring chooses.

  private def hermeticWiring(cache: Option[EnrichmentCache], missing: MissingFixtures): ArchiveReplayWiring =
    new ArchiveReplayWiring(Country.Poland, new InMemoryScrapeArchiveRepository, cache, new FetchOnlyStorage, fixtureTree, settings.FixtureRoot.RepositoryRelative, Some(missing))

  "a hermetic archive replay" should "refuse an unrecorded enrichment request and name its fixture" in {
    val missing = new MissingFixtures
    val wiring  = hermeticWiring(Some(new EnrichmentCache(new InMemoryEnrichmentCacheStore())), missing)
    val url     = "https://www.omdbapi.com/?t=Dune&type=movie&apikey=secret"

    an [Exception] should be thrownBy wiring.enrichmentFetch.get(url)

    missing.keys.map(_._1) shouldBe Seq(clients.tools.RecordingHttpFetch.fixtureKey(url, foldYear = false))
    withClue("the credential must not reach the report: ") { missing.report(fixtureTree) should not include "secret" }
    withClue("a refused request records nothing: ") { recordedFiles shouldBe 0L }
  }

  it should "refuse an unrecorded cinema detail page the same way" in {
    val missing = new MissingFixtures
    val wiring  = hermeticWiring(Some(new EnrichmentCache(new InMemoryEnrichmentCacheStore())), missing)

    an [Exception] should be thrownBy wiring.httoFetch.get("https://cinema.test/film/dune")

    missing.keys.map(_._1) shouldBe Seq("cinema.test/film/dune")
  }

  it should "answer what the recording holds without refusing anything" in {
    // Record through a RECORDING wiring on the same tree, exactly as the recorder would.
    val leaf = new CountingLeaf
    wiringWith(Some(new EnrichmentCache(new InMemoryEnrichmentCacheStore())), leaf)
      .enrichmentFetch.get("https://api.themoviedb.org/3/search/movie?query=dune")
    val missing = new MissingFixtures

    hermeticWiring(Some(new EnrichmentCache(new InMemoryEnrichmentCacheStore())), missing)
      .enrichmentFetch.get("https://api.themoviedb.org/3/search/movie?query=dune") shouldBe
      "body for https://api.themoviedb.org/3/search/movie?query=dune"
    missing.isEmpty shouldBe true
  }

  // The recording is only complete if it holds what FAILED too: measured on Poland's and
  // the UK's samples, every request a hermetic replay of the published tree could not
  // answer was one its recording run had seen fail (OMDb over quota, a Cinemeta 504, a
  // Cineworld 403) and — being transient — never written down.
  it should "replay a failure the recording remembered, without refusing it" in {
    val store = new InMemoryEnrichmentCacheStore()
    val throwing = new CountingLeaf {
      override def get(url: String): String = { super.get(url); throw new HttpStatusException(403, "GET", url, None) }
    }
    val recording = wiringWith(Some(new EnrichmentCache(store, persistSuccesses = false,
      transients = EnrichmentCache.Transients.Recorded)), throwing)
    an [Exception] should be thrownBy recording.httoFetch.get("https://www.cineworld.test/api/movies?ids=1")
    an [Exception] should be thrownBy recording.enrichmentFetch.get("https://www.omdbapi.com/?t=Dune")
    throwing.calls shouldBe 2

    val replayed = new EnrichmentCache(store, persistSuccesses = false, transients = EnrichmentCache.Transients.Replayed)
    replayed.preload()
    val missing = new MissingFixtures
    val hermetic = hermeticWiring(Some(replayed), missing)

    // Failing the way the recording saw them fail — the remembered 403, not a refusal.
    (the [Exception] thrownBy hermetic.httoFetch.get("https://www.cineworld.test/api/movies?ids=1"))
      .getMessage should include("HTTP 403")
    (the [Exception] thrownBy hermetic.enrichmentFetch.get("https://www.omdbapi.com/?t=Dune"))
      .getMessage should include("HTTP 403")
    missing.isEmpty shouldBe true
  }

  // A detail page that answered nothing used to be asked again on every call: the detail
  // chain had no verdict cache, so the recorder had nothing to write and the tree could
  // never say what the recording saw.
  "the archive replay wiring" should "remember a failed detail page for the rest of the run" in {
    val throwing = new CountingLeaf {
      override def get(url: String): String = { super.get(url); throw new HttpStatusException(404, "GET", url, None) }
    }
    val wiring = wiringWith(Some(new EnrichmentCache(new InMemoryEnrichmentCacheStore())), throwing)

    an [Exception] should be thrownBy wiring.httoFetch.get("https://cinema.test/film/gone")
    an [Exception] should be thrownBy wiring.httoFetch.get("https://cinema.test/film/gone")

    throwing.calls shouldBe 1
  }

  // ── Proxied detail routes ──────────────────────────────────────────────────────────────
  //
  // The recorder used to send every cinema detail fetch direct. Prod sends Cineworld (via
  // flicks), Odeon, Vue, biletyna and Multikino through the residential proxy first, because
  // direct is the Cloudflare block — so a recording run asked Cineworld's box-office API for
  // 136 UK films, was answered 403 every time (29,512 remembered 403s), and the tree never
  // held one Cineworld detail. These stand a fake shard in for Decodo; nothing reaches a network.

  /** A Cloudflare-blocked origin, as a GitHub runner's direct egress sees it. */
  private class BlockedLeaf extends CountingLeaf {
    override def get(url: String): String = { super.get(url); throw new HttpStatusException(403, "GET", url, None) }
  }

  private def proxiedWiring(cache: Option[EnrichmentCache], direct: HttpFetch, proxy: HttpFetch,
                            missing: Option[MissingFixtures] = None): ArchiveReplayWiring =
    new ArchiveReplayWiring(Country.UnitedKingdom, new InMemoryScrapeArchiveRepository, cache, new FetchOnlyStorage, fixtureTree,
                            settings.FixtureRoot.RepositoryRelative, missing) {
      override protected def realHttpLeaf: HttpFetch = missing.fold(direct)(_ => super.realHttpLeaf)
      override protected def residentialProxyShards: Option[IndexedSeq[HttpFetch]] = Some(IndexedSeq(proxy))
    }

  private val proxiedRoutes: Seq[(String, ArchiveReplayWiring => HttpFetch)] = Seq(
    "flicks (Cineworld)" -> (_.flicksFetch), "odeon" -> (_.odeonFetch), "vue" -> (_.vueFetch),
    "biletyna" -> (_.biletynaFetch), "multikino" -> (_.multikinoFetch))

  proxiedRoutes.foreach { case (route, fetchOf) =>
    "a recording archive replay" should s"send a $route detail miss through the residential proxy first, as production does" in {
      val direct = new BlockedLeaf
      val proxy  = new CountingLeaf
      val url    = "https://www.cineworld.test/api/gatsby-source-boxofficeapi/movies?ids=1"

      fetchOf(proxiedWiring(Some(new EnrichmentCache(new InMemoryEnrichmentCacheStore())), direct, proxy)).get(url) shouldBe
        s"body for $url"
      withClue("the miss must reach the proxy leg: ") { proxy.calls shouldBe 1 }
      withClue("which answered, so direct is never asked: ") { direct.calls shouldBe 0 }
      withClue("and what it answered is recorded for the hermetic replay: ") { recordedFiles should be > 0L }
    }
  }

  it should "keep a route production sends direct on the direct leg" in {
    val direct = new CountingLeaf
    val proxy  = new CountingLeaf

    proxiedWiring(Some(new EnrichmentCache(new InMemoryEnrichmentCacheStore())), direct, proxy)
      .httoFetch.get("https://cinema.test/film/dune")

    direct.calls shouldBe 1
    proxy.calls shouldBe 0
  }

  it should "egress a proxied route direct when no proxy is configured, as production's chain degrades" in {
    val direct = new CountingLeaf
    wiringWith(Some(new EnrichmentCache(new InMemoryEnrichmentCacheStore())), direct)
      .flicksFetch.get("https://www.cineworld.test/api/movies?ids=1")
    direct.calls shouldBe 1
  }

  // The 403s a previous recording remembered sit in the published verdict cache. A hermetic
  // replay must keep failing them the way that recording saw (until a new recording writes the
  // answer), but the next RECORDING must ask again — now through the proxy — or those films stay
  // detail-less for as long as the remembered entry lives.
  it should "re-ask a 403 the previous recording remembered, through the proxy" in {
    val store = new InMemoryEnrichmentCacheStore()
    val url   = "https://www.cineworld.test/api/gatsby-source-boxofficeapi/movies?ids=2"
    val previous = wiringWith(Some(new EnrichmentCache(store, persistSuccesses = false,
      transients = EnrichmentCache.Transients.Recorded)), new BlockedLeaf)
    an [Exception] should be thrownBy previous.flicksFetch.get(url)
    withClue("the previous recording wrote its 403 down: ") { store.loadAll() should not be empty }

    val next = new EnrichmentCache(store, persistSuccesses = false, transients = EnrichmentCache.Transients.Recorded)
    next.preload()
    val proxy = new CountingLeaf
    proxiedWiring(Some(next), new BlockedLeaf, proxy).flicksFetch.get(url) shouldBe s"body for $url"
    proxy.calls shouldBe 1
  }

  "a hermetic archive replay" should "never reach the proxy, even when one is configured" in {
    val missing = new MissingFixtures
    val proxy   = new CountingLeaf
    val wiring  = proxiedWiring(Some(new EnrichmentCache(new InMemoryEnrichmentCacheStore())), new CountingLeaf, proxy, Some(missing))

    an [Exception] should be thrownBy wiring.flicksFetch.get("https://www.cineworld.test/api/movies?ids=3")

    proxy.calls shouldBe 0
    missing.keys.map(_._1) should have size 1
  }
}
