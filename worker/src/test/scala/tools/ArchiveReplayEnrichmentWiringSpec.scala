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
  /** What a test started that can still fetch — and so record into its tree — after the test ends: the IMDb-id
   *  resolver retries a failed lookup a minute or more later, on a scheduler of its own. Stopped before the trees go,
   *  or a long `worker/test` run found the retry's recording re-creating a deleted tree in the fixture root. */
  private val stoppedAtEnd = scala.collection.mutable.ListBuffer.empty[services.Stoppable]

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
    stoppedAtEnd.foreach(_.stop())
    stoppedAtEnd.clear()
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
  private class FetchOnlyStorage extends tools.ConvergenceStorage {
    override val describe = "unit-spec doubles (enrichment fetch only)"
    override lazy val connection  = new services.MongoConnection(uri = None, dbName = settings.MongoDatabaseName("kinowo"), required = services.MongoRequirement.Optional)
    override lazy val screenings  = new services.movies.InMemoryScreeningsRepository
    override lazy val slots       = new services.movies.InMemorySlotsRepository
    override lazy val movies      = new services.movies.InMemoryMovieRepository(
      screenings = Some(screenings), slots = Some(slots), normalizer = SingleCountryNormalizer.titleNormalizer)
    override lazy val readModel: services.readmodel.ReadModelReader & services.readmodel.ReadModelWriter =
      new services.readmodel.InMemoryReadModelRepository()
    override lazy val archive     = new services.scrapes.InMemoryScrapeArchiveRepository
    override lazy val tasks       = new services.tasks.InMemoryTaskQueue
    override lazy val freshness   = new services.freshness.InMemoryFreshnessStore
    override lazy val chunkScrape = new services.tasks.InMemoryChunkScrapeStore()
    override lazy val omdbAttempt = new services.enrichment.InMemoryOmdbAttemptStore
  }

  private def wiringWith(cache: Option[EnrichmentCache], leaf: HttpFetch): ArchiveReplayWiring =
    new ArchiveReplayWiring(Country.Poland, new InMemoryScrapeArchiveRepository, cache, new FetchOnlyStorage, fixtureTree, settings.FixtureRoot.RepositoryRelative) {
      override protected def realHttpLeaf: HttpFetch = leaf
    }

  /** Production's queue sits behind a dedup cache (`TaskQueueWiring.taskDedupCache`): a key it
   *  queued and has not completed is answered `Duplicate` from memory. The replay wiring went
   *  straight to the store, paying a write round trip per repeat that production never makes —
   *  ~45% of a UK replay's drain, the reaper re-enqueueing every venue still owing a
   *  film's detail each time one venue's lands (2026-10-03). */
  // A RECORDING's requests go out over real time, but its pacers read the harness's frozen clock: no slot
  // ever came round, so the k-th request to a paced host waited k slots. The first recording to fetch
  // Flicks' film pages sat in that wait at 0% CPU until its 120- and 315-minute ceilings (run 37562532213),
  // pinned nothing, and every hermetic leg kept replaying a pair recorded before those pages were asked.
  "a recording's paced host" should "be paced one slot per request, not one more slot per request asked before" in {
    val leaf  = new CountingLeaf
    // The wall, stood in for: it moves only by the waits the pacers ask for.
    val wall  = new MutableClock(TestWiring.FixedInstant.plusSeconds(3600))
    val waits = new java.util.concurrent.ConcurrentLinkedQueue[Long]()
    // Positional: a named argument to an anonymous subclass's constructor is bound before `super`
    // initialises, which this compiler turns into a VerifyError once the body captures `leaf`.
    val wiring = new ArchiveReplayWiring(Country.UnitedStates, new InMemoryScrapeArchiveRepository, None, new FetchOnlyStorage,
      fixtureTree, settings.FixtureRoot.RepositoryRelative, None, Env.of("KINOWO_FLICKS_US_PACE_MS" -> "40"), None,
      new JsonBodies, None, wall) {
      override protected def realHttpLeaf: HttpFetch = leaf
      override protected def pacingSleep: Long => Unit = ms => { waits.add(ms); wall.advance(java.time.Duration.ofMillis(ms)) }
    }
    (1 to 10).foreach(i => wiring.httpFetch.get(s"https://www.flicks.us/movie/paced-$i/"))
    leaf.calls shouldBe 10
    withClue("a frozen clock asks 40, 80, 120 … ms: ") { waits.toArray.toSeq.distinct shouldBe Seq(40L) }
  }

  "the archive replay queue" should "answer a repeat enqueue from production's dedup cache, not the store" in {
    import services.tasks.{EnqueueResult, TaskType}
    val reachedStore = new java.util.concurrent.atomic.AtomicInteger(0)
    val storage = new FetchOnlyStorage {
      override lazy val tasks = new services.tasks.InMemoryTaskQueue {
        override def enqueue(taskType: TaskType, dedupKey: String, payload: Map[String, String], submittedAt: java.time.Instant,
                             notBefore: Option[java.time.Instant], claimAhead: scala.concurrent.duration.FiniteDuration): EnqueueResult = {
          reachedStore.incrementAndGet()
          super.enqueue(taskType, dedupKey, payload, submittedAt, notBefore, claimAhead)
        }
      }
    }
    val queue = new ArchiveReplayWiring(Country.Poland, new InMemoryScrapeArchiveRepository, None, storage, fixtureTree,
      settings.FixtureRoot.RepositoryRelative).taskQueue

    queue.enqueue(TaskType.EnrichDetails, "detail|film|venue") shouldBe EnqueueResult.Added
    queue.enqueue(TaskType.EnrichDetails, "detail|film|venue") shouldBe EnqueueResult.Duplicate
    withClue("the repeat is answered from memory: ")(reachedStore.get shouldBe 1)

    val task = queue.claim("replay", scala.concurrent.duration.Duration(1, "minute")).getOrElse(fail("nothing to claim"))
    queue.complete(task.id, "replay")
    withClue("and completing the task lets the key be queued again, through the store: ") {
      queue.enqueue(TaskType.EnrichDetails, "detail|film|venue") shouldBe EnqueueResult.Added
      reachedStore.get shouldBe 2
    }
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

    wiring.httpFetch.get("https://cinema.test/film/dune") shouldBe "body for https://cinema.test/film/dune"
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
   * In the replay harness the event fires when a film is announced
   * (`announceResolvedNewMovie`), after the boot has run `drainServices()` once.
   * `drainServices` was `stop()` — a permanent `ExecutorService.shutdown()` — so every
   * `ImdbIdMissing` published from then on was submitted to a dead pool and silently dropped. Poland's leg logged
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

    // What the boot does before any film is announced.
    wiring.drainServices()

    wiring.eventBus.publish(
      services.events.ImdbIdMissing("Stop Making Sense", None, "Stop Making Sense"))
    wiring.drainServices()

    withClue(s"the ladder never reached IMDb; the only URLs fetched were ${leaf.urls.mkString(", ")}: ") {
      leaf.urls.exists(_.contains("imdb")) shouldBe true
    }
  }

  /** A row the resolver will act on: known to the cache, a TMDB film TMDB gave no imdbId. */
  private def seedUnidentifiedFilm(wiring: ArchiveReplayWiring): Unit = {
    stoppedAtEnd += wiring.imdbIdResolver
    wiring.movieRepository.upsert(services.movies.FilmId.legacy("Stop Making Sense", None, wiring.movieRepository.normalizer), "Stop Making Sense", None,
      models.MovieRecord(tmdbId = Some(24128)))
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

    an [Exception] should be thrownBy wiring.httpFetch.get("https://cinema.test/film/dune")

    missing.keys.map(_._1) shouldBe Seq("cinema.test/film/dune")
  }

  // A page the tree lacks is asked again on every listing that names it: one Flicks detail page,
  // 36,569 times in a US boot (run 37546591704). Each ask was PACED on its way down to the leaf that
  // refuses it — 200 ms a slot on Flicks — 251 s of a boot spent sleeping before requests that can
  // never be sent. The pacing still decides; it no longer sleeps where nothing goes out.
  it should "refuse an unrecorded request to a paced host without waiting out its pace" in {
    val missing = new MissingFixtures
    val wiring  = new ArchiveReplayWiring(Country.UnitedKingdom, new InMemoryScrapeArchiveRepository,
      Some(new EnrichmentCache(new InMemoryEnrichmentCacheStore())), new FetchOnlyStorage, fixtureTree,
      settings.FixtureRoot.RepositoryRelative, Some(missing))
    val started = System.nanoTime()
    (1 to 4).foreach(i => an [Exception] should be thrownBy wiring.httpFetch.get(s"https://www.flicks.co.uk/movie/unrecorded-$i/"))
    val elapsedMs = (System.nanoTime() - started) / 1000000
    withClue(s"four refusals on a 200 ms-paced host took ${elapsedMs} ms: ") { elapsedMs should be < 300L }
    missing.size should be >= 1
  }

  // A refusal is not the host failing: nothing was sent. Counted as one, the fourth refusal opened the
  // host's breaker and every later page of it was answered "circuit open" above the leaf, so a US leg
  // named 12 of the ~2,300 Flicks and Drafthouse pages its tree lacked (run 37597662228).
  it should "name every unrecorded page of a host, never letting the refusals open its breaker" in {
    val missing = new MissingFixtures
    val wiring  = hermeticWiring(Some(new EnrichmentCache(new InMemoryEnrichmentCacheStore())), missing)
    val pages   = (1 to 12).map(i => s"https://www.flicks.us/movie/unrecorded-$i/")
    pages.foreach { page =>
      val refused = the [Exception] thrownBy wiring.httpFetch.get(page)
      refused.getMessage should not include "circuit open"
    }
    missing.size shouldBe pages.size
  }

  // Wikidata is paced across the fleet (500 ms a request, 2 s horizon). On a hermetic leg's frozen clock no slot
  // ever came round, so from the fifth request on the pacer answered "circuit open" above the leaf, and a Polish leg
  // named none of the ~60 Wikidata lookups its tree lacked (run 37656742608). A request that is never sent paces nothing.
  it should "name every unrecorded request to a fleet-paced host, never pacing what it does not send" in {
    val missing = new MissingFixtures
    val wiring  = hermeticWiring(Some(new EnrichmentCache(new InMemoryEnrichmentCacheStore())), missing)
    val asks    = (1 to 12).map(i => s"https://www.wikidata.org/wiki/Special:EntityData/Q$i.json")
    asks.foreach { url =>
      val refused = the [Exception] thrownBy wiring.enrichmentFetch.get(url)
      refused.getMessage should not include "circuit open"
    }
    missing.size shouldBe asks.size
  }

  // A recording remembers a failure that says nothing about its URL — a circuit it opened on itself, a 503 — and a hermetic
  // leg replays it, so the request never reaches the leaf and is never named: a Polish leg's ~60 Wikidata lookups were
  // answered "circuit open" from the recording for ever (run 37661888315). Such a request is a gap the fill can close, and
  // so is a 403 — the origin refusing CI's address, which the fill asks again through the residential proxy. A 404 is an
  // answer. (Remembered under the BYTES request, as the recorder fetches every response.)
  it should "name a request its recording remembers only a failure for, and never one with an answer" in {
    val missing = new MissingFixtures
    val cache   = new EnrichmentCache(new InMemoryEnrichmentCacheStore(), transients = EnrichmentCache.Transients.Replayed)
    val wiring  = hermeticWiring(Some(cache), missing)
    val passing = "https://www.wikidata.org/wiki/Special:EntityData/Q1.json"
    val refused = "https://www.wikidata.org/wiki/Special:EntityData/Q2.json"
    val gone    = "https://www.wikidata.org/wiki/Special:EntityData/Q3.json"
    cache.remember(CachingEnrichmentFetch.keyOf("BYTES", passing), CachedResponse.Failed(None, "GET", "tools.CircuitOpenException: circuit open"))
    cache.remember(CachingEnrichmentFetch.keyOf("BYTES", refused), CachedResponse.Failed(Some(403), "GET", "HTTP 403"))
    cache.remember(CachingEnrichmentFetch.keyOf("BYTES", gone), CachedResponse.Failed(Some(404), "GET", "HTTP 404"))
    Seq(passing, refused, gone).foreach(url => an [Exception] should be thrownBy wiring.enrichmentFetch.get(url))

    val file = java.nio.file.Files.createTempDirectory("remembered").resolve("enrichment-pl.refetch.tsv")
    missing.writeRefetches(file)
    java.nio.file.Files.readAllLines(file).toArray(Array.empty[String]).toSeq.flatMap(MissingFixtures.Refetch.parse).map(_._2) shouldBe
      Seq(MissingFixtures.Refetch("BYTES", passing), MissingFixtures.Refetch("BYTES", refused))
  }

  // A claimant is a thread of its own: an error its handler or the queue throws ended that thread alone, the join
  // returned as if the queue were drained, and the phase read as quiet with the task still leased — on the harness's
  // frozen clock, for ever. The drain fails with it instead.
  "a harness queue drain" should "fail with what a claimant threw, not read as drained" in {
    val wiring = hermeticWiring(None, new MissingFixtures)
    wiring.taskQueue.enqueue(services.tasks.TaskType.EnrichDetails, "probe-throws", Map.empty, submittedAt = wiring.clock.instant())
    val thrown = the [Throwable] thrownBy wiring.drainQueue("probe")(_ => throw new StackOverflowError("handler blew its stack"))
    thrown.getMessage should include ("handler blew its stack")
  }

  // The detail drain worked one task at a time on one thread: a few thousand detail tasks of serial
  // round trips on every US projection, ~23 s each at under one core (run 37563035752).
  it should "work its tasks on the background budget's claimants side by side" in {
    val wiring  = hermeticWiring(None, new MissingFixtures)
    (1 to 12).foreach(i => wiring.taskQueue.enqueue(services.tasks.TaskType.EnrichDetails, s"probe-$i", Map.empty,
      submittedAt = wiring.clock.instant()))
    // Opens only once two handlers are in flight at once: one claimant at a time never opens it.
    val together = new java.util.concurrent.CountDownLatch(2)
    val met      = new java.util.concurrent.atomic.AtomicBoolean(false)
    val handled  = new java.util.concurrent.atomic.AtomicInteger(0)
    wiring.drainClaimants should be > 1
    wiring.drainQueue("probe") { _ =>
      together.countDown()
      if (together.await(SpecTimeouts.Io.toMillis, java.util.concurrent.TimeUnit.MILLISECONDS)) met.set(true)
      handled.incrementAndGet(); ()
    }
    handled.get shouldBe 12
    withClue("no two tasks were ever handled at once: ") { met.get shouldBe true }
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
    an [Exception] should be thrownBy recording.httpFetch.get("https://www.cineworld.test/api/movies?ids=1")
    an [Exception] should be thrownBy recording.enrichmentFetch.get("https://www.omdbapi.com/?t=Dune")
    throwing.calls shouldBe 2

    val replayed = new EnrichmentCache(store, persistSuccesses = false, transients = EnrichmentCache.Transients.Replayed)
    replayed.preload()
    val missing = new MissingFixtures
    val hermetic = hermeticWiring(Some(replayed), missing)

    // Failing the way the recording saw them fail — the remembered 403, not a refusal.
    (the [Exception] thrownBy hermetic.httpFetch.get("https://www.cineworld.test/api/movies?ids=1"))
      .getMessage should include("HTTP 403")
    (the [Exception] thrownBy hermetic.enrichmentFetch.get("https://www.omdbapi.com/?t=Dune"))
      .getMessage should include("HTTP 403")
    // ...and named as gaps for the fill, which asks a refused request again through the residential proxy.
    missing.size shouldBe 2
  }

  // A detail page that answered nothing used to be asked again on every call: the detail
  // chain had no verdict cache, so the recorder had nothing to write and the tree could
  // never say what the recording saw.
  "the archive replay wiring" should "remember a failed detail page for the rest of the run" in {
    val throwing = new CountingLeaf {
      override def get(url: String): String = { super.get(url); throw new HttpStatusException(404, "GET", url, None) }
    }
    val wiring = wiringWith(Some(new EnrichmentCache(new InMemoryEnrichmentCacheStore())), throwing)

    an [Exception] should be thrownBy wiring.httpFetch.get("https://cinema.test/film/gone")
    an [Exception] should be thrownBy wiring.httpFetch.get("https://cinema.test/film/gone")

    throwing.calls shouldBe 1
  }

  // ── Paid egress ────────────────────────────────────────────────────────────────────────
  //
  // Zyte is billed per request. A test wiring is
  // handed the process's Env (for MONGODB_URI and TMDB's key), and that Env is CI's secrets or
  // a developer's `.env.local` — so a ZYTE_API_KEY in it built a live, Zyte-FIRST leg into the
  // wiring's Multikino poster route and armed the Odeon harvester, and proxy credentials in it
  // built Decodo legs, in a run that is meant to answer from fixtures.

  /** A wiring whose environment carries a Zyte key (plus `extra`), counting every time a route
   *  builds the client the Zyte API is called through — which a route does only for a Zyte leg. */
  private final class PaidKeyedWiring(direct: HttpFetch, extra: Seq[(String, String)] = Nil)
      extends ArchiveReplayWiring(Country.Poland, new InMemoryScrapeArchiveRepository, None, new FetchOnlyStorage, fixtureTree,
        settings.FixtureRoot.RepositoryRelative, environment = Env.of((("ZYTE_API_KEY" -> "paid-key") +: extra)*)) {
    val zyteClientsBuilt = new java.util.concurrent.atomic.AtomicInteger
    override protected def realHttpLeaf: HttpFetch = direct
    override lazy val zyteHttpClient: java.net.http.HttpClient = {
      zyteClientsBuilt.incrementAndGet()
      throw new IllegalStateException("a test wiring reached for Zyte")
    }
    def proxyShardsBuilt: Option[IndexedSeq[HttpFetch]] = residentialProxyShards
  }

  private val paidRoutes: Seq[(String, ArchiveReplayWiring => HttpFetch)] = Seq(
    "ck105 (zyteFetch)" -> (_.zyteFetch), "biletyna" -> (_.biletynaFetch), "multikino" -> (_.multikinoFetch),
    "multikino posters" -> (_.multikinoPosterFetch), "flicks (Cineworld)" -> (_.flicksFetch), "odeon" -> (_.odeonFetch),
    "vue" -> (_.vueFetch))

  paidRoutes.foreach { case (route, fetchOf) =>
    "a test wiring handed ZYTE_API_KEY" should s"build no Zyte leg into the $route route" in {
      val direct = new CountingLeaf
      val wiring = new PaidKeyedWiring(direct)
      val url    = "https://bilety.ck105.koszalin.test/repertuar"

      scala.util.Try(fetchOf(wiring).get(url))

      withClue("the Zyte client must never be built: ") { wiring.zyteClientsBuilt.get shouldBe 0 }
      withClue("the route answers from its free leg: ") { direct.calls shouldBe 1 }
    }
  }

  it should "mint no Odeon token, which only Zyte's browser fetch can harvest" in {
    new PaidKeyedWiring(new CountingLeaf).odeonAuthHarvester.token() shouldBe None
  }

  /** The production egress shape — a residential proxy AND a Zyte key — over a proxy that fails
   *  every call, with the Zyte API stood in for by a client that counts what is sent through it.
   *  (The client itself is built regardless: the scraper catalogue holds ck105's Zyte route.) */
  private final class ProxiedAndZyteKeyedWiring(direct: HttpFetch,
                                                proxied: java.util.concurrent.ConcurrentLinkedQueue[String] = new java.util.concurrent.ConcurrentLinkedQueue[String])
      extends ArchiveReplayWiring(Country.Poland, new InMemoryScrapeArchiveRepository, None, new FetchOnlyStorage, fixtureTree,
        settings.FixtureRoot.RepositoryRelative) {
    override protected def realHttpLeaf: HttpFetch = direct
    override protected def residentialProxyShards: Option[IndexedSeq[HttpFetch]] =
      Some(IndexedSeq(new GetOnlyHttpFetch {
        override def get(url: String): String = { proxied.add(url); throw new java.io.IOException("proxy: Tunnel failed, got: 503") }
      }))
    override protected def zyteApiKey: Option[settings.ZyteApiKey] = Some(settings.ZyteApiKey("paid-key"))
    override lazy val zyteHttpClient: clients.zyte.RefusingHttpClient = new clients.zyte.RefusingHttpClient
  }

  // Zyte is no longer the residential proxy's fallback (dropped 2026-10-05): a proxied route
  // whose proxy fails goes straight to its direct leg, even with a Zyte key configured.
  paidRoutes.filterNot(_._1.startsWith("ck105")).foreach { case (route, fetchOf) =>
    "a proxied route whose proxy fails" should s"fall to direct with no Zyte leg behind the proxy ($route)" in {
      val direct = new CountingLeaf
      val wiring = new ProxiedAndZyteKeyedWiring(direct)

      scala.util.Try(fetchOf(wiring).get("https://biletyna.test/venue"))

      withClue("nothing may be sent to Zyte: ") { wiring.zyteHttpClient.sends.get shouldBe 0 }
      withClue("the route answers from its direct leg: ") { direct.calls shouldBe 1 }
    }
  }

  // The positive control: the one route still on Zyte does reach it, so a zero above is real.
  "ck105's route" should "still ask Zyte first when a key is configured" in {
    val direct = new CountingLeaf
    val wiring = new ProxiedAndZyteKeyedWiring(direct)
    scala.util.Try(wiring.zyteFetch.get("https://bilety.ck105.koszalin.test/repertuar"))
    wiring.zyteHttpClient.sends.get shouldBe 1
    direct.calls shouldBe 1
  }

  // ck105 times out the worker's IP for its posters as for its pages: a poster fetched direct never
  // answers, so the agreement's poster question for Kino Kryterium's take waited for ever and the
  // model's wrong take ("Ktoś całkiem obcy" → the 2007 Perfect Stranger) was never read again.
  "ck105's posters" should "go through the venue's Zyte route, as its scrapes do" in {
    val direct = new CountingLeaf
    val wiring = new ProxiedAndZyteKeyedWiring(direct)
    val directPosters = new java.util.concurrent.atomic.AtomicInteger
    val download = services.sharecards.PosterDownload.routed(
      new services.sharecards.PosterDownload {
        def fetch(url: String): Either[String, java.nio.file.Path] = { directPosters.incrementAndGet(); Left("direct") }
      }, wiring.posterEgressRoutes)

    download.fetch(s"${services.cinemas.CinemaScraperCatalog.KinoKryteriumUrl}/MSI/ImageData.ashx?id=2721&mode=thumb")

    withClue("the poster is asked of Zyte first: ") { wiring.zyteHttpClient.sends.get shouldBe 1 }
    withClue("never of the plain poster download: ") { directPosters.get shouldBe 0 }
  }

  // prod PL 2026-10-06: biletyna.pl refuses the worker's IP its posters (403) as it does its pages, and every one of its
  // 226 venue posters was fetched direct and filed unreadable — the poster evidence for Binti, Fritzi and the rest gone.
  // Its scrapes go through the residential proxy; so must its posters, read off the catalog's own routes.
  "biletyna's posters" should "go through the residential proxy its scrapes use, as every host the catalog routes" in {
    val proxied = new java.util.concurrent.ConcurrentLinkedQueue[String]
    val wiring  = new ProxiedAndZyteKeyedWiring(new CountingLeaf, proxied)
    val directPosters = new java.util.concurrent.atomic.AtomicInteger
    val download = services.sharecards.PosterDownload.routed(
      new services.sharecards.PosterDownload {
        def fetch(url: String): Either[String, java.nio.file.Path] = { directPosters.incrementAndGet(); Left("direct") }
      }, wiring.posterEgressRoutes)

    download.fetch("https://biletyna.pl/file/get/id/394113")

    withClue("the poster is asked of the residential proxy first: ") { proxied.toArray.toSeq shouldBe Seq("https://biletyna.pl/file/get/id/394113") }
    withClue("never of the plain poster download: ") { directPosters.get shouldBe 0 }
    wiring.posterEgressRoutes.keySet should contain allOf ("biletyna.pl", "bilety.ck105.koszalin.pl", "www.multikino.pl")
  }

  "a test wiring handed the residential-proxy credentials" should "build no proxy leg" in {
    new PaidKeyedWiring(new CountingLeaf, Seq("DECODO_PROXY_USER" -> "user", "DECODO_PROXY_PASS" -> "pass"))
      .proxyShardsBuilt shouldBe None
  }

  // A replay's identity model builds no traces and writes none: they are the admin page's diagnostics,
  // and a US cut-over replay spent a tenth of its CPU deriving them and Mongo bulk writes that timed out
  // filing them. Asked with a database present, where the production wiring files them into it.
  "the archive replay wiring" should "discard the identity model's traces even when it has a database" in {
    // Never connected: building a trace store only binds it to the database.
    val client = org.mongodb.scala.MongoClient("mongodb://127.0.0.1:1")
    try {
      val storage = new FetchOnlyStorage {
        override lazy val connection = new services.MongoConnection(uri = None, dbName = settings.MongoDatabaseName("kinowo"),
            required = services.MongoRequirement.Optional) {
          override def database = Some(client.getDatabase("replay-traces-spec"))
        }
      }
      final class Exposed extends ArchiveReplayWiring(Country.Poland, new InMemoryScrapeArchiveRepository, None, storage, fixtureTree,
          settings.FixtureRoot.RepositoryRelative) {
        def traces: services.identity.IdentityTraceStore = identityTraces
      }
      new Exposed().traces shouldBe services.identity.IdentityTraceStore.Discard
    } finally client.close()
  }

  // An order-independence replay's passes parse each TMDB body once between them: the bodies they are
  // handed are the ones the identity store's normalizer and every TMDB client read. Without them, a
  // replay parses for itself as production does — the per-thread handoff, never a shared memo.
  it should "read TMDB bodies through the shared parses it is handed, and through production's own otherwise" in {
    val shared = new SharedJsonBodies
    val passes = (1 to 2).map(_ => new ArchiveReplayWiring(Country.Poland, new InMemoryScrapeArchiveRepository, None, new FetchOnlyStorage,
      fixtureTree, settings.FixtureRoot.RepositoryRelative, tmdbBodies = shared))
    passes.foreach(_.tmdbJsonBodies should be theSameInstanceAs shared)
    val alone = new ArchiveReplayWiring(Country.Poland, new InMemoryScrapeArchiveRepository, None, new FetchOnlyStorage, fixtureTree,
      settings.FixtureRoot.RepositoryRelative)
    alone.tmdbJsonBodies.getClass shouldBe classOf[JsonBodies]
  }

  // An order-independence replay's passes share one identity TMDB layer: a search answer one pass's normalizer files is
  // in the store every other pass reads, and every pass's family answers count each other's filings. Without one, a
  // replay keeps its own, as a worker does.
  it should "file TMDB answers into the identity layer it is handed, shared with every pass handed the same one" in {
    val layer  = new services.identity.IdentityTmdbLayer(None, java.time.Clock.fixed(TestWiring.FixedInstant, java.time.ZoneOffset.UTC))
    def pass(identityTmdb: Option[services.identity.IdentityTmdbLayer]) =
      new ArchiveReplayWiring(Country.Poland, new InMemoryScrapeArchiveRepository, None, new FetchOnlyStorage, fixtureTree,
        settings.FixtureRoot.RepositoryRelative, identityTmdb = identityTmdb)
    val (first, second, alone) = (pass(Some(layer)), pass(Some(layer)), pass(None))
    val question = services.identity.TmdbStore.titleSearchId("pl-PL", "Diuna")
    first.identityTmdbNormalizer.filed("GET", "https://api.themoviedb.org/3/search/movie?query=Diuna&language=pl-PL",
      scala.util.Success("""{"results":[{"id":438631,"title":"Diuna","original_title":"Dune","release_date":"2021-09-15","popularity":50.0}]}"""))
    second.identityTmdbStore.get(services.identity.TmdbKind.Query, Seq(question)).keySet shouldBe Set(question)
    second.familyAnswerStore should be theSameInstanceAs first.familyAnswerStore
    alone.identityTmdbStore.get(services.identity.TmdbKind.Query, Seq(question)) shouldBe empty
  }
}
