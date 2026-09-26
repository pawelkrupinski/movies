package modules

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import models.Country
import services.MongoConnection
import services.events.{ImdbIdMissing, StagingFilmEnriched}
import services.staging.FoldOnStagingEnriched
import services.tasks.{ScrapeReaper, TaskType, UnresolvedTmdbReaper}
import services.metrics.{PrometheusExposition, WorkerHttpMetrics}
import tools.{ExecutionBudget, GetOnlyHttpFetch, HttpFetch, SharedExecutionBudget, TestWiring}

import scala.concurrent.duration._

/** The worker composition root must boot BOTH halves of the write pipeline:
 *  the scrape side (the queue-driven `scrapeReaper`) and the enrich/TMDB side
 *  (the `unresolvedTmdbReaper`, which drives the phase-spread TMDB re-resolve —
 *  the role MovieService's old daily scheduler used to own). `start()` is what
 *  production calls; this asserts it reaches both cascade entry points.
 *
 *  Deterministic spy approach (no network): a `TestWiring` (disabled Mongo, stub
 *  TMDB key, in-memory task queue + freshness store so the unconditional queue
 *  path boots without a cluster) with `scrapeReaper` + `unresolvedTmdbReaper`
 *  overridden by spy subclasses whose `start()` only records the call — the real
 *  `start()` (which schedules background pools) is never invoked for those two,
 *  so nothing touches the network. We then assert both flags flipped. */
class WorkerWiringSpec extends AnyFlatSpec with Matchers {

  class SpyWiring extends TestWiring {
    @volatile var scrapeStarted = false
    @volatile var tmdbRetryStarted = false

    override lazy val scrapeReaper: ScrapeReaper =
      new ScrapeReaper(cinemaScrapers, taskQueue, freshnessStore) {
        override def start(): Unit = scrapeStarted = true
      }

    override lazy val unresolvedTmdbReaper: UnresolvedTmdbReaper =
      new UnresolvedTmdbReaper(movieCache, movieService.retryResolve) {
        override def start(): Unit = tmdbRetryStarted = true
      }
  }

  // A Filmweb-disabled country: same test seams as SpyWiring, but the per-country
  // Filmweb gate pinned off (Country is sealed with only Poland — filmwebEnabled
  // is the wiring's gate, so overriding it is how we simulate a non-Filmweb country).
  class NoFilmwebWiring extends SpyWiring {
    override protected def filmwebEnabled: Boolean = false
  }

  // A minimal wiring that varies the WorkerWiring CONSTRUCTOR (country + injected
  // budget) — which `TestWiring` can't, since it fixes the no-arg super-constructor.
  // Mongo is pinned disabled so nothing connects; we only read the derivation seams.
  class Probe(c: Country, b: ExecutionBudget, e: tools.Env = tools.Env.of())
      extends WorkerWiring(c, b, env = e) {
    override lazy val mongoConnection: MongoConnection =
      new MongoConnection(uri = None, dbName = settings.MongoDatabaseName("unused"), required = services.MongoRequirement.Optional)
    def dbNameForTest: String              = mongoDbName.value
    def defaultScrapeCitiesForTest: Set[String] = scrapeCitiesDefault
  }

  // The fold-on-conclusion policy lives in `FoldOnStagingEnriched` (its own spec);
  // what the root owes is the SUBSCRIPTION. A spy stands in for the real subscriber
  // so the assertion is exactly "publishing the event reaches it", with no fold run.
  class FoldSpyWiring extends TestWiring {
    val folded = scala.collection.mutable.ListBuffer.empty[String]
    override lazy val foldOnStagingEnriched: FoldOnStagingEnriched =
      new FoldOnStagingEnriched(stagingFolder, stagingRepository, (_, _) => ()) {
        override def fold(title: String) = { folded += title; Seq.empty }
      }
  }

  "Constructing WorkerWiring" should "subscribe the staging fold to StagingFilmEnriched" in {
    val wiring = new FoldSpyWiring
    wiring.eventBus.publish(StagingFilmEnriched("Newcomer"))
    wiring.folded shouldBe Seq("Newcomer")
    wiring.stop()
  }

  it should "hand every country's movie cache the one intern pool the metrics bundle publishes" in {
    val shared = services.metrics.WorkerMetrics.singleCountry(Country.default, poolSize = settings.WorkerPoolSize(1))
    def wiringOn(metrics: services.metrics.WorkerMetrics) =
      new WorkerWiring(Country.default, injectedWorkerMetrics = Some(metrics)) with TestWiring
    val (first, second) = (wiringOn(shared), wiringOn(shared))
    try {
      first.movieCache.stringPool should be theSameInstanceAs shared.stringPool
      second.movieCache.stringPool should be theSameInstanceAs shared.stringPool
    } finally { first.stop(); second.stop() }
  }

  it should "hand every country's poster shrinker the one gate it was given" in {
    val gate = services.sharecards.VipsPosterShrinker.newGate()
    def wiringWith(g: tools.PosterDecodeGate) =
      new WorkerWiring(Country.default, posterShrinkGate = g) with TestWiring
    val (first, second) = (wiringWith(gate), wiringWith(gate))
    try {
      first.posterShrinker.gate should be theSameInstanceAs gate
      second.posterShrinker.gate should be theSameInstanceAs gate
    } finally { first.stop(); second.stop() }
  }

  "WorkerWiring.start()" should "boot both the scrape and the enrichment cascade" in {
    val wiring = new SpyWiring
    wiring.start()
    wiring.scrapeStarted shouldBe true
    wiring.tmdbRetryStarted shouldBe true
    wiring.stop()
  }

  // The k3s move left the alerters' chat ids behind and nothing said so for weeks: an
  // alerter wired off must be on /metrics from boot, on or off, for WorkerAlerterDisabled.
  it should "publish whether each of the country's alerters is wired" in {
    val wiring = new SpyWiring
    wiring.start()
    val text = PrometheusExposition.render(wiring.workerMetrics.registry)
    wiring.stop()
    Seq("filmweb_fallback", "filmweb_drop", "staging_stuck").foreach { alerter =>
      text should include regex s"""kinowo_worker_alerter_enabled\\{alerter="$alerter",country="pl"\\} [01]"""
    }
  }

  // Smoothing guard: a resolution BUS EVENT must NOT fan out rating tasks. The old
  // cascade subscribed the rating fetchers to the resolution events and dumped four
  // rating tasks per event instantly (the unspread amplifier behind the midday
  // `kinowo_worker_tasks` rating spikes). Rating enqueues now come from only two
  // bounded paths: the EnrichmentReaper's capped + phase-spread corpus sweep, and the
  // immediate newcomer-fold kick (a trickle — a few promotions a day). `ImdbIdMissing`
  // (the one surviving resolution event) drives id recovery alone, never a rating
  // enqueue — so publishing it on its own leaves the queue untouched.
  it should "not fan out rating tasks when a resolution event fires (only the reaper + newcomer fold enqueue ratings)" in {
    val wiring = new SpyWiring
    val before = wiring.taskQueue.countByState().values.sum
    wiring.eventBus.publish(ImdbIdMissing("Dune", Some(2024), "Dune"))
    wiring.taskQueue.countByState().values.sum shouldBe before
    wiring.stop()
  }

  // Smoothing lever: the reapers are wired with the finer (≤1min) tick interval,
  // so the rating/detail sweeps enqueue a flat per-minute trickle instead of
  // dumping a 5-min-wide backlog in one tick (the residual `kinowo_worker_tasks`
  // spikes). The reapers now read the interval by-name (a live, mid-flight-
  // flippable knob), so this guards the wiring value the composition root supplies.
  it should "wire both reapers with a sub-5-minute tick interval so enqueues stay flat" in {
    val wiring = new SpyWiring
    wiring.enrichmentTickInterval.value should be <= (1.minute: FiniteDuration)
    wiring.detailTickInterval.value     should be <= (1.minute: FiniteDuration)
    wiring.stop()
  }

  // Smoothing default: scrape + enrichment + the rating refreshers share ONE
  // background concurrency budget; capping it at 4 (was 8) is what flattens the
  // per-tick CPU burst that drives the shared-cpu credit downslope — a 2026-06-27
  // live A/B showed 4 ~halved the burst (busy p95 156→58 centi-cores) at unchanged
  // scrape throughput. Guards the composition-root default against a silent bump.
  it should "cap the shared background concurrency budget at 4 by default" in {
    val wiring = new SpyWiring
    wiring.backgroundBudget match {
      case budget: SharedExecutionBudget => budget.maxConcurrent shouldBe 4
      case other                         => fail(s"expected a SharedExecutionBudget, got $other")
    }
    wiring.stop()
  }

  // Per-country Filmweb gate: a Filmweb-enabled country (Poland) wires the Filmweb
  // rating handler + bulk-refresh handler; a disabled country wires neither, so its
  // TaskWorker can't run any Filmweb task and no Filmweb source is constructed.
  "The Filmweb path" should "be wired for a Filmweb-enabled country and dropped for a disabled one" in {
    val enabled  = new SpyWiring
    enabled.ratingHandlers.map(_.taskType)   should contain (TaskType.FilmwebRating: TaskType)
    enabled.operatorHandlers.map(_.taskType) should contain (TaskType.RefreshAllFilmweb: TaskType)
    enabled.stop()

    val disabled = new NoFilmwebWiring
    disabled.ratingHandlers.map(_.taskType)   should not contain (TaskType.FilmwebRating: TaskType)
    disabled.operatorHandlers.map(_.taskType) should not contain (TaskType.RefreshAllFilmweb: TaskType)
    // The other three rating sources are untouched by the gate.
    disabled.ratingHandlers.map(_.taskType) should contain allOf
      (TaskType.ImdbRating: TaskType, TaskType.RtRating: TaskType, TaskType.McRating: TaskType)
    disabled.stop()
  }

  // The whole point of hoisting the budget into WorkerMain: N country wirings draw
  // permits from ONE shared SharedExecutionBudget (one Semaphore/cap), and each
  // wiring scopes to its own country's cities + database.
  "Two country wirings" should "share one injected background budget and scope to their own country" in {
    val budget = new SharedExecutionBudget(4)
    val w1 = new Probe(Country.Poland, budget)
    val w2 = new Probe(Country.Poland, budget)

    (w1.backgroundBudget eq budget)              shouldBe true
    (w2.backgroundBudget eq budget)              shouldBe true
    (w1.backgroundBudget eq w2.backgroundBudget) shouldBe true

    w1.country shouldBe Country.Poland
    w1.defaultScrapeCitiesForTest shouldBe Country.Poland.cities.map(_.slug).toSet
    w1.dbNameForTest              shouldBe Country.Poland.mongoDb
  }

  // Every knob the wiring reads comes off the Env its root handed it — not a
  // process-global — so two wirings built over different configs disagree.
  it should "read its knobs from the Env it was handed" in {
    val budget = new SharedExecutionBudget(4)
    val tuned = new Probe(Country.Poland, budget,
      tools.Env.of("MONGODB_DB" -> "kinowo_probe_db", "KINOWO_SCRAPE_TASKS_PER_VENUE" -> "3"))
    val plain = new Probe(Country.Poland, budget)
    tuned.dbNameForTest       shouldBe "kinowo_probe_db"
    tuned.scrapeTasksPerVenue shouldBe settings.ScrapeTasksPerVenue(3)
    plain.dbNameForTest       shouldBe Country.Poland.mongoDb
    plain.scrapeTasksPerVenue shouldBe settings.ScrapeTasksPerVenue(1)
  }

  // A wiring that only forces the pure, no-I/O catalog derivations (`detailEnrichers`)
  // for a chosen country — mirrors `Probe` above, minus the members `Probe` doesn't
  // need either.
  class DetailProbe(c: Country) extends WorkerWiring(c) {
    override lazy val mongoConnection: MongoConnection =
      new MongoConnection(uri = None, dbName = settings.MongoDatabaseName("unused"), required = services.MongoRequirement.Optional)
    def detailEnricherClassNames: Set[String] = detailEnrichers.map(_.getClass.getSimpleName).toSet
  }

  // Cineworld is a UK-only chain (see CineworldClient), so its chain-wide
  // DetailEnricher must be wired for the UK's own worker only. Before this,
  // `detailEnrichers` was built off `cinemaScraperCatalog.all` — the catalog
  // spanning EVERY country — so Poland's (and every other country's) worker
  // ALSO wired a `CineworldClient` DetailEnricher, enqueuer and reaper entry,
  // and independently recorded chain-wide detail-fetch outcomes under the
  // shared "Cineworld Enrichment" service name: confirmed live via
  // kinowo.net's own /metrics reporting real successes/failures for that
  // service tagged `country="pl"`, and the /uptime page showing it failing on
  // Poland's own page even though no Polish cinema is a Cineworld venue.
  "detailEnrichers" should "exclude another country's chain-wide enricher" in {
    new DetailProbe(Country.Poland).detailEnricherClassNames should not contain "CineworldClient"
  }

  it should "include it for the country that actually has that chain" in {
    new DetailProbe(Country.UnitedKingdom).detailEnricherClassNames should contain ("CineworldClient")
  }

  // The phase split: cinema-site HTTP (`httoFetch`) and third-party metadata/rating
  // HTTP (`enrichmentFetch`) are separate chains sharing one wire leaf, differing
  // ONLY at the innermost counter's `phase` label. This is what lets a Grafana
  // panel read the cinema-scrape failure budget without the enrichment APIs' 404
  // slug-probing and 429s blurring it. Drives a real call through each full chain
  // over a fake leaf (no network) and asserts the two land on different series.
  class PhaseLeafProbe extends SpyWiring {
    override protected def realHttpLeaf: HttpFetch = new GetOnlyHttpFetch {
      override def get(url: String): String = "ok"
    }
  }

  // Both side collections must be wired into the worker's repository, or their writes
  // silently go nowhere: `movies` keeps the embedded copy, the split never takes effect,
  // and the change stream keeps carrying whole documents. The seam is invisible from
  // outside the repository, so it is surfaced as `hasScreenings` / `hasSlots`.
  "The worker's movie repository" should "have both the screenings and the slots split wired" in {
    val wiring = new PhaseLeafProbe
    wiring.movieRepository.hasScreenings shouldBe true
    wiring.movieRepository.hasSlots      shouldBe true
    wiring.stop()
  }

  "The phase-split fetch chains" should "tally cinema-site calls under `scrape` and metadata calls under `enrich`" in {
    val wiring = new PhaseLeafProbe
    (wiring.enrichmentFetch eq wiring.httoFetch) shouldBe false
    wiring.httoFetch.get("https://cinema.example/listing")
    wiring.enrichmentFetch.get("https://api.themoviedb.org/3/movie/1")

    val text = PrometheusExposition.render(wiring.workerMetrics.registry)
    val scrape = WorkerHttpMetrics.Phase.Scrape
    val enrich = WorkerHttpMetrics.Phase.Enrich
    text should include (s"""kinowo_worker_http_total{country="pl",outcome="success",phase="$scrape"} 1""")
    text should include (s"""kinowo_worker_http_total{country="pl",outcome="success",phase="$enrich"} 1""")
    // Not double-counted onto the other phase.
    text should include (s"""kinowo_worker_http_total{country="pl",outcome="success",phase="$scrape"} 1""")
    wiring.stop()
  }

  /** A wiring that records which rating sources the sweep actually drove. Asserting
   *  on the SOURCE rather than on an HTTP call is deliberate: `enrichRatingsSync`
   *  wraps all four refreshes in one `try`, so a throw from any earlier source (a stub
   *  leaf's unparseable body will do it) skips the rest — and a test watching the wire
   *  would then pass for Filmweb whether the gate existed or not. */
  class RatingSourceRecordingWiring extends SpyWiring {
    val refreshed = new java.util.concurrent.ConcurrentLinkedQueue[String]()
    private def record(name: String): Unit = { refreshed.add(name); () }

    // `TestWiring` pins Mongo disabled, so the production repository swallows the
    // seed and the sweep would walk an empty cache — passing whatever the gate did.
    override lazy val movieRepository: services.movies.MovieRepository =
      new services.movies.InMemoryMovieRepository(
        Seq(("Diuna", Some(2024), models.MovieRecord(tmdbId = Some(438631), imdbId = Some("tt15239678")))), normalizer = titleNormalizer)

    // All FOUR stubbed, so the sweep is hermetic and reaches the gate: the real
    // sources would go to the network on the first call and the swallowed throw
    // would skip everything after it.
    override lazy val imdbRatings: services.enrichment.ImdbRatings =
      new services.enrichment.ImdbRatings(movieCache, imdbClient) {
        override def refreshOneSync(t: String, y: Option[Int]): Option[String] = { record("imdb"); None }
      }
    override lazy val rottenTomatoesRatings: services.enrichment.RottenTomatoesRatings =
      new services.enrichment.RottenTomatoesRatings(movieCache, tmdbClient, rottenTomatoesClient) {
        override def refreshOneSync(t: String, y: Option[Int]): Option[String] = { record("rt"); None }
      }
    override lazy val metascoreRatings: services.enrichment.MetascoreRatings =
      new services.enrichment.MetascoreRatings(movieCache, tmdbClient, metacriticClient) {
        override def refreshOneSync(t: String, y: Option[Int]): Option[String] = { record("mc"); None }
      }
    override lazy val filmwebRatings: services.enrichment.FilmwebRatings =
      new services.enrichment.FilmwebRatings(movieCache, tmdbClient, filmwebClient) {
        override def refreshOneSync(t: String, y: Option[Int]): Option[String] = { record("filmweb"); None }
      }
    def sources: Seq[String] = refreshed.toArray(Array.empty[String]).toSeq
  }

  /**
   * The harness's rating sweep stands in for production's `RatingHandler`s, so it has
   * to be gated where they are. Unconditional, it drove Filmweb for every country:
   * prod's German and British corpora hold 0 `filmwebRating` and 0 `filmwebUrl`, while
   * the convergence legs reported 972 and 1293 — a field invented by the harness, on
   * ~2,250 live calls prod never makes, on the two longest legs in the suite.
   */
  /** The eligibility production uses, not a tmdbId gate. `RatingSources` makes IMDb
   *  eligible on an `imdbId` alone and Filmweb on `tmdbId OR filmwebUrl` — the latter
   *  precisely so a tmdbId-less row can RESOLVE its tmdbId via Filmweb→Wikidata. The
   *  harness gated all four on `tmdbId`, so the row that route exists for was the one
   *  row it never walked; 21 of the 25 films production identifies and the replay does
   *  not carry a Filmweb slot. */
  it should "refresh a tmdbId-less row that IMDb and Filmweb are still eligible for" in {
    val wiring = new RatingSourceRecordingWiring {
      override lazy val movieRepository: services.movies.MovieRepository =
        new services.movies.InMemoryMovieRepository(Seq(("Brzezina", Some(1970), models.MovieRecord(
          imdbId = Some("tt0068321"), filmwebUrl = Some("https://www.filmweb.pl/film/Brzezina-1970-8085")))), normalizer = titleNormalizer)
    }
    wiring.movieCache.rehydrate()

    wiring.enrichRatingsSync()

    withClue(s"sources driven: ${wiring.sources.mkString(", ")}: ") {
      wiring.sources should contain allOf ("imdb", "filmweb")   // eligible without a tmdbId
      wiring.sources should not contain "rt"                     // RT/MC really are tmdbId-gated
      wiring.sources should not contain "mc"
    }
    wiring.stop()
  }

  /** A wiring whose rating sources report how many of them are in flight at once.
   *
   *  Each `refreshOneSync` holds its slot briefly, so a serial drain peaks at one
   *  claimant and a pooled one peaks at the budget's cap. The sleep is what makes the
   *  difference observable at all — without it a handler returns before the next
   *  claimant has started and even four threads peak at one. */
  class ConcurrencyRecordingWiring(budget: tools.ExecutionBudget) extends SpyWiring {
    override lazy val backgroundBudget: tools.ExecutionBudget = budget

    private val inFlight = new java.util.concurrent.atomic.AtomicInteger(0)
    private val peak     = new java.util.concurrent.atomic.AtomicInteger(0)
    def peakInFlight: Int = peak.get()

    private def occupyASlot(): Option[String] = {
      val now = inFlight.incrementAndGet()
      peak.updateAndGet(seen => math.max(seen, now))
      try { Thread.sleep(120); None } finally { inFlight.decrementAndGet(); () }
    }

    // Four films rather than one: a single film's four sources would let a serial
    // drain look concurrent if the queue ever handed the same task out twice.
    override lazy val movieRepository: services.movies.MovieRepository =
      new services.movies.InMemoryMovieRepository(Seq(
        ("Diuna",     Some(2024), models.MovieRecord(tmdbId = Some(438631), imdbId = Some("tt15239678"))),
        ("Zimna wojna", Some(2018), models.MovieRecord(tmdbId = Some(468622), imdbId = Some("tt6543652"))),
        ("Ida",       Some(2013), models.MovieRecord(tmdbId = Some(228150), imdbId = Some("tt2718492"))),
        ("Boże Ciało", Some(2019), models.MovieRecord(tmdbId = Some(550310), imdbId = Some("tt9078374")))), normalizer = titleNormalizer)

    override lazy val imdbRatings: services.enrichment.ImdbRatings =
      new services.enrichment.ImdbRatings(movieCache, imdbClient) {
        override def refreshOneSync(t: String, y: Option[Int]): Option[String] = occupyASlot()
      }
    override lazy val rottenTomatoesRatings: services.enrichment.RottenTomatoesRatings =
      new services.enrichment.RottenTomatoesRatings(movieCache, tmdbClient, rottenTomatoesClient) {
        override def refreshOneSync(t: String, y: Option[Int]): Option[String] = occupyASlot()
      }
    override lazy val metascoreRatings: services.enrichment.MetascoreRatings =
      new services.enrichment.MetascoreRatings(movieCache, tmdbClient, metacriticClient) {
        override def refreshOneSync(t: String, y: Option[Int]): Option[String] = occupyASlot()
      }
    override lazy val filmwebRatings: services.enrichment.FilmwebRatings =
      new services.enrichment.FilmwebRatings(movieCache, tmdbClient, filmwebClient) {
        override def refreshOneSync(t: String, y: Option[Int]): Option[String] = occupyASlot()
      }
  }

  /**
   * Production drains the rating queue with a POOL — `TaskWorker` runs
   * `TaskWorker.DefaultPoolSize` threads, each claiming and handling independently.
   * The harness's synchronous stand-in claimed one task at a time, and that is a rule
   * it restated rather than inherited: it made the harness strictly slower than the
   * thing it stands in for, and it dominated the convergence legs. Poland's
   * `enrichRatings` phase was 1,615s of a 2,201s boot — 85% of its enrichment calls
   * are free fixture replays, so nearly all of that was a serial tail of network
   * round-trips that production would have overlapped four ways.
   */
  "the harness rating drain" should "claim through as many workers as the background budget allows" in {
    val wiring = new ConcurrencyRecordingWiring(new SharedExecutionBudget(4))
    wiring.movieCache.rehydrate()

    wiring.enrichRatingsSync()

    withClue(s"peak in-flight rating handlers: ${wiring.peakInFlight}: ") {
      wiring.peakInFlight should be > 1
    }
    wiring.stop()
  }

  /** A wiring whose STAGING handlers report how many of them are in flight at once.
   *
   *  Same instrument as `ConcurrencyRecordingWiring` above, one seam further in: the
   *  staging handlers are replaced by a single spy that holds its slot briefly, so a
   *  serial drain peaks at one claimant and a pooled one peaks at the budget's cap.
   *  `stagingHandlers` is the whole set the drain dispatches on, so overriding it also
   *  keeps the real reaper from enqueuing anything this spy would then mis-handle. */
  class StagingConcurrencyWiring(budget: ExecutionBudget) extends SpyWiring {
    override lazy val backgroundBudget: ExecutionBudget = budget

    private val inFlight = new java.util.concurrent.atomic.AtomicInteger(0)
    private val peak     = new java.util.concurrent.atomic.AtomicInteger(0)
    def peakInFlight: Int = peak.get()

    override lazy val stagingHandlers: Seq[services.tasks.TaskHandler] = Seq(
      new services.tasks.TaskHandler {
        override def taskType: TaskType = TaskType.StagingFold
        override def handle(task: services.tasks.Task): services.tasks.HandlerOutcome = {
          val now = inFlight.incrementAndGet()
          peak.updateAndGet(seen => math.max(seen, now))
          try { Thread.sleep(120); services.tasks.HandlerOutcome.Done }
          finally { inFlight.decrementAndGet(); () }
        }
      })
  }

  /**
   * Production drains the staging queue with the same POOL it drains every other
   * queue with. The harness claimed one task at a time, and by 2026-09-03 that was
   * the single most expensive thing a convergence leg did: Poland spent 287.8s of a
   * 454.1s boot in the staging drain, working 7,770 tasks at ~37ms each against a
   * WARM enrichment tree (2,914 cache hits, 421 live fills) — so it was not upstream
   * network but a serial file of Mongo round-trips production overlaps four ways.
   */
  "the harness staging drain" should "claim through as many workers as the background budget allows" in {
    val wiring = new StagingConcurrencyWiring(new SharedExecutionBudget(4))
    (1 to 8).foreach(i => wiring.taskQueue.enqueue(TaskType.StagingFold, s"film-$i"))

    wiring.advanceStagingOnce()

    withClue(s"peak in-flight staging handlers: ${wiring.peakInFlight}: ") {
      wiring.peakInFlight should be > 1
    }
    wiring.stop()
  }

  it should "stay strictly serial under a same-thread budget, like every other drain" in {
    // The order-independence replay passes wire `SameThreadExecutionBudget` so their
    // seeded shuffle is the only nondeterminism left. A staging drain that pooled
    // regardless would put a thread race under the very assertion written to catch
    // order dependence — and it would flake rather than fail.
    val wiring = new StagingConcurrencyWiring(new tools.SameThreadExecutionBudget)
    (1 to 8).foreach(i => wiring.taskQueue.enqueue(TaskType.StagingFold, s"film-$i"))

    wiring.advanceStagingOnce()

    withClue(s"peak in-flight staging handlers: ${wiring.peakInFlight}: ") {
      wiring.peakInFlight shouldBe 1
    }
    wiring.stop()
  }

  /** …and follows that same budget DOWN. The convergence suite's order-independence
   *  passes wire `SameThreadExecutionBudget` precisely so the only nondeterminism left
   *  is their seeded shuffle; a drain that pooled regardless would put a thread race
   *  back under the assertion written to catch order dependence, and it would flake
   *  rather than fail. The budget is the one lever, so no spec has to restate it. */
  it should "stay strictly serial under a same-thread budget" in {
    val wiring = new ConcurrencyRecordingWiring(new tools.SameThreadExecutionBudget)
    wiring.movieCache.rehydrate()

    wiring.enrichRatingsSync()

    withClue(s"peak in-flight rating handlers: ${wiring.peakInFlight}: ") {
      wiring.peakInFlight shouldBe 1
    }
    wiring.stop()
  }

  "the harness rating sweep" should "not drive Filmweb for a country that has no Filmweb" in {
    val wiring = new RatingSourceRecordingWiring { override protected def filmwebEnabled: Boolean = false }
    wiring.movieCache.rehydrate()

    wiring.enrichRatingsSync()

    withClue(s"sources driven: ${wiring.sources.mkString(", ")}: ") {
      wiring.sources should contain allOf ("imdb", "rt", "mc")   // the sweep DID run
      wiring.sources should not contain "filmweb"                 // …and skipped only this
    }
    wiring.stop()
  }

  /** Each source gets its own `try`, so one that throws cannot skip the ones after
   *  it. Shared, a single `try` made coverage depend on a source's POSITION: a local
   *  run whose Rotten Tomatoes probes 404'd finished with Metacritic 12 and Filmweb 11
   *  against CI's 307 and 478, because RT is listed above them. */
  it should "keep refreshing the other sources when one of them throws" in {
    val wiring = new RatingSourceRecordingWiring {
      override lazy val rottenTomatoesRatings: services.enrichment.RottenTomatoesRatings =
        new services.enrichment.RottenTomatoesRatings(movieCache, tmdbClient, rottenTomatoesClient) {
          override def refreshOneSync(t: String, y: Option[Int]): Option[String] =
            throw new RuntimeException("RT slug probe 404'd")
        }
    }
    wiring.movieCache.rehydrate()

    wiring.enrichRatingsSync()

    withClue(s"sources driven: ${wiring.sources.mkString(", ")}: ") {
      wiring.sources should contain allOf ("imdb", "mc", "filmweb")
    }
    wiring.stop()
  }

  // …and still does for a country that HAS it, so the gate can't be "off everywhere".
  it should "still drive Filmweb for a country that has it" in {
    val wiring = new RatingSourceRecordingWiring
    wiring.movieCache.rehydrate()

    wiring.enrichRatingsSync()

    wiring.sources should contain ("filmweb")
    wiring.stop()
  }

  /**
   * `resolveTmdbId`'s last rungs — `viaLetterboxd` and `viaFilmwebWikidata` — are the
   * ones that exist for the arthouse long tail TMDB's own search misses. Prod passes
   * both resolvers in; the harness rebuilt `MovieService` with positional defaults and
   * silently got `None` for each, so a suite whose entire purpose is to measure how
   * much of a country's repertoire resolves was measuring it with two rungs sawn off.
   */
  "the harness MovieService" should "keep prod's Letterboxd rung of the resolution ladder" in {
    val asked = new java.util.concurrent.atomic.AtomicReference[String]("")
    val wiring = new SpyWiring {
      // Valid-but-empty payloads: TMDB's own search and `/find` both conclude "nothing",
      // which is what hands the row down to the fallback rungs.
      override protected def realHttpLeaf: HttpFetch = new GetOnlyHttpFetch {
        override def get(url: String): String = """{"results":[],"movie_results":[]}"""
      }
      override lazy val movieRepository: services.movies.MovieRepository =
        new services.movies.InMemoryMovieRepository(
          Seq(("Obscure Arthouse Film", Some(2019), models.MovieRecord(imdbId = Some("tt5555555")))), normalizer = titleNormalizer)
      override lazy val letterboxdIdResolver: services.enrichment.LetterboxdIdResolver =
        new services.enrichment.LetterboxdIdResolver(letterboxdClient) {
          override def resolveTmdbId(imdbId: String): Option[Int] = { asked.set(imdbId); None }
        }
    }
    wiring.movieCache.rehydrate()

    wiring.movieService.retryResolve("Obscure Arthouse Film", Some(2019))
    wiring.drainServices()

    withClue("the Letterboxd rung was never consulted: ") { asked.get() shouldBe "tt5555555" }
    wiring.stop()
  }

  // Confidence-gated ratings are a staged-migration switch (identity phase 3): nothing the
  // resolver computes may reach the read model until the composition root is told so.
  "the rating gate" should "be off unless KINOWO_IDENTITY_RATING_GATE switches it on" in {
    val budget = new SharedExecutionBudget(4)
    new Probe(Country.Poland, budget).ratingGate shouldBe theSameInstanceAs(services.identity.RatingGate.off)
    new Probe(Country.Poland, budget, tools.Env.of("KINOWO_IDENTITY_RATING_GATE" -> "true")).ratingGate should not be
      theSameInstanceAs(services.identity.RatingGate.off)
  }

  // The identity shadow run is a staged-migration switch too: nothing resolves in shadow — and no
  // shadow collection is written — until the composition root is told so.
  "the identity shadow run" should "be wired only when KINOWO_IDENTITY_SHADOW switches it on" in {
    val budget = new SharedExecutionBudget(4)
    def probe(env: tools.Env) = new Probe(Country.Spain, budget, env) {
      override lazy val observationStore: Option[services.observations.ObservationStore] =
        Some(services.observations.ObservationStore.inMemory(clock))
    }
    probe(tools.Env.of()).shadowIdentityReaper shouldBe None
    probe(tools.Env.of("KINOWO_IDENTITY_SHADOW" -> "true")).shadowIdentityReaper shouldBe defined
    // With nothing observed to read (no capture, no database) there is nothing to resolve from.
    new Probe(Country.Spain, budget, tools.Env.of("KINOWO_IDENTITY_SHADOW" -> "true")).shadowIdentityReaper shouldBe None
  }

  // The capture keeps the identity resolver's evidence — what `TmdbIdentityLookups` reads: the
  // TMDB client's answers and every venue's details — and nothing else. A rating page is per-film
  // enrichment with no bearing on identity (docs/design/identity-resolver.md §2), and Metacritic
  // and Rotten Tomatoes pages were ~80% of what the capture wrote in production.
  "the observation capture" should "observe the TMDB client and the venue details, and no rating or other enrichment client" in {
    val store = services.observations.ObservationStore.inMemory(java.time.Clock.fixed(java.time.Instant.EPOCH, java.time.ZoneOffset.UTC))
    val answersEverything: HttpFetch = new HttpFetch {
      override def get(url: String): String                                  = "{}"
      override def post(url: String, body: String, contentType: String): String = "{}"
    }
    val wiring = new Probe(Country.Poland, new SharedExecutionBudget(4), tools.Env.of("TMDB_API_KEY" -> "test-key")) {
      override lazy val observationStore: Option[services.observations.ObservationStore] = Some(store)
      override lazy val enrichmentFetch: HttpFetch = answersEverything
      override lazy val httoFetch: HttpFetch       = answersEverything
    }
    def attempt(call: => Any): Unit = { scala.util.Try(call); () }
    attempt(wiring.metacriticClient.canonicalUrl("Dune"))
    attempt(wiring.rottenTomatoesClient.scoreFor("https://www.rottentomatoes.com/m/dune"))
    attempt(wiring.imdbClient.lookup("tt1160419"))
    attempt(wiring.imdbClient.findId("Dune", Some(2021)))
    attempt(wiring.filmwebClient.search("Dune"))
    attempt(wiring.wikidataClient.findImdbIdByTitle("Dune", Some(2021)))
    val otherHosts = store.currentLookups().map(_.query.host).toSet
    withClue("rating / enrichment lookups observed: ")(otherHosts shouldBe empty)

    attempt(wiring.tmdbClient.search("Dune", None))
    attempt(wiring.detailEnrichers.head.fetchFilmDetail("dune"))
    val observed = store.currentLookups().map(_.query)
    observed.map(_.host).toSet should contain ("api.themoviedb.org")
    observed.count(_.key.startsWith("DETAIL ")) shouldBe 1
    // What the capture files is exactly what the purge of the unscoped capture keeps.
    observed.filterNot(_.isIdentityEvidence) shouldBe empty
    wiring.stop()
  }

  it should "run after each settle tick, and persist its run" in {
    val wiring = new Probe(Country.Spain, new SharedExecutionBudget(4), tools.Env.of("KINOWO_IDENTITY_SHADOW" -> "true")) {
      override lazy val observationStore: Option[services.observations.ObservationStore] =
        Some(services.observations.ObservationStore.inMemory(clock))
    }
    wiring.shadowRuns.latestRun() shouldBe None
    wiring.settleTick()
    wiring.shadowRuns.latestRun() shouldBe defined
  }

  it should "when switched on, withhold a title-only match no venue's facts back — scored from the row alone" in {
    import models._
    val normalizer = services.movies.SingleCountryNormalizer.titleNormalizer
    val relay = "Samson i dalila | metropolitan opera: live in hd 2026/27"
    val stored = services.movies.StoredMovieRecord.synthesised(relay, None, MovieRecord(
      imdbRating = Some(6.8), imdbId = Some("tt0041838"), tmdbId = Some(29993), data = Map[Source, SourceData](
        Kino1410 -> SourceData(title = Some(relay), rawTitle = Some(relay)),
        Tmdb     -> SourceData(title = Some("Samson i Dalia"), originalTitle = Some("Samson and Delilah"), releaseYear = Some(1949),
                      runtimeMinutes = Some(131), director = Seq("Cecil B. DeMille"), countries = Seq("USA")))), normalizer)
    val movie = services.readmodel.ReadModelProjection.resolve(stored, normalizer)
    val gate = new Probe(Country.Poland, new SharedExecutionBudget(4), tools.Env.of("KINOWO_IDENTITY_RATING_GATE" -> "true")).ratingGate
    gate(stored, movie) shouldBe services.identity.RatingGate.withheld(movie)
  }
}
