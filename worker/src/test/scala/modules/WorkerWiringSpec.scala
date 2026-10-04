package modules

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import models.Country
import services.MongoConnection
import services.events.ImdbIdMissing
import services.tasks.{ScrapeReaper, TaskType}
import services.metrics.{PrometheusExposition, WorkerHttpMetrics}
import tools.{ExecutionBudget, GetOnlyHttpFetch, HttpFetch, SharedExecutionBudget, TestWiring}

import scala.concurrent.duration._

/** The worker composition root must boot BOTH halves of the pipeline: the scrape side
 *  (the queue-driven `scrapeReaper`) and the identity projection (the `settleReaper`'s
 *  projection tick). `start()` is what production calls; this asserts it reaches both
 *  entry points.
 *
 *  Deterministic spy approach (no network): a `TestWiring` (disabled Mongo, stub
 *  TMDB key, in-memory task queue + freshness store so the unconditional queue
 *  path boots without a cluster) with `scrapeReaper` + `settleReaper`
 *  overridden by spy subclasses whose `start()` only records the call — the real
 *  `start()` (which schedules background pools) is never invoked for those two,
 *  so nothing touches the network. We then assert both flags flipped. */
class WorkerWiringSpec extends AnyFlatSpec with Matchers {

  class SpyWiring extends TestWiring {
    @volatile var scrapeStarted = false
    @volatile var projectionStarted = false

    override lazy val scrapeReaper: ScrapeReaper =
      new ScrapeReaper(cinemaScrapers, taskQueue, freshnessStore, clock = _root_.tools.SpecClock.Pinned) {
        override def start(): Unit = scrapeStarted = true
      }

    override lazy val settleReaper: services.tasks.SettleReaper = {
      val runs = scheduledRunStore
      new services.tasks.SettleReaper(() => (), settings.SettleInterval(5.minutes), services.tasks.SettleReaper.InitialDelay(5.minutes),
          runStore = runs, clock = _root_.tools.SpecClock.Pinned) {
        override def start(): Unit = projectionStarted = true
      }
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

  "Constructing WorkerWiring" should "hand every country's movie cache the one intern pool the metrics bundle publishes" in {
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

  "WorkerWiring.start()" should "boot both the scrape and the identity projection" in {
    val wiring = new SpyWiring
    wiring.start()
    wiring.scrapeStarted shouldBe true
    wiring.projectionStarted shouldBe true
    wiring.stop()
  }

  // The k3s move left the alerters' chat ids behind and nothing said so for weeks: an
  // alerter wired off must be on /metrics from boot, on or off, for WorkerAlerterDisabled.
  it should "publish whether each of the country's alerters is wired" in {
    val wiring = new SpyWiring
    wiring.start()
    val text = PrometheusExposition.render(wiring.workerMetrics.registry)
    wiring.stop()
    Seq("filmweb_fallback", "filmweb_drop").foreach { alerter =>
      text should include regex s"""kinowo_worker_alerter_enabled\\{alerter="$alerter",country="pl"\\} [01]"""
    }
  }

  // Smoothing guard: a resolution BUS EVENT must NOT fan out rating tasks. The old
  // cascade subscribed the rating fetchers to the resolution events and dumped four
  // rating tasks per event instantly (the unspread amplifier behind the midday
  // `kinowo_worker_tasks` rating spikes). Rating enqueues now come from only two
  // bounded paths: the EnrichmentReaper's capped + phase-spread corpus sweep, and the
  // identity projection's kick for the films it newly makes (a trickle). `ImdbIdMissing`
  // (the one surviving resolution event) drives id recovery alone, never a rating
  // enqueue — so publishing it on its own leaves the queue untouched.
  it should "not fan out rating tasks when a resolution event fires (only the reaper + the identity projection enqueue ratings)" in {
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

  // The phase split: cinema-site HTTP (`httpFetch`) and third-party metadata/rating
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
    (wiring.enrichmentFetch eq wiring.httpFetch) shouldBe false
    wiring.httpFetch.get("https://cinema.example/listing")
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

  // Confidence-gated ratings are a staged-migration switch (identity phase 3): nothing the
  // resolver computes may reach the read model until the composition root is told so.
  "the rating gate" should "be off unless KINOWO_IDENTITY_RATING_GATE switches it on" in {
    val budget = new SharedExecutionBudget(4)
    new Probe(Country.Poland, budget).ratingGate shouldBe theSameInstanceAs(services.identity.RatingGate.off)
    new Probe(Country.Poland, budget, tools.Env.of("KINOWO_IDENTITY_RATING_GATE" -> "true")).ratingGate should not be
      theSameInstanceAs(services.identity.RatingGate.off)
  }

  // The model reads TMDB from its normalized store first (`StoredFirstLookups`): a question the
  // store has no answer to is asked live once, filed into the store as it arrives, and read from there
  // after — never asked again per take-up.
  "the identity model's lookups" should "ask TMDB live only for what the model's store lacks, and file the answer there" in {
    val requests = new java.util.concurrent.atomic.AtomicInteger()
    val counting: HttpFetch = new HttpFetch {
      override def get(url: String): String = { requests.incrementAndGet(); """{"results":[]}""" }
      override def post(url: String, body: String, contentType: String): String = get(url)
    }
    val wiring = new Probe(Country.Spain, new SharedExecutionBudget(4),
      tools.Env.of("TMDB_API_KEY" -> "test-key")) {
      override lazy val enrichmentFetch: HttpFetch = counting
    }
    val question = services.identity.CandidateQuery.Title("dune")
    wiring.cutoverLookups().candidates(question) shouldBe services.identity.Answer.Known(Nil)
    val asked = requests.get
    asked should be > 0
    wiring.cutoverLookups().candidates(question) shouldBe services.identity.Answer.Known(Nil)
    requests.get shouldBe asked
    wiring.stop()
  }

  // The model asks live only what its store lacks: an answer it holds — a search that found nothing
  // before TMDB had the film, a record TMDB has since changed — is renewed only by the fill's refreshes
  // and change sweep, on a schedule of their own.
  "the identity model's live lookup fill" should "be wired on a claimed schedule of its own that starts a round, at its configured rate" in {
    val rounds = new java.util.concurrent.atomic.AtomicInteger()
    val wiring = new Probe(Country.Spain, new SharedExecutionBudget(4)) {
      override protected lazy val shadowLookupExecutor: java.util.concurrent.ExecutorService = new java.util.concurrent.AbstractExecutorService {
        def execute(command: Runnable): Unit = { rounds.incrementAndGet(); () }
        def shutdown(): Unit = (); def shutdownNow(): java.util.List[Runnable] = java.util.List.of()
        def isShutdown: Boolean = false; def isTerminated: Boolean = false
        def awaitTermination(timeout: Long, unit: java.util.concurrent.TimeUnit): Boolean = true
      }
    }
    wiring.shadowLookupFill.effectiveRate shouldBe WorkerWiring.DefaultShadowLookupRate
    wiring.identityLookupRefreshSchedule.tickIfClaimed() shouldBe true
    rounds.get shouldBe 1
    wiring.stop()
    new Probe(Country.Spain, new SharedExecutionBudget(4), tools.Env.of("KINOWO_IDENTITY_SHADOW_LOOKUP_RATE" -> "12"))
      .shadowLookupFill.effectiveRate shouldBe settings.IdentityShadowLookupRate(12)
  }

  // The model's thread and its prefetch pool outlived `stop()`: a drain still writing families to a
  // Mongo connection the same stop had closed, and every stopped test wiring's threads left behind.
  "stopping a wiring" should "shut down the identity model's thread and its prefetch pool" in {
    final class Threads extends Probe(Country.Spain, new SharedExecutionBudget(4)) {
      def modelThreads: Seq[java.util.concurrent.ExecutorService] = Seq(identityModelScheduler, identityPrefetchPool)
    }
    val wiring = new Threads
    wiring.identityModel
    wiring.stop()
    wiring.modelThreads.map(_.isShutdown) shouldBe Seq(true, true)
  }

  // Each rebuild built its own trace store, whose writer thread no one ever stopped: one idle thread per
  // rebuild for the life of the process, and the replaced model's queued writes racing the new one's.
  it should "write every rebuilt identity model's traces through one store, and close it" in {
    val client = org.mongodb.scala.MongoClient("mongodb://127.0.0.1:1") // never connected: a store only binds to it
    try {
      final class Traces extends Probe(Country.Spain, new SharedExecutionBudget(4)) {
        // The traces' database alone: a whole wiring over an unreachable one would hydrate its stores for minutes.
        override protected def identityTracesDatabase = Some(client.getDatabase("wiring-traces-spec"))
        def traces: services.identity.IdentityTraceStore = identityTraces
      }
      val wiring = new Traces
      wiring.traces should be theSameInstanceAs wiring.traces
    } finally client.close()

    var closed = false
    final class Stopped extends Probe(Country.Spain, new SharedExecutionBudget(4)) {
      override protected def newIdentityTraces: services.identity.IdentityTraceStore = new services.identity.IdentityTraceStore {
        def replace(removed: Set[String], added: Seq[services.identity.FamilyTraces]): Unit = ()
        override def close(): Unit = closed = true
      }
      def traces: services.identity.IdentityTraceStore = identityTraces
    }
    val stopped = new Stopped
    stopped.traces   // a model's first rebuild builds it
    stopped.stop()
    closed shouldBe true
  }

  "the rating gate, switched on," should "withhold a title-only match no venue's facts back — scored from the row alone" in {
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

  // A pod that boots inside an hour a previous pod already claimed (`SettleReaper`'s window) still projects one interval
  // after boot: every projection on scrapes waits for that first one, and behind the claim a restarted worker projected
  // nothing until the next hour (2026-10-04, US/UK/DE).
  "A worker's boot projection" should "run one interval after boot, whatever the reconcile's window claim says" in {
    class Booting extends TestWiring {
      val manual    = new tools.ManualScheduler(new tools.MutableClock(TestWiring.FixedInstant))
      var projected = 0
      override lazy val scheduledRunStore: services.schedule.ScheduledRunStore = services.schedule.NeverClaimScheduledRunStore
      override protected lazy val identityProjectionTriggerScheduler: java.util.concurrent.ScheduledExecutorService = manual
      override def settleTick(): Unit = projected += 1
    }
    val w = new Booting
    w.settleReaper
    w.manual.advance(java.time.Duration.ofSeconds(w.identityProjectionInterval.value.toSeconds - 1))
    w.projected shouldBe 0
    w.manual.advance(java.time.Duration.ofSeconds(1))
    w.projected shouldBe 1
    w.manual.advance(java.time.Duration.ofHours(3))
    w.projected shouldBe 1                                        // once: the hours are the claimed reconcile's
  }
}
