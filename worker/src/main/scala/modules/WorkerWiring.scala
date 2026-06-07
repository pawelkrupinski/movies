package modules

import clients.TmdbClient
import models.City
import services.{MongoConnection, ShowtimeCache, Stoppable, UptimeMonitor}
import services.cinemas._
import services.enrichment._
import services.events.{EventBus, InProcessEventBus}
import services.freshness.{FreshnessStore, MongoFreshnessStore}
import services.movies.{CaffeineMovieCache, MongoMovieRepo, MovieRepo, MovieService, UnscreenedCleanup}
import services.tasks.{MongoTaskQueue, ScrapeCinemaHandler, ScrapeReaper, TaskQueue, TaskWorker}
import tools.{Env, HttpFetch, MonitoringHttpFetch, RealHttpFetch, ScrapeCities, SharedExecutionBudget}

/**
 * Write composition root: cinema scraping + the enrichment cascade. Runs as its
 * own process (`WorkerMain`), writing through MovieCache to Mongo; the serving
 * app's cache picks the writes up via the Mongo change stream. Constructs the
 * shared data layer (Mongo, MovieCache, EventBus, UptimeMonitor) itself — the
 * two apps share the database, not in-process objects.
 *
 * This is the scrape/enrich half of what used to be the monolith's single
 * `Wiring`; the serving half now lives in the web app's `modules.Wiring`.
 */
class WorkerWiring {
  lazy val uptimeMonitor = new UptimeMonitor(mongoConnection.database)
  // `cinemaScraperCatalog.scrapeHosts` is passed BY-NAME (the catalog fetches
  // through this very `httoFetch`, so eager evaluation would cycle). It's forced
  // once on the first request and tells the monitor which hosts are cinema
  // scrapes — suppressed, since RetryingCinemaScraper already tracks each cinema
  // under its displayName.
  lazy val httoFetch: HttpFetch =
    new MonitoringHttpFetch(new RealHttpFetch(), uptimeMonitor, cinemaScraperCatalog.scrapeHosts)

  // ── External API clients ──────────────────────────────────────────────────
  lazy val tmdbClient = new TmdbClient(httoFetch)
  lazy val filmwebClient = new FilmwebClient(httoFetch)
  lazy val imdbClient = new ImdbClient(httoFetch)
  lazy val metacriticClient = new MetacriticClient(httoFetch)
  lazy val rottenTomatoesClient = new RottenTomatoesClient(httoFetch)

  // ── Cinema scrapers ───────────────────────────────────────────────────────
  // The per-city scraper graph lives in CinemaScraperCatalog (Mongo-free, so a
  // diagnostic like tools.FilmwebDiff can build the real scrapers without the
  // worker's write machinery). WorkerWiring supplies the seams it varies —
  // `httoFetch`, the Zyte-routed `multikinoFetch` / `biletynaFetch`, and Helios's REST date — and
  // wraps each raw scraper in RetryingCinemaScraper for production ticks.
  lazy val multikinoFetch: HttpFetch = MultikinoClient.fetchFor(httoFetch)
  // biletyna.pl 403s our datacenter IP; route Kino Kameralne through Zyte.
  lazy val biletynaFetch: HttpFetch = ZyteFallback.fetchFor(httoFetch)
  lazy val cinemaScraperCatalog = new CinemaScraperCatalog(httoFetch, multikinoFetch, biletynaFetch, heliosToday)
  def kinoMuzaClient: KinoMuzaClient = cinemaScraperCatalog.kinoMuzaClient

  // Scrape every modelled city by default; KINOWO_SCRAPE_CITIES (comma-separated
  // slugs) only NARROWS the set, e.g. to shed load if the worker throttles/OOMs.
  // `protected def` so test wirings can pin the set independently.
  protected def scrapeCities: Set[String] =
    ScrapeCities.enabled(Env.get("KINOWO_SCRAPE_CITIES"), default = ScrapeCities.allCities)

  // The date Helios bakes into its REST URLs. Production uses the real Warsaw
  // date; fixture-replay test wirings override with the fixture's capture date.
  protected def heliosToday: java.time.LocalDate =
    java.time.LocalDate.now(java.time.ZoneId.of("Europe/Warsaw"))

  // Upper bound on how many times a cinema scrape is attempted before giving up.
  // Each scraper declares its own `maxFetchAttempts` (default 3; a flaky upstream
  // like GCF raises it) and the wrap below takes the smaller of that and this
  // ceiling. Production leaves the ceiling generous so a cinema's own value wins;
  // fixture-replay test wirings drop it to 1 — a missing fixture is a PERMANENT
  // miss, so retrying just multiplies the fixture-server boot time (backoff per
  // fixture-less cinema, which with the full 40+-city catalogue blows past the CI
  // port-file ceiling).
  protected def scrapeAttemptCeiling: Int = 6

  lazy val cinemaScrapers: Seq[CinemaScraper] =
    City.all
      .filter(c => scrapeCities(c.slug))
      .flatMap(c => cinemaScraperCatalog.byCity.getOrElse(c.slug, Nil))
      .map(s => new RetryingCinemaScraper(
        s, uptimeMonitor, maxAttempts = math.min(s.maxFetchAttempts, scrapeAttemptCeiling)))

  // ── Background concurrency budget ───────────────────────────────────────────
  // Scrape + enrichment + the rating refreshers draw run permits from ONE shared
  // budget so a cold start / hourly rating walk can't peg the worker's vCPU.
  lazy val backgroundBudget = new SharedExecutionBudget(Env.positiveInt("KINOWO_BG_CONCURRENCY", 8))

  // ── Events ────────────────────────────────────────────────────────────────
  lazy val eventBus: EventBus = new InProcessEventBus()

  // ── Mongo ─────────────────────────────────────────────────────────────────
  // The worker is the writer — Mongo is mandatory (opt out only for local dev
  // with MONGODB_OPTIONAL=true).
  lazy val mongoConnection: MongoConnection = {
    val optedOut = Env.get("MONGODB_OPTIONAL").exists(v => v == "true" || v == "1")
    MongoConnection.fromEnv(required = MongoConnection.isRequired(testMode = false, optedOut = optedOut))
  }

  // ── MovieRecord cache (write-through) ───────────────────────────────────────
  lazy val movieRepo: MovieRepo = new MongoMovieRepo(mongoConnection.database, fallbackToOwnInit = false)
  lazy val movieCache: CaffeineMovieCache = new CaffeineMovieCache(movieRepo, eventBus)

  lazy val imdbRatings = new ImdbRatings(movieCache, imdbClient, backgroundBudget.ec("IMDb-stage"))
  lazy val imdbIdResolver = new ImdbIdResolver(movieCache, imdbClient, eventBus, backgroundBudget.ec("imdb-id-resolver"))
  lazy val rottenTomatoesRatings = new RottenTomatoesRatings(movieCache, tmdbClient, rottenTomatoesClient, backgroundBudget.ec("RT-stage"))
  lazy val metascoreRatings = new MetascoreRatings(movieCache, tmdbClient, metacriticClient, backgroundBudget.ec("Metascore-stage"))
  lazy val filmwebRatings = new FilmwebRatings(movieCache, tmdbClient, filmwebClient, backgroundBudget.ec("Filmweb-stage"))
  // How many times the TMDB stage retries a transient failure. Overridden to 0
  // by the fixture-replay test wiring (see TestWiring) so permanent fixture
  // misses don't churn the cascade.
  def tmdbMaxRetries: Int = 6
  lazy val movieService = new MovieService(movieCache, eventBus, tmdbClient, backgroundBudget.ec("enrichment-worker"), maxRetries = tmdbMaxRetries)
  lazy val unscreenedCleanup = new UnscreenedCleanup(movieCache)
  lazy val kinoMuzaSynopsisRefresher = new KinoMuzaSynopsisRefresher(movieCache, kinoMuzaClient, httoFetch)

  // ── Showtime aggregation ──────────────────────────────────────────────────
  // How many cinemas scrape in parallel — a slice of the shared backgroundBudget
  // (see SharedExecutionBudget) so a cold-start scrape can't crowd out enrichment.
  // Single source of truth: the test wiring's scrape spy reuses `showtimeFetchEc`
  // rather than re-spelling this default.
  def scrapeConcurrency: Int = Env.positiveInt("KINOWO_SCRAPE_CONCURRENCY", 4)
  lazy val showtimeFetchEc = backgroundBudget.ec("showtime-fetch", scrapeConcurrency)

  lazy val showtimeCache = new ShowtimeCache(
    cinemaScrapers, eventBus, movieCache, showtimeFetchEc
  )

  // ── Task queue (scrape scheduling) ──────────────────────────────────────────
  // When enabled, cinema scraping is driven by a durable Mongo task queue instead
  // of ShowtimeCache's continuous loop: a reaper enqueues each cinema at most once
  // per 15-min freshness window, and the worker scrapes it (skipping if a
  // concurrent run already refreshed it). Enrichment still flows through the bus
  // exactly as before — only the scrape *scheduling* changes here. Off by default
  // until cut over; KINOWO_QUEUE_SCRAPING=true flips it.
  protected def queueScraping: Boolean =
    Env.get("KINOWO_QUEUE_SCRAPING").exists(v => v == "true" || v == "1")

  lazy val taskQueue: TaskQueue = new MongoTaskQueue(mongoConnection.database)
  lazy val freshnessStore: FreshnessStore = new MongoFreshnessStore(mongoConnection.database)
  lazy val cinemaScrapeRunner = new CinemaScrapeRunner(movieCache, eventBus)
  lazy val scrapeCinemaHandler = new ScrapeCinemaHandler(
    cinemaScrapers.map(s => ScrapeCinemaHandler.scraperKey(s.cinema) -> s).toMap,
    cinemaScrapeRunner, freshnessStore
  )
  // The worker dispatches scrapes onto the same sub-capped budget the old loop
  // used, so a backlog can't peg the box.
  lazy val taskWorker = new TaskWorker(
    taskQueue, Seq(scrapeCinemaHandler),
    backgroundBudget.ec("task-worker", scrapeConcurrency),
    pollInterval = scala.concurrent.duration.DurationInt(5).seconds
  )
  lazy val scrapeReaper = new ScrapeReaper(cinemaScrapers, taskQueue, freshnessStore)

  // Subscribe BEFORE start() so the bus's first MovieRecordCreated events reach
  // the enrichment handlers. (See the original monolith comment block for the
  // full event-cascade rationale — the wiring is unchanged.)
  //   MovieRecordCreated → movieService           (TMDB stage)
  //   TmdbResolved       → imdb/RT/MC/Filmweb ratings
  //   ImdbIdMissing      → imdbIdResolver + RT/MC/Filmweb (TMDB-only hits)
  //   ImdbIdResolved     → imdbRatings
  //   CinemaMovieAdded   → kinoMuzaSynopsisRefresher
  eventBus.subscribe(movieService.onMovieRecordCreated)
  eventBus.subscribe(imdbIdResolver.onImdbIdMissing)
  eventBus.subscribe(imdbRatings.onTmdbResolved)
  eventBus.subscribe(imdbRatings.onImdbIdResolved)
  eventBus.subscribe(rottenTomatoesRatings.onTmdbResolved)
  eventBus.subscribe(rottenTomatoesRatings.onImdbIdMissing)
  eventBus.subscribe(metascoreRatings.onTmdbResolved)
  eventBus.subscribe(metascoreRatings.onImdbIdMissing)
  eventBus.subscribe(filmwebRatings.onTmdbResolved)
  eventBus.subscribe(filmwebRatings.onImdbIdMissing)
  eventBus.subscribe(kinoMuzaSynopsisRefresher.onCinemaMovieAdded)

  def start(): Unit = {
    // Force Mongo at boot so connection errors surface in the boot timeline.
    mongoConnection.database
    movieCache.start()
    movieService.start()
    imdbRatings.start()
    rottenTomatoesRatings.start()
    metascoreRatings.start()
    filmwebRatings.start()
    unscreenedCleanup.start()
    kinoMuzaSynopsisRefresher.start()
    if (queueScraping) {
      taskWorker.start()
      scrapeReaper.start()
    } else {
      showtimeCache.start()
    }
  }

  /** Event-cascade drain order, producer→consumer (see monolith comment). */
  def cascadeDrainOrder: Seq[Stoppable] = Seq(
    movieService, imdbIdResolver, imdbRatings,
    rottenTomatoesRatings, metascoreRatings, filmwebRatings
  )

  def stop(): Unit = {
    if (queueScraping) {
      scrapeReaper.stop()
      taskWorker.stop()
      taskQueue.close()
      freshnessStore.close()
    } else {
      showtimeCache.stop()
    }
    cascadeDrainOrder.foreach(_.stop())
    unscreenedCleanup.stop()
    kinoMuzaSynopsisRefresher.stop()
    movieCache.stop()
    movieRepo.close()
    mongoConnection.close()
  }
}
