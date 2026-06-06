package modules

import clients.TmdbClient
import models.City
import services.{MongoConnection, ShowtimeCache, Stoppable, UptimeMonitor}
import services.cinemas._
import services.enrichment._
import services.events.{EventBus, InProcessEventBus}
import services.movies.{CaffeineMovieCache, MongoMovieRepo, MovieRepo, MovieService, UnscreenedCleanup}
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
  lazy val httoFetch: HttpFetch = new MonitoringHttpFetch(new RealHttpFetch(), uptimeMonitor)

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
  // `httoFetch`, the Zyte-routed `multikinoFetch`, and Helios's REST date — and
  // wraps each raw scraper in RetryingCinemaScraper for production ticks.
  lazy val multikinoFetch: HttpFetch = MultikinoClient.fetchFor(httoFetch)
  lazy val cinemaScraperCatalog = new CinemaScraperCatalog(httoFetch, multikinoFetch, heliosToday)
  def kinoMuzaClient: KinoMuzaClient = cinemaScraperCatalog.kinoMuzaClient

  // Scrape only the cities in KINOWO_SCRAPE_CITIES (comma-separated slugs),
  // defaulting to Poznań. `protected def` so test wirings can scrape every city.
  protected def scrapeCities: Set[String] =
    ScrapeCities.enabled(Env.get("KINOWO_SCRAPE_CITIES"), default = Set("poznan"))

  // The date Helios bakes into its REST URLs. Production uses the real Warsaw
  // date; fixture-replay test wirings override with the fixture's capture date.
  protected def heliosToday: java.time.LocalDate =
    java.time.LocalDate.now(java.time.ZoneId.of("Europe/Warsaw"))

  lazy val cinemaScrapers: Seq[CinemaScraper] =
    City.all
      .filter(c => scrapeCities(c.slug))
      .flatMap(c => cinemaScraperCatalog.byCity.getOrElse(c.slug, Nil))
      .map(s => new RetryingCinemaScraper(s, uptimeMonitor))

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
  lazy val movieService = new MovieService(movieCache, eventBus, tmdbClient, backgroundBudget.ec("enrichment-worker"))
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
    showtimeCache.start()
  }

  /** Event-cascade drain order, producer→consumer (see monolith comment). */
  def cascadeDrainOrder: Seq[Stoppable] = Seq(
    movieService, imdbIdResolver, imdbRatings,
    rottenTomatoesRatings, metascoreRatings, filmwebRatings
  )

  def stop(): Unit = {
    showtimeCache.stop()
    cascadeDrainOrder.foreach(_.stop())
    unscreenedCleanup.stop()
    kinoMuzaSynopsisRefresher.stop()
    movieCache.stop()
    movieRepo.close()
    mongoConnection.close()
  }
}
