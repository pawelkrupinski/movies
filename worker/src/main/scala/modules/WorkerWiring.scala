package modules

import clients.TmdbClient
import models.{Cinema, City}
import services.{MongoCachingDetailFetch, MongoConnection, ShowtimeCache, Stoppable, UptimeMonitor}
import services.alerts.{FallbackAlert, FilmwebDropAlerter, TelegramNotifier}
import services.cinemas._
import services.enrichment._
import services.fallback.{FallbackEvent, FilmwebFallbackState, FilmwebFallbackStore, MongoFilmwebFallbackStore}
import services.events.{EventBus, InProcessEventBus}
import services.freshness.{FreshnessKind, FreshnessStore, MongoFreshnessStore}
import services.movies.{CaffeineMovieCache, MongoMovieRepo, MovieRepo, MovieService, UnscreenedCleanup}
import services.tasks.{DetailReaper, DetailTaskEnqueuer, EnrichDetailsHandler, EnrichmentReaper, MongoTaskQueue, RatingEnqueuer, RatingHandler, ScrapeCinemaHandler, ScrapeReaper, TaskQueue, TaskType, TaskWorker}
import services.titlerules.{MongoTitleRulesRepo, TitleRulesCache, TitleRulesRepo}
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
  // wraps each raw scraper in RetryingCinemaScraper (retry) + UptimeRecordingScraper
  // (record the outcome) for production ticks.
  lazy val multikinoFetch: HttpFetch = MultikinoClient.fetchFor(httoFetch)
  // biletyna.pl 403s our datacenter IP; route Kino Kameralne through Zyte.
  lazy val biletynaFetch: HttpFetch = ZyteFallback.fetchFor(httoFetch)
  lazy val cinemaScraperCatalog = new CinemaScraperCatalog(
    httoFetch, multikinoFetch, biletynaFetch, heliosToday, deferDetail,
    // Mongo-backed chain detail cache so Helios / Cinema City detail is deduped
    // across worker servers, not just within one process.
    (h, ttl) => new MongoCachingDetailFetch(h, mongoConnection.database, ttl))
  def kinoMuzaClient: KinoMuzaClient = cinemaScraperCatalog.kinoMuzaClient

  // When true, cinemas that implement DetailEnricher scrape BARE and their
  // per-film detail is fetched via EnrichDetails queue tasks (deduped per
  // (group, film), multi-server-safe) instead of inline in fetch(). Independent
  // of KINOWO_QUEUE_SCRAPING: detail enrichment is queue-based in either scrape
  // mode. Off by default so prod is unchanged until cutover.
  protected def deferDetail: Boolean =
    Env.get("KINOWO_DEFERRED_DETAIL").exists(v => v == "true" || v == "1")

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

  // ── Filmweb fallback ────────────────────────────────────────────────────────
  // Each non-chain venue whose own scraper throws or comes back empty is served
  // from Filmweb instead (FilmwebFallbackScraper), and the swap is recorded for
  // the /uptime/fallback page. Each cinema's Filmweb id is resolved once (one GET
  // per Filmweb-listed city), guarded so a network/resolver failure yields no
  // fallback rather than a boot failure; cinemas Filmweb doesn't list simply have
  // no fallback available. Test wirings pin this empty so fixture replay never
  // resolves or fetches Filmweb live (see TestWiring).
  protected lazy val filmwebFallbackIds: Map[Cinema, Int] =
    scala.util.Try(new FilmwebCinemaIdResolver(httoFetch).resolveAll())
      .toOption.getOrElse(Nil)
      .collect { case r if r.resolved => r.cinema -> r.filmwebId.get }
      .toMap

  protected def filmwebFallbackFor(cinema: Cinema): Option[CinemaScraper] =
    filmwebFallbackIds.get(cinema).map(id =>
      new FilmwebShowtimesClient(httoFetch, id, cinema, today = heliosToday))

  lazy val filmwebFallbackStore: FilmwebFallbackStore =
    new MongoFilmwebFallbackStore(mongoConnection.database)

  // Telegram alerter for fallback ENTER / RECOVERED events, wired only when the
  // bot token + chat id are configured (absent in CI / local without secrets →
  // no alerts). Posts to the dedicated "Fallback to Filmweb" topic when a topic
  // id is set. See reference_fallback_telegram_channel.
  protected lazy val fallbackTelegramNotifier: Option[TelegramNotifier] = for {
    token  <- Env.get("TELEGRAM_BOT_TOKEN")
    chatId <- Env.get("KINOWO_FALLBACK_TG_CHAT_ID").flatMap(s => scala.util.Try(s.toLong).toOption)
  } yield new TelegramNotifier(httoFetch, token, chatId,
    Env.get("KINOWO_FALLBACK_TG_TOPIC_ID").flatMap(s => scala.util.Try(s.toLong).toOption))

  // Fired on each ENTER / PROBE_FAILED / RECOVERED transition; alerts on the
  // page-worthy ones (FallbackAlert filters PROBE_FAILED out) when Telegram is
  // configured.
  protected def filmwebFallbackOnEvent: (FilmwebFallbackState, FallbackEvent) => Unit =
    (state, event) => FallbackAlert.messageFor(state, event).foreach(msg => fallbackTelegramNotifier.foreach(_.send(msg)))

  // Cinemas whose ONLY scraper is a FilmwebShowtimesClient — served by Filmweb by
  // design, not as a fallback. Published to Mongo at start() for the status page.
  lazy val filmwebOnlyCinemas: Set[String] =
    cinemaScraperCatalog.all.groupBy(_.cinema)
      .collect { case (c, scrapers) if scrapers.nonEmpty && scrapers.forall(_.isInstanceOf[FilmwebShowtimesClient]) =>
        c.displayName }
      .toSet

  // Telegram alerter for the OTHER half of the Filmweb story: a venue whose sole
  // source IS Filmweb (no own-site fallback possible) going empty/404 because
  // Filmweb dropped it — a nudge to migrate it to an own-site scraper. Posts to
  // the dedicated "Filmweb Drops Cinemas" channel; off unless its chat id is set,
  // so CI / local without secrets raise no alerts. See reference_fallback_telegram_channel.
  protected lazy val filmwebDropAlerter: Option[FilmwebDropAlerter] = for {
    token  <- Env.get("TELEGRAM_BOT_TOKEN")
    chatId <- Env.get("KINOWO_FILMWEB_DROP_TG_CHAT_ID").flatMap(s => scala.util.Try(s.toLong).toOption)
  } yield {
    val notifier = new TelegramNotifier(httoFetch, token, chatId,
      Env.get("KINOWO_FILMWEB_DROP_TG_TOPIC_ID").flatMap(s => scala.util.Try(s.toLong).toOption))
    new FilmwebDropAlerter(filmwebOnlyCinemas, notifier.send,
      Env.positiveInt("KINOWO_FILMWEB_DROP_THRESHOLD", 3))
  }

  // The single drop-watcher shared across every UptimeRecordingScraper wrap (it
  // self-filters to the Filmweb-only venues), or a no-op when unconfigured.
  protected lazy val scrapeOutcomeListener: ScrapeOutcomeListener =
    filmwebDropAlerter.getOrElse(ScrapeOutcomeListener.NoOp)

  lazy val cinemaScrapers: Seq[CinemaScraper] =
    City.all
      .filter(c => scrapeCities(c.slug))
      .flatMap(c => cinemaScraperCatalog.byCity.getOrElse(c.slug, Nil))
      .map { raw =>
        val retried = new RetryingCinemaScraper(raw, maxAttempts = math.min(raw.maxFetchAttempts, scrapeAttemptCeiling))
        if (FallbackEligibility.eligible(raw))
          new FilmwebFallbackScraper(
            retried,
            () => filmwebFallbackFor(raw.cinema),
            () => filmwebFallbackIds.get(raw.cinema),
            uptimeMonitor,
            filmwebFallbackStore,
            onEvent = filmwebFallbackOnEvent)
        else
          new UptimeRecordingScraper(retried, uptimeMonitor, scrapeOutcomeListener)
      }

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

  // Title-stripping rules. The worker owns seeding: a fresh DB gets the migrated
  // defaults so behaviour is unchanged from the hardcoded baseline.
  lazy val titleRulesRepo: TitleRulesRepo = new MongoTitleRulesRepo(mongoConnection.database, fallbackToOwnInit = false)
  lazy val titleRulesCache: TitleRulesCache = new TitleRulesCache(titleRulesRepo, seedIfEmpty = true)

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
    cinemaScrapers, eventBus, movieCache, showtimeFetchEc, runner = Some(cinemaScrapeRunner)
  )

  // ── Task queue (scrape scheduling) ──────────────────────────────────────────
  // When enabled, cinema scraping is driven by a durable Mongo task queue instead
  // of ShowtimeCache's continuous loop: a reaper enqueues each cinema at most once
  // per 15-min freshness window, and the worker scrapes it (skipping if a
  // concurrent run already refreshed it). This flag changes only the scrape
  // *scheduling*; detail and rating enrichment are governed by KINOWO_DEFERRED_DETAIL
  // and KINOWO_QUEUE_ENRICHMENT independently. Off by default until cut over.
  protected def queueScraping: Boolean =
    Env.get("KINOWO_QUEUE_SCRAPING").exists(v => v == "true" || v == "1")

  // When enabled, rating refresh (IMDb/Filmweb/RT/Metacritic) runs as freshness-
  // gated queue tasks instead of the in-process bus-fetch + 4h cache walks — so
  // ratings are deduped and shared across servers. TMDB / IMDb-id RESOLUTION
  // stays inline (it's one-shot per scraped row, already driven by the queue-
  // gated scrape). Off by default; KINOWO_QUEUE_ENRICHMENT=true flips it.
  protected def queueEnrichment: Boolean =
    Env.get("KINOWO_QUEUE_ENRICHMENT").exists(v => v == "true" || v == "1")

  lazy val taskQueue: TaskQueue = new MongoTaskQueue(mongoConnection.database)
  lazy val freshnessStore: FreshnessStore = new MongoFreshnessStore(mongoConnection.database)

  // Cinemas that defer their per-film detail (implement DetailEnricher), wired
  // only when deferDetail is on — otherwise empty, so no enqueuers/reaper run and
  // clients enrich inline. Indexed by detailGroup for the handler (task → fetch);
  // the per-cinema enqueuers and reaper iterate the list directly.
  lazy val detailEnrichers: Seq[DetailEnricher] =
    if (deferDetail) cinemaScraperCatalog.all.collect { case de: DetailEnricher => de } else Nil

  // The shared scrape core: record + publish. Injected into BOTH ShowtimeCache
  // and ScrapeCinemaHandler. Detail enqueue is no longer here — it's event-driven
  // (DetailTaskEnqueuer off CinemaMovieAdded) plus the DetailReaper backstop.
  lazy val cinemaScrapeRunner = new CinemaScrapeRunner(movieCache, eventBus)

  lazy val scrapeCinemaHandler = new ScrapeCinemaHandler(
    cinemaScrapers.map(s => ScrapeCinemaHandler.scraperKey(s.cinema) -> s).toMap,
    cinemaScrapeRunner, freshnessStore
  )
  lazy val enrichDetailsHandler = new EnrichDetailsHandler(
    detailEnrichers.map(de => de.detailGroup -> de).toMap, movieCache, freshnessStore, uptimeMonitor
  )
  // Detail enqueue is event-driven: one enqueuer per deferred cinema fires the
  // first detail fetch off CinemaMovieAdded; the reaper is the periodic
  // refresh/retry backstop (CinemaMovieAdded fires only on first appearance).
  // Both empty/no-op when !deferDetail, since detailEnrichers is then Nil.
  lazy val detailEnqueuers: Seq[DetailTaskEnqueuer] =
    detailEnrichers.map(de => new DetailTaskEnqueuer(de, movieCache, taskQueue, freshnessStore))
  lazy val detailReaper = new DetailReaper(detailEnrichers, movieCache, taskQueue, freshnessStore)

  // Rating refresh as queue tasks (wired only when queueEnrichment). The handlers
  // reuse each *Ratings class's existing per-row refreshOneSync; the enqueuer
  // turns the resolution bus events into rating tasks; the reaper is the periodic
  // (staggered 4h) backstop that replaces the 4h cache walks.
  lazy val ratingEnqueuer = new RatingEnqueuer(movieCache, taskQueue)
  lazy val ratingHandlers: Seq[services.tasks.TaskHandler] =
    if (queueEnrichment) Seq(
      new RatingHandler(TaskType.ImdbRating,    FreshnessKind.ImdbRating,    freshnessStore, imdbRatings.refreshOneSync),
      new RatingHandler(TaskType.FilmwebRating, FreshnessKind.FilmwebRating, freshnessStore, filmwebRatings.refreshOneSync),
      new RatingHandler(TaskType.RtRating,      FreshnessKind.RtRating,      freshnessStore, rottenTomatoesRatings.refreshOneSync),
      new RatingHandler(TaskType.McRating,      FreshnessKind.McRating,      freshnessStore, metascoreRatings.refreshOneSync)
    ) else Nil
  lazy val enrichmentReaper = new EnrichmentReaper(movieCache, taskQueue, freshnessStore)

  // A fixed pool of workers, each fetching and running ONE task at a time — so
  // the number of scrapes/enrichments in flight at once is hard-capped at the
  // pool size and a backlog can't peg the box. (Replaces the old single batch
  // poller that claimed up to 20 tasks per tick onto a shared-budget EC.)
  def workerPoolSize: Int = Env.positiveInt("KINOWO_WORKER_POOL_SIZE", 4)
  lazy val taskWorker = new TaskWorker(
    taskQueue, Seq(scrapeCinemaHandler, enrichDetailsHandler) ++ ratingHandlers,
    poolSize = workerPoolSize
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
  // Resolution stays inline in both modes (one-shot per scraped row).
  eventBus.subscribe(movieService.onMovieRecordCreated)
  eventBus.subscribe(imdbIdResolver.onImdbIdMissing)
  eventBus.subscribe(kinoMuzaSynopsisRefresher.onCinemaMovieAdded)
  // One detail enqueuer per deferred cinema (empty when !deferDetail).
  detailEnqueuers.foreach(e => eventBus.subscribe(e.onCinemaMovieAdded))
  if (queueEnrichment) {
    // Rating subscribers ENQUEUE tasks; RatingHandlers + EnrichmentReaper fetch.
    eventBus.subscribe(ratingEnqueuer.onTmdbResolved)
    eventBus.subscribe(ratingEnqueuer.onImdbIdResolved)
    eventBus.subscribe(ratingEnqueuer.onImdbIdMissing)
  } else {
    // Legacy: rating subscribers fetch inline (+ 4h cache walks started below).
    eventBus.subscribe(imdbRatings.onTmdbResolved)
    eventBus.subscribe(imdbRatings.onImdbIdResolved)
    eventBus.subscribe(rottenTomatoesRatings.onTmdbResolved)
    eventBus.subscribe(rottenTomatoesRatings.onImdbIdMissing)
    eventBus.subscribe(metascoreRatings.onTmdbResolved)
    eventBus.subscribe(metascoreRatings.onImdbIdMissing)
    eventBus.subscribe(filmwebRatings.onTmdbResolved)
    eventBus.subscribe(filmwebRatings.onImdbIdMissing)
  }

  def start(): Unit = {
    // Force Mongo at boot so connection errors surface in the boot timeline.
    mongoConnection.database
    // Seed + install title rules before the cache hydrates so scrape/merge keys
    // are computed with the active rules from the very first tick.
    titleRulesCache.start()
    movieCache.start()
    movieService.start()
    // The *Ratings 4h cache walks run only in legacy mode; the EnrichmentReaper
    // replaces them when ratings are queue-driven. (refreshOneSync, which the
    // rating handlers call, needs no start().)
    if (!queueEnrichment) {
      imdbRatings.start()
      rottenTomatoesRatings.start()
      metascoreRatings.start()
      filmwebRatings.start()
    }
    unscreenedCleanup.start()
    kinoMuzaSynopsisRefresher.start()
    // Publish the by-design Filmweb-only cinemas so the /uptime/fallback page can
    // list them (the catalog is worker-only; the web reads this from Mongo).
    filmwebFallbackStore.putFilmwebOnly(filmwebOnlyCinemas)
    // Tag each cinema with its scraper-client marker (shared platform client vs a
    // bespoke one) so the /uptime page can show it. Same rationale as above — the
    // catalog is worker-only, so the marker rides the UptimeMonitor tag channel.
    CinemaClientMarkers.markers(cinemaScraperCatalog.all).foreach {
      case (cinema, tag) => uptimeMonitor.tagService(cinema, Set(tag))
    }
    // The task worker runs whenever there's queue work: queue-driven scraping,
    // deferred detail, and/or queue-driven rating enrichment.
    if (queueScraping || deferDetail || queueEnrichment) taskWorker.start()
    if (queueEnrichment) enrichmentReaper.start()
    if (deferDetail) detailReaper.start()
    if (queueScraping) scrapeReaper.start() else showtimeCache.start()
  }

  /** Event-cascade drain order, producer→consumer (see monolith comment). */
  def cascadeDrainOrder: Seq[Stoppable] = Seq(
    movieService, imdbIdResolver, imdbRatings,
    rottenTomatoesRatings, metascoreRatings, filmwebRatings
  )

  def stop(): Unit = {
    if (queueScraping) scrapeReaper.stop() else showtimeCache.stop()
    if (queueEnrichment) enrichmentReaper.stop()
    if (deferDetail) detailReaper.stop()
    if (queueScraping || deferDetail || queueEnrichment) {
      taskWorker.stop()
      taskQueue.close()
      freshnessStore.close()
    }
    cascadeDrainOrder.foreach(_.stop())
    unscreenedCleanup.stop()
    kinoMuzaSynopsisRefresher.stop()
    movieCache.stop()
    titleRulesCache.stop()
    movieRepo.close()
    titleRulesRepo.close()
    mongoConnection.close()
  }
}
