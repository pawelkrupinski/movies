package modules

import clients.TmdbClient
import models.{Cinema, City}
import services.{MongoCachingDetailFetch, MongoConnection, Stoppable, UptimeMonitor}
import services.alerts.{FallbackAlert, FilmwebDropAlerter, TelegramNotifier}
import services.cinemas._
import services.enrichment._
import services.fallback.{FallbackEvent, FilmwebFallbackState, FilmwebFallbackStore, MongoFilmwebFallbackStore}
import services.events.{EventBus, InProcessEventBus, MovieRecordCreated}
import services.freshness.{FreshnessKind, FreshnessStore, MongoFreshnessStore}
import services.movies.{CaffeineMovieCache, MongoMovieRepo, MovieRepo, MovieService, MongoNormalizationReportRepo, NormalizationRebuilder, NormalizationReport, NormalizationReportRepo, UnscreenedCleanup}
import services.readmodel.{MongoReadModelRepo, ReadModelProjector, ReadModelReader, ReadModelWriter}
import services.tasks.{BulkRefreshHandler, DetailReaper, DetailTaskEnqueuer, EnrichDetailsHandler, EnrichmentReaper, MongoTaskQueue, RatingEnqueuer, RatingHandler, ResolveTmdbHandler, ScrapeCinemaHandler, ScrapeReaper, TaskQueue, TaskType, TaskWorker, WorkerHeartbeat}
import services.titlerules.{MongoTitleRulesRepo, TitleRuleSet, TitleRulesCache, TitleRulesRepo}
import tools.{Env, HttpFetch, MonitoringHttpFetch, RealHttpFetch, ScrapeCities, SharedExecutionBudget}

import scala.concurrent.duration.DurationLong

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
  // (group, film), multi-server-safe) instead of inline in fetch(). Off by
  // default so prod is unchanged until cutover.
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

  // The per-cinema scraper-client marker ("shared:<Client>" / "custom:<Client>"),
  // derived once from the catalog. Shared by the boot reconcile and the per-event
  // retag so the FtFW tag is layered on top of — never instead of — the marker.
  protected lazy val clientMarkers: Map[String, String] =
    CinemaClientMarkers.markers(cinemaScraperCatalog.all)

  // The per-cinema public source-page URL ("url:<https…>"), derived once from
  // the catalog alongside the client marker so the /uptime page can link each
  // cinema name to the page we scrape. Rides the same tag channel. Filmweb-backed
  // venues are upgraded from their `/cinema/-<id>` fallback to the canonical,
  // browser-renderable `/showtimes/<City>/<Name>-<id>` page, resolved once from
  // Filmweb's /info at boot (city + name aren't in our model); tolerant, so a
  // venue whose resolve fails keeps the fallback.
  protected lazy val sourceUrls: Map[String, String] = {
    val base           = CinemaClientMarkers.sourceUrls(cinemaScraperCatalog.all)
    val filmwebClients = cinemaScraperCatalog.all.collect { case f: FilmwebShowtimesClient => f }
    base ++ FilmwebShowtimesClient.resolveAll(filmwebClients)
  }

  // Fired on each ENTER / PROBE_FAILED / RECOVERED transition: alerts on the
  // page-worthy ones (FallbackAlert filters PROBE_FAILED out) when Telegram is
  // configured, and (re)writes the cinema's /uptime tags so the FtFW chip appears
  // on ENTER and clears on RECOVER. `state.active` is the post-transition truth
  // (put() runs before onEvent), so no store round-trip is needed.
  protected def filmwebFallbackOnEvent: (FilmwebFallbackState, FallbackEvent) => Unit =
    (state, event) => {
      FallbackAlert.messageFor(state, event).foreach(msg => fallbackTelegramNotifier.foreach(_.send(msg)))
      uptimeMonitor.tagService(state.cinema, CinemaClientMarkers.tagsFor(clientMarkers.get(state.cinema), sourceUrls.get(state.cinema), state.active))
    }

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

  // ── Denormalised read model (web_movies + web_screenings) ───────────────────
  // The worker projects every `movies` write into the two read-model collections
  // the serving app consumes. One impl is both reader (boot-seed the diff state)
  // and writer (upsert/delete the derived docs).
  // Typed as the read+write intersection so test wirings can swap in
  // `InMemoryReadModelRepo` (Mongo-free fixture replay).
  lazy val readModelRepo: ReadModelReader & ReadModelWriter = new MongoReadModelRepo(mongoConnection.database)
  lazy val readModelProjector = new ReadModelProjector(movieRepo, readModelRepo, readModelRepo)

  // Title-stripping rules. The worker owns seeding: a fresh DB gets the migrated
  // defaults so behaviour is unchanged from the hardcoded baseline. When an edit
  // arrives over the change stream, re-merge existing records so the rule applies
  // retroactively, not just to future scrapes.
  lazy val normalizationRebuilder = new NormalizationRebuilder(movieCache,
    onSplitOff = (title, year) => eventBus.publish(MovieRecordCreated(title, year)))
  lazy val normalizationReportRepo: NormalizationReportRepo =
    new MongoNormalizationReportRepo(mongoConnection.database, fallbackToOwnInit = false)
  lazy val titleRulesRepo: TitleRulesRepo = new MongoTitleRulesRepo(mongoConnection.database, fallbackToOwnInit = false)
  lazy val titleRulesCache: TitleRulesCache =
    new TitleRulesCache(titleRulesRepo, seedIfEmpty = true,
      onRulesChanged = (oldRules, newRules) => {
        // Merge-key changes (per-cinema / structural / canonical) → re-merge /
        // un-merge existing rows.
        val result = normalizationRebuilder.rebuild()
        // Search-tier changes → re-resolve the rows whose upstream query moved.
        val reEnriched = normalizationRebuilder.reEnrichSearchChanges(
          TitleRuleSet(oldRules), TitleRuleSet(newRules),
          (title, year) => eventBus.publish(MovieRecordCreated(title, year)))
        // Publish the realized outcome so the admin editor can show what happened.
        normalizationReportRepo.writeLatest(
          NormalizationReport.render(result, reEnriched, System.currentTimeMillis()))
      })

  // The *Ratings classes refresh synchronously (the queue's RatingHandler / the
  // operator bulk walk), so they own no EC — only imdbIdResolver still runs
  // async off the bus and draws a shared-budget EC.
  lazy val imdbRatings = new ImdbRatings(movieCache, imdbClient)
  lazy val imdbIdResolver = new ImdbIdResolver(movieCache, imdbClient, eventBus, backgroundBudget.ec("imdb-id-resolver"))
  lazy val rottenTomatoesRatings = new RottenTomatoesRatings(movieCache, tmdbClient, rottenTomatoesClient)
  lazy val metascoreRatings = new MetascoreRatings(movieCache, tmdbClient, metacriticClient)
  lazy val filmwebRatings = new FilmwebRatings(movieCache, tmdbClient, filmwebClient)
  // How many times the TMDB stage retries a transient failure. Overridden to 0
  // by the fixture-replay test wiring (see TestWiring) so permanent fixture
  // misses don't churn the cascade.
  def tmdbMaxRetries: Int = 6
  lazy val movieService = new MovieService(movieCache, eventBus, tmdbClient, backgroundBudget.ec("enrichment-worker"), maxRetries = tmdbMaxRetries)
  lazy val unscreenedCleanup = new UnscreenedCleanup(movieCache)
  lazy val kinoMuzaSynopsisRefresher = new KinoMuzaSynopsisRefresher(movieCache, kinoMuzaClient, httoFetch)

  // ── Task queue (scrape scheduling) ──────────────────────────────────────────
  // Hold the first scrape back from boot so the cold-boot scrape burst doesn't
  // pile onto the cache hydrate and drain the shared-CPU credit balance to zero.
  // The ScrapeReaper's first tick enqueues every stale cinema (all of them on a
  // cold boot) for the TaskWorker to drain at once.
  def initialScrapeDelaySeconds: Long = Env.positiveLong("KINOWO_SCRAPE_INITIAL_DELAY_SECONDS", 45L)

  // Cinema scraping is driven by a durable Mongo task queue: the ScrapeReaper
  // enqueues each cinema at most once per freshness window, and the TaskWorker
  // scrapes it (skipping if a concurrent run already refreshed it). Detail and
  // rating enrichment are governed by KINOWO_DEFERRED_DETAIL and
  // KINOWO_QUEUE_ENRICHMENT independently.

  // Rating refresh (IMDb/Filmweb/RT/Metacritic) runs as freshness-gated queue
  // tasks — deduped and shared across servers. TMDB / IMDb-id RESOLUTION stays
  // inline (it's one-shot per scraped row, already driven by the queue-gated
  // scrape).
  lazy val taskQueue: TaskQueue = new MongoTaskQueue(mongoConnection.database)
  lazy val freshnessStore: FreshnessStore = new MongoFreshnessStore(mongoConnection.database)

  // Cinemas that defer their per-film detail (implement DetailEnricher), wired
  // only when deferDetail is on — otherwise empty, so no enqueuers/reaper run and
  // clients enrich inline. Indexed by detailGroup for the handler (task → fetch);
  // the per-cinema enqueuers and reaper iterate the list directly.
  lazy val detailEnrichers: Seq[DetailEnricher] =
    if (deferDetail) cinemaScraperCatalog.all.collect { case de: DetailEnricher => de } else Nil

  // The shared scrape core: record + publish, injected into ScrapeCinemaHandler.
  // Detail enqueue is no longer here — it's event-driven (DetailTaskEnqueuer off
  // CinemaMovieAdded) plus the DetailReaper backstop.
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

  // Rating refresh as queue tasks. The handlers reuse each *Ratings class's
  // per-row refreshOneSync; the enqueuer turns the resolution bus events into
  // rating tasks; the reaper is the periodic (staggered 4h) backstop.
  lazy val ratingEnqueuer = new RatingEnqueuer(movieCache, taskQueue)
  lazy val ratingHandlers: Seq[services.tasks.TaskHandler] = Seq(
    new RatingHandler(TaskType.ImdbRating,    FreshnessKind.ImdbRating,    freshnessStore, imdbRatings.refreshOneSync),
    new RatingHandler(TaskType.FilmwebRating, FreshnessKind.FilmwebRating, freshnessStore, filmwebRatings.refreshOneSync),
    new RatingHandler(TaskType.RtRating,      FreshnessKind.RtRating,      freshnessStore, rottenTomatoesRatings.refreshOneSync),
    new RatingHandler(TaskType.McRating,      FreshnessKind.McRating,      freshnessStore, metascoreRatings.refreshOneSync)
  )
  lazy val enrichmentReaper = new EnrichmentReaper(movieCache, taskQueue, freshnessStore,
    bootSweepDelaySeconds = Env.positiveLong("KINOWO_ENRICHMENT_BOOT_SWEEP_DELAY_SECONDS",
      EnrichmentReaper.DefaultBootSweepDelaySeconds))

  // Operator-triggered handlers — ALWAYS registered (not gated by
  // queueEnrichment): the web `/tasks` buttons enqueue a corpus-wide refresh and
  // the `/debug` row button enqueues a per-movie re-resolve, regardless of which
  // enrichment mode the worker runs. The bulk handlers call each source's
  // existing refreshAll / retryUnresolvedTmdb; ResolveTmdb forces one row and
  // lets the event chain re-run the downstream ratings.
  lazy val operatorHandlers: Seq[services.tasks.TaskHandler] = Seq(
    new BulkRefreshHandler(TaskType.RefreshAllTmdb,       "TMDB",       () => movieService.retryUnresolvedTmdb()),
    new BulkRefreshHandler(TaskType.RefreshAllImdb,       "IMDb",       () => imdbRatings.refreshAllNow()),
    new BulkRefreshHandler(TaskType.RefreshAllFilmweb,    "Filmweb",    () => filmwebRatings.refreshAllNow()),
    new BulkRefreshHandler(TaskType.RefreshAllMetacritic, "Metacritic", () => metascoreRatings.refreshAllNow()),
    new BulkRefreshHandler(TaskType.RefreshAllRt,         "RT",         () => rottenTomatoesRatings.refreshAllNow()),
    new ResolveTmdbHandler(movieService.reenrichTmdbSync)
  )

  // A fixed pool of workers, each fetching and running ONE task at a time — so
  // the number of scrapes/enrichments in flight at once is hard-capped at the
  // pool size and a backlog can't peg the box. (Replaces the old single batch
  // poller that claimed up to 20 tasks per tick onto a shared-budget EC.)
  def workerPoolSize: Int = Env.positiveInt("KINOWO_WORKER_POOL_SIZE", 4)
  lazy val taskWorker = new TaskWorker(
    taskQueue, Seq(scrapeCinemaHandler, enrichDetailsHandler) ++ ratingHandlers ++ operatorHandlers,
    poolSize = workerPoolSize
  )
  lazy val scrapeReaper =
    new ScrapeReaper(cinemaScrapers, taskQueue, freshnessStore, initialDelay = initialScrapeDelaySeconds.seconds)
  // Logs queue depth every minute so a CPU-credit/steal episode can be correlated
  // with the scrape/enrich backlog that drove it (the diagnostic that was missing
  // when the 2026-06-12 worker-steal episode had to be reconstructed from metrics).
  lazy val workerHeartbeat = new WorkerHeartbeat(taskQueue)

  // Subscribe BEFORE start() so the bus's first MovieRecordCreated events reach
  // the enrichment handlers. (See the original monolith comment block for the
  // full event-cascade rationale — the wiring is unchanged.)
  //   MovieRecordCreated → movieService           (TMDB stage)
  //   TmdbResolved       → ratingEnqueuer          (enqueue IMDb/RT/MC/Filmweb)
  //   ImdbIdMissing      → imdbIdResolver + ratingEnqueuer (TMDB-only hits)
  //   ImdbIdResolved     → ratingEnqueuer          (enqueue IMDb)
  //   CinemaMovieAdded   → kinoMuzaSynopsisRefresher
  // Resolution stays inline (one-shot per scraped row).
  eventBus.subscribe(movieService.onMovieRecordCreated)
  eventBus.subscribe(imdbIdResolver.onImdbIdMissing)
  eventBus.subscribe(kinoMuzaSynopsisRefresher.onCinemaMovieAdded)
  // One detail enqueuer per deferred cinema (empty when !deferDetail).
  detailEnqueuers.foreach(e => eventBus.subscribe(e.onCinemaMovieAdded))
  // Rating subscribers ENQUEUE tasks; RatingHandlers + EnrichmentReaper fetch.
  eventBus.subscribe(ratingEnqueuer.onTmdbResolved)
  eventBus.subscribe(ratingEnqueuer.onImdbIdResolved)
  eventBus.subscribe(ratingEnqueuer.onImdbIdMissing)

  def start(): Unit = {
    // Force Mongo at boot so connection errors surface in the boot timeline.
    mongoConnection.database
    // Seed + install title rules before the cache hydrates so scrape/merge keys
    // are computed with the active rules from the very first tick.
    titleRulesCache.start()
    // Boot ordering, tuned to not drain the shared-CPU credit balance on a cold
    // JVM: the cache hydrate (synchronous findAll — the first scrape tick needs a
    // populated cache for sibling/redirect checks) and the projector's state seed
    // run at boot, but the two heaviest jobs are deferred off the boot window —
    // the first scrape pass (KINOWO_SCRAPE_INITIAL_DELAY_SECONDS) and the
    // projector's full reconcile (KINOWO_READMODEL_RECONCILE_BOOT_DELAY_SECONDS).
    movieCache.start()
    // Start the read-model projector after the cache so its state seed (and the
    // first deferred reconcile) read a hydrated `movies` collection; it watches the
    // change stream independently of the cache's own watch.
    readModelProjector.start()
    movieService.start()
    // Ratings refresh via the queue (RatingHandlers + the EnrichmentReaper
    // backstop); refreshOneSync, which the handlers call, needs no start().
    unscreenedCleanup.start()
    kinoMuzaSynopsisRefresher.start()
    // Publish the by-design Filmweb-only cinemas so the /uptime/fallback page can
    // list them (the catalog is worker-only; the web reads this from Mongo).
    filmwebFallbackStore.putFilmwebOnly(filmwebOnlyCinemas)
    // Tag each cinema with its scraper-client marker (shared platform client vs a
    // bespoke one) plus the FtFW chip if it's already in Filmweb fallback at boot
    // (transitions only fire on change, so an in-flight fallback would otherwise go
    // untagged until it next flips). Same rationale as above — the catalog is
    // worker-only, so the tags ride the UptimeMonitor tag channel.
    clientMarkers.foreach { case (cinema, marker) =>
      val inFallback = filmwebFallbackStore.get(cinema).exists(_.active)
      uptimeMonitor.tagService(cinema, CinemaClientMarkers.tagsFor(Some(marker), sourceUrls.get(cinema), inFallback))
    }
    // The task worker drains all queue work: scraping, deferred detail, and/or
    // queue-driven rating enrichment.
    taskWorker.start(); workerHeartbeat.start()
    enrichmentReaper.start()
    if (deferDetail) detailReaper.start()
    scrapeReaper.start()
  }

  /** Event-cascade drain order, producer→consumer (see monolith comment). Only
   *  the async stages need draining: the TMDB stage and the IMDb-id resolver.
   *  Rating refresh is synchronous (queue-driven), so the *Ratings own no pool. */
  def cascadeDrainOrder: Seq[Stoppable] = Seq(movieService, imdbIdResolver)

  def stop(): Unit = {
    scrapeReaper.stop()
    enrichmentReaper.stop()
    if (deferDetail) detailReaper.stop()
    workerHeartbeat.stop()
    taskWorker.stop()
    taskQueue.close()
    freshnessStore.close()
    cascadeDrainOrder.foreach(_.stop())
    unscreenedCleanup.stop()
    kinoMuzaSynopsisRefresher.stop()
    readModelProjector.stop()
    movieCache.stop()
    titleRulesCache.stop()
    readModelRepo.close()
    movieRepo.close()
    titleRulesRepo.close()
    mongoConnection.close()
  }
}
