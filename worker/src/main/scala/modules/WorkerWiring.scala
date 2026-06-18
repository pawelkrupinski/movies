package modules

import clients.TmdbClient
import models.{Cinema, City}
import services.{MongoCachingDetailFetch, MongoConnection, Stoppable, UptimeMonitor}
import services.alerts.{FallbackAlert, FilmwebDropAlerter, StagingStuckAlerter, TelegramNotifier}
import services.cinemas._
import services.enrichment._
import services.fallback.{FallbackEvent, FilmwebFallbackState, FilmwebFallbackStore, MongoFilmwebFallbackStore}
import services.events.{EventBus, InProcessEventBus, MovieDetailsComplete, StagingFilmEnriched, TaskFinished}
import services.freshness.{FreshnessKind, FreshnessStore, MongoFreshnessStore}
import services.movies.{CaffeineMovieCache, MongoMovieRepository, MovieRepository, MovieService, MongoNormalizationReportRepository, NormalizationRebuilder, NormalizationReport, NormalizationReportRepository, UnscreenedCleanup}
import services.readmodel.{MongoReadModelRepository, ReadModelProjector, ReadModelReader, ReadModelWriter}
import services.resolution.{MongoResolutionStore, ResolutionCache, WriteThroughResolutionCache}
import services.schedule.{AlwaysClaimScheduledRunStore, MongoScheduledRunStore, ScheduledRunStore}
import services.tasks.{BulkRefreshHandler, DetailReaper, DetailTaskEnqueuer, EnrichDetailsHandler, EnrichmentReaper, EnrichTaskKeys, MongoTaskQueue, RatingEnqueuer, RatingHandler, ResolveTmdbHandler, ScrapeCinemaHandler, ScrapeReaper, TaskQueue, TaskType, TaskWorker, WorkerHeartbeat}
import services.staging.{MongoStagingFolder, MongoStagingRepository, StagingDetailHandler, StagingFoldHandler, StagingFolder, StagingReaper, StagingRepository, StagingResolveImdbIdHandler, StagingResolveTmdbHandler, StagingSteps}
import services.titlerules.{MongoTitleRulesRepository, TitleRuleSet, TitleRulesCache, TitleRulesRepository}
import tools.{Env, FallbackHttpFetch, HttpFetch, MonitoringHttpFetch, RealHttpFetch, ResidentialProxy, ScrapeCities, SessionWarmingHttpFetch, SharedExecutionBudget, StickyShardHttpFetch}

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration
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
class WorkerWiring extends play.api.Logging {
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
  // Residential-proxy egress (Decodo static-ISP, PL Netia) for the cinema sites
  // that Cloudflare-block our Fly datacenter IP. Non-secret host+ports come from
  // the committed residential-proxy.properties; the KINOWO_PROXY_USER/PASS secrets
  // come from Env (env -> .env.local). Some only when both are present — absent in
  // local/test, where the chain collapses to the existing Zyte/direct path. See
  // the `reference_decodo_isp_proxy` memory.
  // One RealHttpFetch per Decodo pool IP (each pinned, own cookie jar), built
  // once and shared by the proxied clients; None where the KINOWO_PROXY_* secrets
  // aren't set (local/CI/fixture-replay → Zyte/direct). Sharing the shards means
  // each IP warms its Multikino session at most once and reuses it across the
  // venues routed there.
  private lazy val proxyShards: Option[IndexedSeq[RealHttpFetch]] =
    ResidentialProxy.fromEnv().map(_.perPort.map(cfg => new RealHttpFetch(Some(cfg))).toIndexedSeq)

  // Proxy primary → existing chain (Zyte then direct) as fallback, so a proxy IP
  // that's ever unreachable/burned silently rolls over and scraping never breaks.
  //
  // StickyShardHttpFetch fans each client's venues across all the pool IPs, keyed
  // by venue URL so a given venue always egresses via the same IP while different
  // venues spread across the pool. This keeps Multikino off a single pinned IP —
  // notably it avoids getting stuck on `ports.head`, which is a M247 *datacenter*
  // IP that Multikino's Cloudflare blocks — and lets each IP hold its own warmed
  // Multikino session. (It is NOT what cleared the 2026-06-16 "Limit: 3" outage:
  // that was a Decodo auth rejection — the worker's egress IP fell off the
  // whitelist on a machine recreate — fixed account-side, not in this code.)
  //
  // `warmUrl` wraps EACH shard in its own SessionWarmingHttpFetch — for Multikino,
  // whose films API 401s on a cold call from the proxy IP until the homepage warms
  // a session cookie (verified 2026-06-16). Per-venue stickiness means the warm
  // and the API retry share an IP, and each IP warms once then reuses the cookie.
  // Stateless venues (biletyna, ck105) pass None.
  private def proxyPrimary(fallback: HttpFetch, warmUrl: Option[String] = None): HttpFetch =
    proxyShards.fold(fallback) { shards =>
      val legs: IndexedSeq[HttpFetch] =
        warmUrl.fold[IndexedSeq[HttpFetch]](shards)(u => shards.map(new SessionWarmingHttpFetch(_, u)))
      val proxyLeg = new StickyShardHttpFetch(legs)
      new FallbackHttpFetch(Seq("proxy" -> proxyLeg, "fallback" -> fallback), onOutcome = recordProxyOutcome)
    }

  // Meter the residential-proxy leg to /uptime: a green "Residential proxy" bar
  // means the proxy served, a red one means it failed and we fell back to Zyte
  // (so red-bar frequency = how often Zyte is still used). Aggregated across all
  // proxied cinemas into one row. Only the outer chain's "proxy" leg is metered;
  // the inner Zyte/direct chain runs with the default no-op.
  private val ResidentialProxyService = "Residential proxy"
  private def recordProxyOutcome(backend: String, error: Option[String]): Unit =
    if (backend == "proxy") error match {
      case None        => uptimeMonitor.recordSuccess(ResidentialProxyService)
      case Some(label) => uptimeMonitor.recordFailure(ResidentialProxyService, label)
    }

  lazy val multikinoFetch: HttpFetch =
    proxyPrimary(MultikinoClient.fetchFor(httoFetch), warmUrl = Some(MultikinoClient.HomeUrl))
  // Zyte residential egress → direct fallback (Zyte only when ZYTE_API_KEY is set).
  lazy val zyteFetch: HttpFetch = ZyteFallback.fetchFor(httoFetch)
  // biletyna.pl 403s our datacenter IP; residential proxy primary, Zyte fallback.
  lazy val biletynaFetch: HttpFetch = proxyPrimary(zyteFetch)
  lazy val cinemaScraperCatalog = new CinemaScraperCatalog(
    httoFetch, multikinoFetch, biletynaFetch, heliosToday,
    // Mongo-backed chain detail cache so Helios / Cinema City detail is deduped
    // across worker servers, not just within one process.
    (h, ttl) => new MongoCachingDetailFetch(h, mongoConnection.database, ttl),
    // Kino Kryterium (bilety.ck105.koszalin.pl) times out our Fly egress IP AND
    // every Decodo proxy IP at the TCP layer, so a direct scrape came back empty
    // → a permanent white /uptime bar. Only Zyte's true-residential network
    // reaches it, so route it straight through Zyte (the Decodo proxy can't help).
    zyteFetch = zyteFetch)

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
      FallbackAlert.messageFor(state, event).foreach(message => fallbackTelegramNotifier.foreach(_.send(message)))
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
        val served  =
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
        // Drop non-film live events (concerts, stand-up, kabaret, recitals,
        // theatre) from whatever was finally served — own-site OR Filmweb
        // fallback. Outermost so retry/uptime see the raw upstream outcome.
        new NonMovieEventFilteringScraper(served)
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
  lazy val movieRepository: MovieRepository = new MongoMovieRepository(mongoConnection.database, fallbackToOwnInit = false)

  // Staging-ingest: a genuinely-new film incubates in `pending_movies`
  // (resolve-then-fold) instead of landing straight in `movies`; a film already
  // known to `movies` keeps the direct path. The `staging` sink is wired into the
  // cache, the promoter scheduled and the fold subscribed (below) unconditionally.
  lazy val stagingRepository: StagingRepository = new MongoStagingRepository(mongoConnection.database)
  lazy val movieCache: CaffeineMovieCache =
    new CaffeineMovieCache(movieRepository, eventBus, staging = Some(stagingRepository))

  // ── Denormalised read model (web_movies + web_screenings) ───────────────────
  // The worker projects every `movies` write into the two read-model collections
  // the serving app consumes. One impl is both reader (boot-seed the diff state)
  // and writer (upsert/delete the derived documents).
  // Typed as the read+write intersection so test wirings can swap in
  // `InMemoryReadModelRepository` (Mongo-free fixture replay).
  lazy val readModelRepository: ReadModelReader & ReadModelWriter = new MongoReadModelRepository(mongoConnection.database)
  lazy val readModelProjector = new ReadModelProjector(movieRepository, readModelRepository, readModelRepository)

  // Title-stripping rules. The worker owns seeding: a fresh DB gets the migrated
  // defaults so behaviour is unchanged from the hardcoded baseline. When an edit
  // arrives over the change stream, re-merge existing records so the rule applies
  // retroactively, not just to future scrapes.
  lazy val normalizationRebuilder = new NormalizationRebuilder(movieCache,
    onSplitOff = (title, year) => eventBus.publish(MovieDetailsComplete(title, year)))
  lazy val normalizationReportRepository: NormalizationReportRepository =
    new MongoNormalizationReportRepository(mongoConnection.database, fallbackToOwnInit = false)
  lazy val titleRulesRepository: TitleRulesRepository = new MongoTitleRulesRepository(mongoConnection.database, fallbackToOwnInit = false)
  lazy val titleRulesCache: TitleRulesCache =
    new TitleRulesCache(titleRulesRepository, seedIfEmpty = true,
      onRulesChanged = (oldRules, newRules) => {
        // Merge-key changes (per-cinema / structural / canonical) → re-merge /
        // un-merge existing rows.
        val result = normalizationRebuilder.rebuild()
        // Search-tier changes → re-resolve the rows whose upstream query moved.
        val reEnriched = normalizationRebuilder.reEnrichSearchChanges(
          TitleRuleSet(oldRules), TitleRuleSet(newRules),
          (title, year) => eventBus.publish(MovieDetailsComplete(title, year)))
        // Publish the realized outcome so the admin editor can show what happened.
        normalizationReportRepository.writeLatest(
          NormalizationReport.render(result, reEnriched, System.currentTimeMillis()))
      })

  // The *Ratings classes refresh synchronously (the queue's RatingHandler / the
  // operator bulk walk), so they own no EC — only imdbIdResolver still runs
  // async off the bus and draws a shared-budget EC.
  lazy val imdbRatings = new ImdbRatings(movieCache, imdbClient)
  lazy val imdbIdCache: ResolutionCache = resolutionCache("resolve_imdb")
  lazy val imdbIdResolver = new ImdbIdResolver(movieCache, imdbClient, eventBus,
    backgroundBudget.executionContext("imdb-id-resolver"), imdbIdCache = imdbIdCache)
  lazy val rottenTomatoesRatings = new RottenTomatoesRatings(movieCache, tmdbClient, rottenTomatoesClient, resolutionCache("resolve_rt"))
  lazy val metascoreRatings = new MetascoreRatings(movieCache, tmdbClient, metacriticClient, resolutionCache("resolve_mc"))
  lazy val filmwebRatings = new FilmwebRatings(movieCache, tmdbClient, filmwebClient, resolutionCache("resolve_filmweb"))
  // Single-movie TMDB resolution is dispatched as a `ResolveTmdb` worker task:
  // drained by the TaskWorker, retried (`Reschedule`) + deduped by the queue,
  // and shown with a live queue place on `/debug`. `taskQueue` is a lazy val
  // declared below; the closure defers reading it, so there's no init cycle.
  // Hint-keyed resolution caches (Caffeine + per-source Mongo collection, 24h
  // TTL): the same hints resolve once instead of hitting the upstream each cycle.
  // One factory so the test wiring can swap in a passthrough (the fixture harness
  // proves the pipeline is a pure function of the corpus, with no shared cache).
  protected def resolutionCache(collection: String): ResolutionCache =
    new WriteThroughResolutionCache(new MongoResolutionStore(mongoConnection.database, collection))
  lazy val tmdbIdCache: ResolutionCache = resolutionCache("resolve_tmdb")
  lazy val movieService = new MovieService(
    movieCache, eventBus, tmdbClient, backgroundBudget.executionContext("enrichment-worker"),
    enqueueResolveTmdb = Some((title, year, originalTitle, director) => {
      taskQueue.enqueue(
        TaskType.ResolveTmdb,
        EnrichTaskKeys.resolveTmdbDedup(title, year),
        EnrichTaskKeys.resolveTmdbPayload(title, year, director, originalTitle))
      ()
    }),
    tmdbIdCache = tmdbIdCache)
  lazy val unscreenedCleanup = new UnscreenedCleanup(movieCache)

  // ── Task queue (scrape scheduling) ──────────────────────────────────────────
  // Hold the first scrape back from boot so the cold-boot scrape burst doesn't
  // pile onto the cache hydrate and drain the shared-CPU credit balance to zero.
  // The ScrapeReaper's first tick enqueues every stale cinema (all of them on a
  // cold boot) for the TaskWorker to drain at once.
  def initialScrapeDelaySeconds: Long = Env.positiveLong("KINOWO_SCRAPE_INITIAL_DELAY_SECONDS", 45L)

  // Cap on stale cinemas enqueued per reaper tick. A cold boot (or a long backlog)
  // would otherwise queue every cinema at once and let the TaskWorker pool drain
  // flat-out for minutes, exhausting the shared-CPU credit balance — the boot-storm
  // throttle spike. ~25/min drains inside the 1-min tick, leaving idle gaps for
  // credit to recover; the backlog clears over a handful of ticks. Tune down if a
  // restart still throttles, up once Mongo/CPU have headroom.
  def maxScrapeEnqueuePerTick: Int = Env.positiveLong("KINOWO_SCRAPE_MAX_ENQUEUE_PER_TICK", 25L).toInt

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

  // Cluster-wide occurrence claims gate the reapers' recurring ticks so each
  // scheduled occurrence runs on ONE machine (rotating), not on every machine.
  // Absent Mongo (local dev opt-out) → always-claim, i.e. run unlocked.
  lazy val scheduledRunStore: ScheduledRunStore =
    mongoConnection.database
      .map(db => new MongoScheduledRunStore(db.getCollection[org.mongodb.scala.bson.collection.immutable.Document]("scheduled_runs")))
      .getOrElse(AlwaysClaimScheduledRunStore)

  // Cinemas that defer their per-film detail (implement DetailEnricher) scrape
  // BARE; their detail is filled via EnrichDetails queue tasks. Indexed by
  // detailGroup for the handler (task → fetch); the per-cinema enqueuers and
  // reaper iterate the list directly.
  lazy val detailEnrichers: Seq[DetailEnricher] =
    cinemaScraperCatalog.all.collect { case de: DetailEnricher => de }

  /** Cinemas that defer per-film detail AND whose detail supplies TMDB hints —
   *  a film one of these scrapes (with a detail filmUrl) waits for its
   *  EnrichDetails task before TMDB resolution. A display-only enricher
   *  (`defersTmdbResolution = false`, e.g. KinoMuza) still rides the
   *  EnrichDetails pipeline but isn't held back: it resolves from the listing
   *  and merges its synopsis/poster/trailer in asynchronously. */
  lazy val deferredDetailCinemas: Set[models.Cinema] =
    detailEnrichers.filter(_.defersTmdbResolution).map(_.cinema).toSet

  // The shared scrape core: record + decide-trigger, injected into
  // ScrapeCinemaHandler. Detail enqueue is event-driven (DetailTaskEnqueuer off
  // CinemaMovieAdded) plus the DetailReaper backstop; the runner publishes
  // MovieDetailsComplete only for rows that don't await deferred detail.
  lazy val cinemaScrapeRunner = new CinemaScrapeRunner(movieCache, eventBus, deferredDetailCinemas)

  lazy val scrapeCinemaHandler = new ScrapeCinemaHandler(
    cinemaScrapers.map(s => ScrapeCinemaHandler.scraperKey(s.cinema) -> s).toMap,
    cinemaScrapeRunner, freshnessStore
  )
  lazy val enrichDetailsHandler = new EnrichDetailsHandler(
    detailEnrichers.map(de => de.detailGroup -> de).toMap, movieCache, freshnessStore, uptimeMonitor, eventBus
  )
  // Detail enqueue is event-driven: one enqueuer per deferred cinema fires the
  // first detail fetch off CinemaMovieAdded; the reaper is the periodic
  // refresh/retry backstop (CinemaMovieAdded fires only on first appearance).
  lazy val detailEnqueuers: Seq[DetailTaskEnqueuer] =
    detailEnrichers.map(de => new DetailTaskEnqueuer(de, movieCache, taskQueue, freshnessStore))
  lazy val detailReaper = new DetailReaper(detailEnrichers, movieCache, taskQueue, freshnessStore, eventBus,
    runStore = scheduledRunStore)

  // ── Staging incubation (resolve-then-fold) ──────────────────────────────────
  // A newcomer in `pending_movies` walks the SAME steps the direct path runs,
  // but each is now a durable queue task (StagingDetail → StagingResolveTmdb →
  // StagingResolveImdbId → StagingFold) so it retries/backs off + dedups like
  // every other task. `StagingSteps` holds the shared logic (detail-enrich,
  // cache-free `resolveStagingRecord`, IMDb recovery); `StagingReaper` chains the
  // steps (off `TaskFinished`) and is the periodic backstop. On the fold step a
  // `StagingFilmEnriched` event drives the transactional folder, which merges the
  // concluded film into `movies` and deletes its staging rows.
  lazy val stagingFolder: StagingFolder = new MongoStagingFolder(mongoConnection)
  lazy val stagingSteps = new StagingSteps(
    stagingRepository, detailEnrichers, movieService.resolveStagingRecord, imdbIdResolver.findIdFor, freshnessStore)
  lazy val stagingHandlers: Seq[services.tasks.TaskHandler] = Seq(
    new StagingDetailHandler(stagingSteps),
    new StagingResolveTmdbHandler(stagingSteps),
    new StagingResolveImdbIdHandler(stagingSteps),
    new StagingFoldHandler(title => eventBus.publish(StagingFilmEnriched(title)))
  )
  private val StagingReaperInitialDelay = Env.positiveLong("KINOWO_STAGING_PROMOTE_INITIAL_SECONDS", 30L)
  private val StagingReaperInterval     = Env.positiveLong("KINOWO_STAGING_PROMOTE_SECONDS", 120L)
  lazy val stagingReaper = new StagingReaper(stagingSteps, taskQueue, stagingRepository,
    interval     = FiniteDuration(StagingReaperInterval, TimeUnit.SECONDS),
    initialDelay = FiniteDuration(StagingReaperInitialDelay, TimeUnit.SECONDS),
    runStore     = scheduledRunStore)

  // Telegram alerter for newcomers the promoter can't conclude: a row sitting in
  // `pending_movies` TMDB-unresolved for over an hour never folds into `movies`, so
  // it never reaches the app — a silent data hole. Routes to its own chat if set,
  // else the shared "Kinowo Monitoring" group (KINOWO_FALLBACK_TG_CHAT_ID), so it
  // works on prod without a new secret; off in CI / local without any chat id.
  protected lazy val stagingStuckAlerter: Option[StagingStuckAlerter] = for {
    token  <- Env.get("TELEGRAM_BOT_TOKEN")
    chatId <- Env.get("KINOWO_STAGING_STUCK_TG_CHAT_ID")
                .orElse(Env.get("KINOWO_FALLBACK_TG_CHAT_ID"))
                .flatMap(s => scala.util.Try(s.toLong).toOption)
  } yield {
    val notifier = new TelegramNotifier(httoFetch, token, chatId,
      Env.get("KINOWO_STAGING_STUCK_TG_TOPIC_ID").flatMap(s => scala.util.Try(s.toLong).toOption))
    new StagingStuckAlerter(stagingRepository, notifier.send,
      stuckThreshold = FiniteDuration(Env.positiveLong("KINOWO_STAGING_STUCK_MINUTES", 60L), TimeUnit.MINUTES),
      interval       = FiniteDuration(Env.positiveLong("KINOWO_STAGING_STUCK_SCAN_MINUTES", 10L), TimeUnit.MINUTES))
  }

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
      EnrichmentReaper.DefaultBootSweepDelaySeconds),
    runStore = scheduledRunStore)

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
    new ResolveTmdbHandler(movieService.resolveTmdbOnce)
  )

  // A fixed pool of workers, each fetching and running ONE task at a time — so
  // the number of scrapes/enrichments in flight at once is hard-capped at the
  // pool size and a backlog can't peg the box. (Replaces the old single batch
  // poller that claimed up to 20 tasks per tick onto a shared-budget EC.)
  def workerPoolSize: Int = Env.positiveInt("KINOWO_WORKER_POOL_SIZE", 4)
  lazy val taskWorker = new TaskWorker(
    taskQueue, Seq(scrapeCinemaHandler, enrichDetailsHandler) ++ ratingHandlers ++ operatorHandlers ++ stagingHandlers,
    poolSize = workerPoolSize,
    // Each completed task announces itself so StagingReaper can chain the next
    // staging step; non-staging completions are ignored by its subscriber.
    onCompleted = task => eventBus.publish(TaskFinished(task.taskType, task.dedupKey, task.payload))
  )
  lazy val scrapeReaper =
    new ScrapeReaper(cinemaScrapers, taskQueue, freshnessStore, initialDelay = initialScrapeDelaySeconds.seconds,
      maxEnqueuePerTick = maxScrapeEnqueuePerTick, runStore = scheduledRunStore)
  // Logs queue depth every minute so a CPU-credit/steal episode can be correlated
  // with the scrape/enrich backlog that drove it (the diagnostic that was missing
  // when the 2026-06-12 worker-steal episode had to be reconstructed from metrics).
  lazy val workerHeartbeat = new WorkerHeartbeat(taskQueue)

  // Subscribe BEFORE start() so the bus's first MovieDetailsComplete events reach
  // the enrichment handlers. (See the original monolith comment block for the
  // full event-cascade rationale — the wiring is unchanged.)
  //   MovieDetailsComplete → movieService           (TMDB stage)
  //   TmdbResolved       → ratingEnqueuer          (enqueue IMDb/RT/MC/Filmweb)
  //   ImdbIdMissing      → imdbIdResolver + ratingEnqueuer (TMDB-only hits)
  //   ImdbIdResolved     → ratingEnqueuer          (enqueue IMDb)
  // Resolution stays inline (one-shot per scraped row).
  eventBus.subscribe(movieService.onMovieDetailsComplete)
  eventBus.subscribe(imdbIdResolver.onImdbIdMissing)
  // One detail enqueuer per deferred-detail cinema.
  detailEnqueuers.foreach(e => eventBus.subscribe(e.onCinemaMovieAdded))
  // Rating subscribers ENQUEUE tasks; RatingHandlers + EnrichmentReaper fetch.
  eventBus.subscribe(ratingEnqueuer.onTmdbResolved)
  eventBus.subscribe(ratingEnqueuer.onImdbIdResolved)
  eventBus.subscribe(ratingEnqueuer.onImdbIdMissing)
  // A concluded newcomer folds (group-scoped, settling as it goes) into `movies`
  // the moment the StagingFold handler publishes. Each BRAND-NEW film the fold
  // introduces (no pre-existing `movies` row merged in) gets its first-time
  // ratings scheduled right away; a merge into an existing row keeps that row's
  // ratings, so it isn't re-enqueued.
  eventBus.subscribe { case StagingFilmEnriched(title) =>
    stagingFolder.foldGroup(title).foreach { case (key, record) =>
      movieService.scheduleRatingsForNewMovie(key, record)
    }
  }
  // The reaper advances the staging chain (detail → resolve → imdb → fold) one
  // step per finished staging task.
  eventBus.subscribe(stagingReaper.onTaskFinished)

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
    // The task worker drains all queue work: scraping, deferred detail, and
    // queue-driven rating enrichment.
    taskWorker.start(); workerHeartbeat.start()
    enrichmentReaper.start()
    detailReaper.start()
    scrapeReaper.start()
    // Incubate pending_movies through the queue: the reaper kicks new newcomers
    // and backstops stalled chains; the TaskWorker (above) drains the steps.
    stagingReaper.start()
    stagingStuckAlerter.foreach(_.start())
  }

  /** Event-cascade drain order, producer→consumer (see monolith comment). Only
   *  the async stages need draining: the TMDB stage and the IMDb-id resolver.
   *  Rating refresh is synchronous (queue-driven), so the *Ratings own no pool. */
  def cascadeDrainOrder: Seq[Stoppable] = Seq(movieService, imdbIdResolver)

  def stop(): Unit = {
    stagingStuckAlerter.foreach(_.stop())
    stagingReaper.stop()
    scrapeReaper.stop()
    enrichmentReaper.stop()
    detailReaper.stop()
    workerHeartbeat.stop()
    taskWorker.stop()
    taskQueue.close()
    freshnessStore.close()
    cascadeDrainOrder.foreach(_.stop())
    unscreenedCleanup.stop()
    readModelProjector.stop()
    movieCache.stop()
    titleRulesCache.stop()
    readModelRepository.close()
    movieRepository.close()
    titleRulesRepository.close()
    mongoConnection.close()
  }
}
