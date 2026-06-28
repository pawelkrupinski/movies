package modules

import clients.TmdbClient
import models.{Cinema, City}
import services.{MongoCachingDetailFetch, MongoConnection, Stoppable, UptimeMonitor}
import services.alerts.{FallbackAlert, FilmwebDropAlerter, StagingStuckAlerter, TelegramNotifier}
import services.cinemas._
import services.enrichment._
import services.fallback.{FallbackEvent, FilmwebFallbackState, FilmwebFallbackStore, MongoFilmwebFallbackStore}
import services.events.{EventBus, ImdbIdMissing, InProcessEventBus, StagingFilmEnriched, TaskFinished}
import services.freshness.{FreshnessKind, FreshnessStore, MongoFreshnessStore}
import services.movies.{CaffeineMovieCache, MongoMovieRepository, MovieRepository, MovieService, QueueResolveDispatcher, UnscreenedCleanup}
import services.readmodel.{MongoReadModelRepository, ReadModelProjector, ReadModelReader, ReadModelWriter}
import services.resolution.{MongoResolutionStore, ResolutionCache, WriteThroughResolutionCache}
import services.schedule.{AlwaysClaimScheduledRunStore, MongoScheduledRunStore, ScheduledRunStore}
import services.metrics.{MeteredTaskQueue, WorkerTaskMetrics}
import services.tasks.{BulkRefreshHandler, ChunkScrapeCoordinator, ChunkScrapePlanner, ChunkScrapeReaper, ChunkScrapeStore, DetailReaper, DetailTaskEnqueuer, EnrichDetailsHandler, EnrichmentReaper, MongoChunkScrapeStore, BulkCadenceRecorder, MongoTaskQueue, QueueEnrichmentRetrigger, RatingHandler, ResolveImdbIdHandler, ResolveTmdbHandler, ScrapeChunkHandler, ScrapeChunkReduceHandler, ScrapeCinemaHandler, ScrapeReaper, SettleReaper, TaskQueue, TaskType, TaskWorker, UnresolvedTmdbReaper, WorkerHeartbeat}
import services.staging.{MongoStagingFolder, MongoStagingRepository, StagingDetailHandler, StagingFoldHandler, StagingFolder, StagingReaper, StagingRepository, StagingResolveImdbIdHandler, StagingResolveTmdbHandler, StagingSteps}
import tools.{DaemonExecutors, Env, ExecutionBudget, FallbackHttpFetch, HostCircuitBreakerHttpFetch, HostScrapeStats, HttpFetch, MonitoringHttpFetch, RealHttpFetch, ResidentialProxy, ScrapeCities, SessionWarmingHttpFetch, SharedExecutionBudget, StickyShardHttpFetch, ThrottledHttpFetch}

import java.lang.management.ManagementFactory
import java.util.concurrent.ExecutorService
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicBoolean
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
  // ThrottledHttpFetch sits closest to the wire: a per-host 429 gate that pauses
  // ALL callers to a rate-limited host together (honoring Retry-After) instead of
  // each of the ~12 concurrent TMDB callers retrying independently into the same
  // burst. Inside MonitoringHttpFetch so its waits don't skew uptime.
  // The per-host circuit breaker sits closest to the wire: after a few consecutive
  // timeouts/5xx from a host it OPENS and fast-fails every further call to that
  // host for a cooldown, so a slow/hanging origin (Helios's restapi, 2026-06-23)
  // stops pinning the ParallelDetailFetch slots for its whole timeout on every
  // call. Generalises RealHttpFetch's static per-host FastFailRequestTimeout to any
  // host, with no allowlist. Wrapped by ThrottledHttpFetch (429 gate) and
  // MonitoringHttpFetch (so a fast-fail still shows as the real unavailability).
  lazy val httoFetch: HttpFetch =
    new MonitoringHttpFetch(
      new ThrottledHttpFetch(new HostCircuitBreakerHttpFetch(new RealHttpFetch())),
      uptimeMonitor, cinemaScraperCatalog.scrapeHosts)

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
  // design, not as a fallback. Feeds the FilmwebDropAlerter (a Filmweb-only venue
  // going empty means migrate it to an own-site scraper).
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
        // A chunked cinema is scraped via the task fan-out (ScrapeCinemaHandler
        // routes it to ChunkScrapePlanner) and its outcome is recorded at the
        // reduce step, so it skips the per-scrape AdaptiveTimeout — each chunk is
        // already its own bounded task. Everything else is bounded here: the whole
        // scrape (retries included) to an adaptive per-host budget, OUTSIDE retry
        // but INSIDE the uptime recorder so a cut surfaces as a normal failure.
        val inner: CinemaScraper =
          if (raw.isInstanceOf[ChunkedCinemaScraper]) retried
          else new AdaptiveTimeoutScraper(retried, hostScrapeStats, adaptiveTimeoutExecutor)
        recordingScraper(inner, FallbackEligibility.eligible(raw))
      }

  /** Wrap a scrape source with the outcome recorder: the Filmweb fallback for an
   *  eligible single venue, else the plain uptime recorder. Extracted so the
   *  chunked reduce step (`publishScrape`) records uptime + falls back exactly
   *  like a live scrape. */
  private def recordingScraper(inner: CinemaScraper, eligible: Boolean): CinemaScraper =
    if (eligible)
      new FilmwebFallbackScraper(inner,
        () => filmwebFallbackFor(inner.cinema), () => filmwebFallbackIds.get(inner.cinema),
        uptimeMonitor, filmwebFallbackStore, onEvent = filmwebFallbackOnEvent)
    else
      new UptimeRecordingScraper(inner, uptimeMonitor, scrapeOutcomeListener)

  /** Rolling per-host scrape-duration stats backing the adaptive scrape timeout.
   *  In-memory by design — it adds no Mongo write load (the throttle this guards
   *  against IS write/CPU pressure) and rebuilds within a few refresh ticks. */
  lazy val hostScrapeStats: HostScrapeStats = new HostScrapeStats()

  /** Runs each scrape so [[AdaptiveTimeoutScraper]] can time it out and interrupt
   *  it. Virtual threads (cheap, daemon) in production; the test harness
   *  overrides this with a caller-runs executor to stay deterministic. */
  protected lazy val adaptiveTimeoutExecutor: ExecutorService =
    DaemonExecutors.virtualThreadEC("adaptive-timeout")

  // ── Background concurrency budget ───────────────────────────────────────────
  // Scrape + enrichment + the rating refreshers draw run permits from ONE shared
  // budget so a cold start / hourly rating walk can't peg the worker's vCPU.
  // Default 4 (was 8): a live A/B on 2026-06-27 showed halving the parallel-parse
  // cap ~halved the per-tick CPU burst (busy p95 156→58 centi-cores) at unchanged
  // scrape throughput, which is what drives the shared-cpu credit downslope — the
  // burst is the CPU of decoding/parsing scrape payloads that land together, not
  // network wait. Override with KINOWO_BG_CONCURRENCY if a bigger machine lands.
  lazy val backgroundBudget: ExecutionBudget = new SharedExecutionBudget(Env.positiveInt("KINOWO_BG_CONCURRENCY", 4))

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
    new CaffeineMovieCache(movieRepository, eventBus, staging = Some(stagingRepository),
      retrigger = enrichmentRetrigger, mergeMetrics = taskMetrics)

  // After a merge changes an enrichment's input fields, re-kick that enrichment
  // (per case) as a worker task — clearing its freshness stamp so the tmdbId-keyed
  // dedup doesn't skip the re-fetch. See QueueEnrichmentRetrigger / MergeRetrigger.
  lazy val enrichmentRetrigger = new QueueEnrichmentRetrigger(taskQueue, freshnessStore)

  // ── Denormalised read model (web_movies + web_screenings) ───────────────────
  // The worker projects every `movies` write into the two read-model collections
  // the serving app consumes. One impl is both reader (boot-seed the diff state)
  // and writer (upsert/delete the derived documents).
  // Typed as the read+write intersection so test wirings can swap in
  // `InMemoryReadModelRepository` (Mongo-free fixture replay).
  lazy val readModelRepository: ReadModelReader & ReadModelWriter = new MongoReadModelRepository(mongoConnection.database)
  lazy val readModelProjector = new ReadModelProjector(movieRepository, readModelRepository, readModelRepository, taskMetrics)

  // Title-stripping rules. The worker owns seeding: a fresh DB gets the migrated
  // defaults so behaviour is unchanged from the hardcoded baseline. When an edit
  // arrives over the change stream, re-merge existing records so the rule applies
  // retroactively, not just to future scrapes.

  // The *Ratings classes refresh synchronously (the queue's RatingHandler / the
  // operator bulk walk), so they own no EC — only imdbIdResolver still runs
  // async off the bus and draws a shared-budget EC.
  // Each bulk walk feeds its displayed-value changes into the SAME tmdbId-keyed
  // adaptive cadence the per-row RatingHandler feeds (BulkCadenceRecorder), so an
  // operator's corpus refresh can't move a rating without the cadence seeing it —
  // which a later per-row refresh would otherwise mis-read as a fresh change.
  lazy val imdbRatings = new ImdbRatings(movieCache, imdbClient, BulkCadenceRecorder(ratingCadenceStore, FreshnessKind.ImdbRating))
  lazy val imdbIdCache: ResolutionCache = resolutionCache("resolve_imdb")
  lazy val wikidataClient = new WikidataClient(httoFetch)
  lazy val imdbIdResolver = new ImdbIdResolver(movieCache, imdbClient,
    backgroundBudget.executionContext("imdb-id-resolver"), imdbIdCache = imdbIdCache,
    wikidata = Some(wikidataClient))
  lazy val rottenTomatoesRatings = new RottenTomatoesRatings(movieCache, tmdbClient, rottenTomatoesClient, resolutionCache("resolve_rt"),
    cadenceRecorder = BulkCadenceRecorder(ratingCadenceStore, FreshnessKind.RtRating))
  lazy val metascoreRatings = new MetascoreRatings(movieCache, tmdbClient, metacriticClient, resolutionCache("resolve_mc"),
    cadenceRecorder = BulkCadenceRecorder(ratingCadenceStore, FreshnessKind.McRating))
  lazy val filmwebRatings = new FilmwebRatings(movieCache, tmdbClient, filmwebClient, resolutionCache("resolve_filmweb"),
    onImdbIdMissing = (title, year, searchTitle) => eventBus.publish(ImdbIdMissing(title, year, searchTitle)),
    cadenceRecorder = BulkCadenceRecorder(ratingCadenceStore, FreshnessKind.FilmwebRating))
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
    movieCache, eventBus, tmdbClient,
    dispatcher = Some(new QueueResolveDispatcher(taskQueue)),
    tmdbIdCache = tmdbIdCache,
    // SAME store the rating handlers read, so the resolved → first-rating delay
    // (stamped here on resolution, observed there on first attempt) correlates.
    freshness = freshnessStore,
    // Kick a freshly-promoted newcomer's ratings the instant it folds — the SAME
    // enqueuer the EnrichmentReaper walks the corpus with, so a newcomer and a
    // reaper sweep share the eligibility + due gate. A fold is a trickle, so this
    // doesn't recreate the old TmdbResolved corpus-wide burst.
    enqueueNewcomerRatings = (key, record) => { ratingEnqueuer.enqueueDueFor(key, record, java.time.Instant.now()); () })
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
  // restart still throttles, up once Mongo/CPU have headroom. Default sized in
  // ScrapeCadence (≥1.5× the steady-state due rate at the freshness window).
  def maxScrapeEnqueuePerTick: Int =
    Env.positiveInt("KINOWO_SCRAPE_MAX_ENQUEUE_PER_TICK", services.tasks.ScrapeCadence.MaxEnqueuePerTick)

  // Cinema scraping is driven by a durable Mongo task queue: the ScrapeReaper
  // enqueues each cinema at most once per freshness window, and the TaskWorker
  // scrapes it (skipping if a concurrent run already refreshed it). Detail and
  // rating enrichment are governed by KINOWO_DEFERRED_DETAIL and
  // KINOWO_QUEUE_ENRICHMENT independently.

  // Rating refresh (IMDb/Filmweb/RT/Metacritic) runs as freshness-gated queue
  // tasks — deduped and shared across servers. TMDB / IMDb-id RESOLUTION stays
  // inline (it's one-shot per scraped row, already driven by the queue-gated
  // scrape).
  // Task-pipeline metrics, exposed at /metrics (WorkerMain) and scraped by Fly
  // Prometheus. The queue is wrapped so every enqueue is metered centrally; the
  // TaskWorker reports claims/outcomes/durations via the same object as its
  // `TaskObserver`; the /metrics handler refreshes the queue gauges per scrape.
  // One registry shared by every worker metric so a single /metrics scrape (and
  // the existing `taskMetrics.scrape()` render) exposes the task pipeline AND the
  // corpus census together.
  lazy val metricsRegistry: io.prometheus.metrics.model.registry.PrometheusRegistry =
    new io.prometheus.metrics.model.registry.PrometheusRegistry()
  lazy val taskMetrics: WorkerTaskMetrics = new WorkerTaskMetrics(workerPoolSize, metricsRegistry)
  // Periodic census of the movies corpus (counts of resolved/rated rows), sampled
  // off-band and exposed on the same registry — see WorkerCorpusMetrics.
  lazy val corpusMetrics: services.metrics.WorkerCorpusMetrics =
    new services.metrics.WorkerCorpusMetrics(movieRepository, metricsRegistry)
  // Per-site backlog of resolved films whose rating has NEVER run — the never-run
  // latency the first-attempt histogram can't show (see RatingRunCensus).
  lazy val ratingRunCensus: services.metrics.RatingRunCensus =
    new services.metrics.RatingRunCensus(movieCache, freshnessStore, metricsRegistry)
  lazy val taskQueue: TaskQueue =
    new MeteredTaskQueue(new MongoTaskQueue(mongoConnection.database), taskMetrics)
  lazy val freshnessStore: FreshnessStore = new MongoFreshnessStore(mongoConnection.database)
  // Live config: install the Mongo override cache as Env's override source and
  // publish this process's (non-secret) knobs to the shared registry so the web
  // `/admin/config` page can list + flip them mid-flight. See EnvConfigService.
  lazy val envConfigService = new services.config.EnvConfigService(
    app       = "worker",
    overrides = new services.config.MongoEnvOverrideStore(mongoConnection.database),
    registry  = new services.config.MongoEnvRegistryStore(mongoConnection.database),
    tickInterval = Env.positiveLong("KINOWO_CONFIG_REFRESH_SECONDS", 30L).seconds)
  // Auto-recovery from the shared-CPU-credit deadlock: the AUTHORITATIVE control
  // is an in-process poller that reads the REAL fly_instance_cpu_balance straight
  // from Fly's Prometheus and tells the reapers to back off enqueue while credit
  // is floored, so the box earns idle and rebuilds credit. Needs a read-only Fly
  // token (KINOWO_FLY_PROM_TOKEN); absent it (local dev / tests), there's no
  // poller and only the external gate drives throttling.
  //
  // RESTART PATHS — all three share a single one-shot guard (`restartFired`) so
  // at most one restart fires per process lifetime regardless of which path trips:
  //
  //   1. DOWNSLOPE (projection trigger in CpuCreditPoller): fires when the drain
  //      rate projects a floor hit in < 10 min, while balance is still above 6000.
  //      Restarts immediately (subject to a JVM-uptime cooldown that prevents a
  //      rapid-restart loop if the circuit breaker hasn't had time to trip yet).
  //      Confirmed fastest path: 2026-06-26 incident would have been averted at
  //      02:33 (credit 50k, 6 min to floor) instead of 89 min at true-floor.
  //
  //   2. FLOOR FAST-PATH (ThrottleStuckWatchdog.isFloorStuck): if credit stays
  //      below floorThreshold (≈ 1000) for 15 min, restart without waiting for
  //      the 45-min stuckAfter or applying the trend guard. At true-floor the
  //      oscillations are Fly micro-bursts, not recovery — trend guard is useless.
  //
  //   3. STUCK PATH (ThrottleStuckWatchdog.isStuck): if throttled continuously
  //      for 45 min and credit is NOT trending up — the duty-cycle + reaper backoff
  //      failed to recover. The trend guard (credit-trending-up check) defers the
  //      restart while self-recovery is underway, preventing the 30-min crash loop
  //      of 2026-06-24.
  //
  // UPTIME COOLDOWN on the downslope path: if the process is younger than
  // `projectionRestartCooldownSeconds`, the downslope restart is suppressed.
  // This prevents a rapid-restart loop where each new process immediately
  // re-triggers the projection (circuit breaker needs a full scrape cycle to
  // trip the offending host).  Paths 2 and 3 are NOT cooldown-gated — by the
  // time they fire, enough time has elapsed that the circuit breaker has had
  // its chance.
  private val restartFired = new AtomicBoolean(false)
  private def doRestart(reason: String): Unit =
    if (restartFired.compareAndSet(false, true)) {
      logger.error(s"Worker restart triggered ($reason); exiting non-zero so Fly reschedules the machine.")
      sys.exit(1)
    }

  def projectionRestartCooldownSeconds: Long =
    Env.positiveLong("KINOWO_WORKER_PROJECTION_RESTART_COOLDOWN_SECONDS", 600L)

  lazy val cpuCreditPoller: Option[services.tasks.CpuCreditPoller] =
    Env.get("KINOWO_FLY_PROM_TOKEN").map { token =>
      new services.tasks.CpuCreditPoller(
        new RealHttpFetch(), token,
        onProjectionThrottle = () => {
          val uptimeSec = ManagementFactory.getRuntimeMXBean.getUptime / 1000
          if (uptimeSec < projectionRestartCooldownSeconds)
            logger.warn(s"CpuCreditPoller: projection throttle fired but JVM uptime ${uptimeSec}s is within the " +
              s"${projectionRestartCooldownSeconds}s cooldown — skipping downslope restart to avoid a rapid-restart " +
              s"loop (circuit breaker needs time to trip the offending host).")
          else
            doRestart(s"projection downslope (uptime ${uptimeSec}s)")
        }
      )
    }

  // The credit-balance throttle pushed in from OUTSIDE (the same Grafana alert on
  // fly_instance_cpu_balance → the worker's /throttle endpoint). Kept as an
  // INDEPENDENT backstop to the poller: if the poller can't reach api.fly.io (or
  // its token lapses) and fails open, this still trips on a real credit crunch.
  lazy val externalThrottleGate = new services.tasks.ExternalThrottleGate
  lazy val throttleSignal: services.tasks.ScrapeThrottleSignal =
    cpuCreditPoller.fold[services.tasks.ScrapeThrottleSignal](externalThrottleGate)(
      poller => services.tasks.ScrapeThrottleSignal.either(externalThrottleGate, poller))

  def throttleStuckMinutes: Long  = Env.positiveLong("KINOWO_WORKER_THROTTLE_STUCK_MINUTES", 45L)
  def floorStuckMinutes: Long     = Env.positiveLong("KINOWO_WORKER_FLOOR_STUCK_MINUTES", 15L)
  def floorThreshold: Double      = Env.positiveLong("KINOWO_WORKER_FLOOR_THRESHOLD", 1000L).toDouble
  lazy val throttleStuckWatchdog = new services.tasks.ThrottleStuckWatchdog(
    throttleSignal, stuckAfter = throttleStuckMinutes.minutes,
    onStuck         = () => doRestart("floor/stuck watchdog"),
    creditBalance   = () => cpuCreditPoller.flatMap(_.lastBalance),
    floorThreshold  = floorThreshold,
    floorStuckAfter = floorStuckMinutes.minutes)

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

  // ── Chunked (map-reduce) scrape machinery ──────────────────────────────────
  // A chunked cinema (ChunkedCinemaScraper) is scraped as one ScrapeChunk task
  // per chunk, gathered by ChunkScrapeCoordinator + the ChunkScrapeReaper backstop
  // and aggregated by one ScrapeChunkReduce task — namespaced by a per-run id so a
  // re-scrape can't conflict with an in-flight one. See ChunkScrapeStore.
  lazy val chunkScrapeStore: ChunkScrapeStore = new MongoChunkScrapeStore(mongoConnection.database)

  /** Raw chunked clients keyed by displayName — the plan/fetchChunk/reduce
   *  functions the chunk tasks call. Empty until a client opts in. */
  lazy val chunkScrapers: Map[String, ChunkedCinemaScraper] =
    City.all
      .filter(c => scrapeCities(c.slug))
      .flatMap(c => cinemaScraperCatalog.byCity.getOrElse(c.slug, Nil))
      .collect { case cs: ChunkedCinemaScraper => ScrapeCinemaHandler.scraperKey(cs.cinema) -> cs }
      .toMap

  /** Publish a (pre-scraped) listing through the SAME recorder + runner a live
   *  scrape uses — so the chunked reduce records uptime and falls back to Filmweb
   *  identically. Also the sink for plan-step (nav-fetch) failures. */
  private val publishScrape: CinemaScraper => Unit =
    inner => { cinemaScrapeRunner.run(recordingScraper(inner, FallbackEligibility.eligible(inner))); () }

  lazy val chunkScrapePlanner       = new ChunkScrapePlanner(chunkScrapers, chunkScrapeStore, taskQueue, publishScrape)
  lazy val scrapeChunkHandler       = new ScrapeChunkHandler(chunkScrapers, chunkScrapeStore)
  lazy val scrapeChunkReduceHandler = new ScrapeChunkReduceHandler(chunkScrapers, chunkScrapeStore, publishScrape, freshnessStore)
  lazy val chunkScrapeCoordinator   = new ChunkScrapeCoordinator(chunkScrapeStore, taskQueue)
  lazy val chunkScrapeReaper        = new ChunkScrapeReaper(chunkScrapeStore, taskQueue, chunkScrapeCoordinator,
    runStore = scheduledRunStore)

  // ONE shared due schedule backs both the scrape reaper (enqueue) and the scrape
  // handler (pickup re-gate), so they agree on what's due and a cinema's scrapes
  // spread across the freshness window instead of falling due in a lockstep wave.
  val scrapeDueWindow = new services.tasks.DueWindow(services.freshness.Freshness.defaultScrapeTtl)
  lazy val scrapeCinemaHandler = new ScrapeCinemaHandler(
    cinemaScrapers.map(s => ScrapeCinemaHandler.scraperKey(s.cinema) -> s).toMap,
    cinemaScrapeRunner, freshnessStore, scrapeDueWindow,
    chunkPlanner = Some(chunkScrapePlanner)
  )
  // Shared detail refresh schedule. Its period is the DetailEnrich TTL (6h,
  // `Freshness.ttlFor`); the SAME instance backs the reaper (enqueue gate) and the
  // handler (pickup gate) so they agree on "due" — see [[services.tasks.DueWindow]].
  val detailDueWindow = new services.tasks.DueWindow(6L.hours)
  lazy val enrichDetailsHandler = new EnrichDetailsHandler(
    detailEnrichers.map(de => de.detailGroup -> de).toMap, movieCache, freshnessStore, uptimeMonitor, eventBus,
    detailDueWindow
  )
  // Detail enqueue is event-driven: one enqueuer per deferred cinema fires the
  // first detail fetch off CinemaMovieAdded; the reaper is the periodic
  // refresh/retry backstop (CinemaMovieAdded fires only on first appearance),
  // phase-spread + capped so a re-key cohort trickles instead of dumping (~1k
  // EnrichDetails in one tick, which cascaded into the ResolveTmdb/rating bursts
  // that pinned the shared-CPU credit). Same lever as the scrape/rating reapers.
  lazy val detailEnqueuers: Seq[DetailTaskEnqueuer] =
    detailEnrichers.map(de => new DetailTaskEnqueuer(de, movieCache, taskQueue, freshnessStore))
  // The trickle every NON-scrape reaper (detail, ratings, tmdb-retry) drops to
  // while the worker is CPU-credit throttled. Backing off scrapes alone wasn't
  // enough — ratings/detail kept the pool busy so it never idled to rebuild
  // credit; quieting the WHOLE pipeline is what lets it recover (see
  // CpuCreditPoller / ScrapeThrottleSignal.cap).
  def throttledSecondaryEnqueuePerTick: Int =
    Env.positiveInt("KINOWO_THROTTLED_ENQUEUE_PER_TICK", services.tasks.ScrapeCadence.ThrottledSecondaryEnqueuePerTick)
  def maxDetailEnqueuePerTick: Int = Env.positiveLong("KINOWO_DETAIL_MAX_ENQUEUE_PER_TICK", 50L).toInt
  // How often the detail reaper wakes to enqueue the now-due slice (the spread
  // granularity). Finer = flatter per-minute `EnrichDetails` trickle on the
  // `kinowo_worker_tasks` panel, at the cost of cheap in-memory corpus scans.
  // Default 1min (≈360 ticks per 6h).
  def detailTickInterval: FiniteDuration =
    Env.positiveLong("KINOWO_DETAIL_TICK_INTERVAL_SECONDS", DetailReaper.DefaultTickInterval.toSeconds).seconds
  lazy val detailReaper = new DetailReaper(detailEnrichers, movieCache, taskQueue, freshnessStore, eventBus,
    dueWindow = detailDueWindow, tickInterval = detailTickInterval, maxEnqueuePerTick = maxDetailEnqueuePerTick,
    throttledMaxEnqueuePerTick = throttledSecondaryEnqueuePerTick, throttle = throttleSignal,
    runStore = scheduledRunStore)

  // The whole-corpus settle on its OWN periodic tick, decoupled from the cache
  // hydrate. The settle used to ride `MovieCache.rehydrate`'s backstop reload
  // (KINOWO_CACHE_REHYDRATE_SECONDS=1800s), but a reload re-derives every key as
  // `displayTitle`, so settling right after it re-keyed the spelling-variant rows —
  // the per-deploy flap. Now the load is a pure read and this reaper re-asserts the
  // one-row-per-film invariant once per the SAME 30-min window (cluster-claimed).
  def settleIntervalSeconds: FiniteDuration =
    Env.positiveLong("KINOWO_SETTLE_INTERVAL_SECONDS", SettleReaper.DefaultInterval.toSeconds).seconds
  lazy val settleReaper = new SettleReaper(() => movieService.settle(),
    interval = settleIntervalSeconds, runStore = scheduledRunStore)

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
  // per-row refreshOneSync; the EnrichmentReaper is the SOLE enqueue path — it
  // refreshes each row once per 4h, phase-spread across frequent ticks and capped
  // per tick. A freshly-resolved film's first ratings come from the reaper's
  // due-immediately first pass (within a tick, bounded by the cap), NOT from an
  // instant per-resolution-event burst, so a cohort of resolutions can't fan out
  // into a rating-task spike (the midday `kinowo_worker_tasks` peaks).
  // ONE shared due schedule backs both the reaper (enqueue) and every handler
  // (pickup re-gate), so they agree on what's due — see [[DueWindow]].
  // Its period is the rating TTL (4h, `Freshness.ttlFor`).
  // Per-(source, film) change history → adaptive refresh interval. The rating
  // DueWindow resolves each key's period from its cadence stats instead of a flat
  // 4h, so a film whose displayed value hasn't moved backs off toward 4 days while
  // a fresh/volatile one stays at the 2h base. The reaper + handler share this one
  // instance (their due definitions must agree — see DueWindow).
  lazy val ratingCadenceStore: services.cadence.RatingCadenceStore =
    new services.cadence.MongoRatingCadenceStore(mongoConnection.database)
  val ratingDueWindow = new services.tasks.DueWindow(
    key => services.cadence.RatingCadence.intervalFor(ratingCadenceStore.statsFor(key)),
    services.cadence.RatingCadence.BaseInterval
  )
  lazy val ratingHandlers: Seq[services.tasks.TaskHandler] = Seq(
    new RatingHandler(TaskType.ImdbRating,    FreshnessKind.ImdbRating,    freshnessStore, ratingDueWindow, ratingCadenceStore, imdbRatings.refreshOneSync,         metrics = taskMetrics),
    new RatingHandler(TaskType.FilmwebRating, FreshnessKind.FilmwebRating, freshnessStore, ratingDueWindow, ratingCadenceStore, filmwebRatings.refreshOneSync,      metrics = taskMetrics),
    new RatingHandler(TaskType.RtRating,      FreshnessKind.RtRating,      freshnessStore, ratingDueWindow, ratingCadenceStore, rottenTomatoesRatings.refreshOneSync, metrics = taskMetrics),
    new RatingHandler(TaskType.McRating,      FreshnessKind.McRating,      freshnessStore, ratingDueWindow, ratingCadenceStore, metascoreRatings.refreshOneSync,    metrics = taskMetrics)
  )
  // Cap on rating-refresh tasks the EnrichmentReaper enqueues per tick. The phase
  // spread keeps steady-state ticks small (~N·tickInterval/period per source ≈ a
  // handful across all four at the 1min cadence), so this only bites a cold/long-down
  // corpus where every row is due at once — bounding that recovery burst, the same
  // lever as the scrape reaper. Set comfortably above the steady-state so normal
  // operation is never throttled; the leftover stays due and drains over the next ticks.
  def maxEnrichmentEnqueuePerTick: Int = Env.positiveLong("KINOWO_ENRICHMENT_MAX_ENQUEUE_PER_TICK", 250L).toInt
  // How often the reaper wakes to enqueue the now-due slice (the spread granularity).
  // Finer = flatter per-minute rating trickle on the `kinowo_worker_tasks` panel,
  // at the cost of cheap in-memory corpus scans. Default 1min (≈240 ticks per 4h).
  def enrichmentTickInterval: FiniteDuration =
    Env.positiveLong("KINOWO_ENRICHMENT_TICK_INTERVAL_SECONDS", EnrichmentReaper.DefaultTickInterval.toSeconds).seconds
  // The per-row rating-enqueue decision, shared by the reaper's corpus walk and the
  // newcomer-fold kick (`MovieService.announceResolvedNewMovie`) so the two agree on
  // eligibility + the tmdbId-keyed due gate. ONE instance, handed to both.
  lazy val ratingEnqueuer = new services.tasks.RatingEnqueuer(taskQueue, freshnessStore, ratingDueWindow)
  lazy val enrichmentReaper = new EnrichmentReaper(movieCache, taskQueue, freshnessStore,
    dueWindow = ratingDueWindow, tickInterval = enrichmentTickInterval,
    maxEnqueuePerTick = maxEnrichmentEnqueuePerTick,
    throttledMaxEnqueuePerTick = throttledSecondaryEnqueuePerTick, throttle = throttleSignal,
    runStore = scheduledRunStore, enqueuer = Some(ratingEnqueuer))

  // Re-tries unresolved-TMDB rows once per 24h, phase-spread across the period —
  // the queue-era replacement for MovieService's old daily, all-at-once
  // `retryUnresolvedTmdb` scheduler (it re-dispatched the whole unresolved
  // backlog 10s after boot, the boot ResolveTmdb burst that pinned the
  // shared-CPU credit). `retryResolve` clears each due row's negative + dispatches
  // its ResolveTmdb. Cap bounds a clock-jump/cold burst the same way the rating
  // reaper does — the leftover stays due and re-tries next period.
  def maxTmdbRetryEnqueuePerTick: Int = Env.positiveLong("KINOWO_TMDB_RETRY_MAX_ENQUEUE_PER_TICK", 100L).toInt
  lazy val unresolvedTmdbReaper = new UnresolvedTmdbReaper(movieCache, movieService.retryResolve,
    maxEnqueuePerTick = maxTmdbRetryEnqueuePerTick,
    throttledMaxEnqueuePerTick = throttledSecondaryEnqueuePerTick, throttle = throttleSignal,
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
    new BulkRefreshHandler(TaskType.SettleNow,            "Settle",     () => movieService.settle()),
    new ResolveTmdbHandler(movieService.resolveTmdbOnce),
    // Movies-path IMDb-id recovery as a task (was inline off ImdbIdMissing) — so
    // the merge-retrigger path can re-kick it; resolveSync writes the id, and the
    // EnrichmentReaper then enqueues the now-eligible IMDb rating on its next pass.
    new ResolveImdbIdHandler(imdbIdResolver)
  )

  // A fixed pool of workers, each fetching and running ONE task at a time — so
  // the number of scrapes/enrichments in flight at once is hard-capped at the
  // pool size and a backlog can't peg the box. (Replaces the old single batch
  // poller that claimed up to 20 tasks per tick onto a shared-budget EC.)
  def workerPoolSize: Int = Env.positiveInt("KINOWO_WORKER_POOL_SIZE", 4)
  // While CPU-credit throttled, each busy worker pauses this long before its next
  // claim so the pool sheds load and the box idles enough to rebuild credit. The
  // reaper enqueue-backoff (below) only trims NEW work; it can't drain a backlog
  // that itself keeps the pool pegged at the throttle ceiling — the sustained
  // spiral of 2026-06-23. Live-tunable via KINOWO_WORKER_THROTTLE_PAUSE_MILLIS.
  def workerThrottlePauseMillis: Long = Env.positiveLong("KINOWO_WORKER_THROTTLE_PAUSE_MILLIS", 2000L)
  lazy val taskWorker = new TaskWorker(
    taskQueue, Seq(scrapeCinemaHandler, enrichDetailsHandler, scrapeChunkHandler, scrapeChunkReduceHandler) ++ ratingHandlers ++ operatorHandlers ++ stagingHandlers,
    poolSize = workerPoolSize,
    // The SAME composite credit-throttle signal the reapers read, so the pool
    // duty-cycles in lockstep with the enqueue-backoff under a credit crunch.
    throttle = throttleSignal,
    throttlePause = workerThrottlePauseMillis.millis,
    // Each completed task announces itself so StagingReaper can chain the next
    // staging step; non-staging completions are ignored by its subscriber.
    onCompleted = task => eventBus.publish(TaskFinished(task.taskType, task.dedupKey, task.payload)),
    // Report claims / outcomes / handler durations to the Prometheus metrics.
    observer = taskMetrics
  )
  // While throttled the reaper enqueues at most this many newly-due cinemas per
  // tick (vs the healthy `maxScrapeEnqueuePerTick`), so the backlog drains and the
  // pool earns idle to rebuild credit. Sized in ScrapeCadence to still clear the
  // whole catalogue within one freshness window (so a throttle episode keeps pace
  // with the freshness setting rather than parking the corpus ~1.5h stale, the old
  // cap=3 behaviour), while staying below the healthy cap so the pool still idles.
  def throttledScrapeEnqueuePerTick: Int =
    Env.positiveInt("KINOWO_SCRAPE_THROTTLED_MAX_ENQUEUE_PER_TICK",
      services.tasks.ScrapeCadence.ThrottledMaxEnqueuePerTick)
  lazy val scrapeReaper =
    new ScrapeReaper(cinemaScrapers, taskQueue, freshnessStore, dueWindow = scrapeDueWindow,
      initialDelay = initialScrapeDelaySeconds.seconds,
      maxEnqueuePerTick = maxScrapeEnqueuePerTick,
      throttledMaxEnqueuePerTick = throttledScrapeEnqueuePerTick, throttle = throttleSignal,
      runStore = scheduledRunStore)
  // Logs queue depth every minute so a CPU-credit/steal episode can be correlated
  // with the scrape/enrich backlog that drove it (the diagnostic that was missing
  // when the 2026-06-12 worker-steal episode had to be reconstructed from metrics).
  lazy val workerHeartbeat = new WorkerHeartbeat(taskQueue)

  // Last-resort backstop for a WEDGED-but-alive JVM (the 2026-06-23 heap OOM, where
  // the process limped on for ~2h answering /health 200 because the OOM had killed
  // its worker threads but not the process, and the throttle watchdog couldn't see
  // it — the credit poller failed open to "healthy"). The LivenessWatchdog watches
  // the heartbeat pulse and, if it stalls past the threshold, dumps the about-to-die
  // heap to the Fly volume (so a leak-vs-too-tight analysis is possible offline) and
  // exits non-zero so Fly reschedules. Threshold sits several heartbeat intervals
  // above the 1-min pulse so GC jitter never trips it.
  def livenessStaleMinutes: Long = Env.positiveLong("KINOWO_WORKER_LIVENESS_STALE_MINUTES", 5L)
  def heapDumpDir: String        = Env.get("KINOWO_HEAP_DUMP_DIR").getOrElse("/data/heapdumps")
  lazy val livenessWatchdog = new services.tasks.LivenessWatchdog(
    lastBeatMillis     = () => workerHeartbeat.lastTickMillis,
    stalenessThreshold = livenessStaleMinutes.minutes,
    onWedged           = () => { tools.HeapDumper.dump(heapDumpDir); sys.exit(70) })

  // Subscribe BEFORE start() so the bus's first MovieDetailsComplete events reach
  // the enrichment handlers.
  //   MovieDetailsComplete → movieService    (TMDB stage)
  //   ImdbIdMissing        → imdbIdResolver  (recover the missing IMDb id)
  // Resolution stays inline (one-shot per scraped row). Ratings are NOT enqueued
  // off resolution any more — the EnrichmentReaper is the sole rating-enqueue path
  // (capped + phase-spread), so a cohort of resolutions can't fan out into an
  // instant rating-task burst. ImdbIdMissing is the only resolution event with a
  // subscriber now (id recovery); the old TmdbResolved / ImdbIdResolved events
  // were removed once nothing consumed them.
  eventBus.subscribe(movieService.onMovieDetailsComplete)
  eventBus.subscribe(imdbIdResolver.onImdbIdMissing)
  // One detail enqueuer per deferred-detail cinema.
  detailEnqueuers.foreach(e => eventBus.subscribe(e.onCinemaMovieAdded))
  // A concluded newcomer folds (group-scoped, settling as it goes) into `movies`
  // the moment the StagingFold handler publishes. Each BRAND-NEW film the fold
  // introduces (no pre-existing `movies` row merged in) re-publishes its resolution
  // outcome so a TMDB-only hit kicks IMDb-id recovery, AND immediately enqueues its
  // now-eligible rating tasks (`announceResolvedNewMovie` → `ratingEnqueuer`) so a
  // newcomer's ratings don't wait for the reaper's next tick — a trickle, not the
  // corpus-wide burst the reaper's cap smooths. A merge into an existing row keeps
  // that row's ratings, so it's left untouched.
  eventBus.subscribe { case StagingFilmEnriched(title) =>
    stagingFolder.foldGroup(title).foreach { case (key, record) =>
      movieService.announceResolvedNewMovie(key, record)
    }
  }
  // The reaper advances the staging chain (detail → resolve → imdb → fold) one
  // step per finished staging task.
  eventBus.subscribe(stagingReaper.onTaskFinished)
  // The coordinator enqueues a chunked scrape's reduce once its last chunk task
  // finishes (the ChunkScrapeReaper backstop covers lost completions).
  eventBus.subscribe(chunkScrapeCoordinator.onTaskFinished)

  def start(): Unit = {
    // Force Mongo at boot so connection errors surface in the boot timeline.
    mongoConnection.database
    // Install the override source first so boot-time knob reads already see flips.
    envConfigService.start()
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
    // Ratings refresh via the queue (RatingHandlers + the EnrichmentReaper
    // backstop); refreshOneSync, which the handlers call, needs no start().
    unscreenedCleanup.start()
    // Tag each cinema with its scraper-client marker (shared platform client vs a
    // bespoke one) plus the FtFW chip if it's already in Filmweb fallback at boot
    // (transitions only fire on change, so an in-flight fallback would otherwise go
    // untagged until it next flips). Same rationale as above — the catalog is
    // worker-only, so the tags ride the UptimeMonitor tag channel.
    clientMarkers.foreach { case (cinema, marker) =>
      val inFallback = filmwebFallbackStore.get(cinema).exists(_.active)
      uptimeMonitor.tagService(cinema, CinemaClientMarkers.tagsFor(Some(marker), sourceUrls.get(cinema), inFallback))
    }
    // Poll the real CPU-credit balance so the reapers back off before the box
    // starves (the authoritative throttle signal; absent its token, the external
    // gate alone drives backoff).
    cpuCreditPoller.foreach(_.start())
    // Arm the last-resort restart backstop for a throttle spiral the backoff can't break.
    throttleStuckWatchdog.start()
    // The task worker drains all queue work: scraping, deferred detail, and
    // queue-driven rating enrichment.
    taskWorker.start(); workerHeartbeat.start()
    // Arm AFTER the heartbeat (so the first pulse is already stamped) — restart a
    // wedged-but-alive JVM the throttle watchdog can't see.
    livenessWatchdog.start()
    enrichmentReaper.start()
    unresolvedTmdbReaper.start()
    detailReaper.start()
    settleReaper.start()
    scrapeReaper.start()
    // Backstop the chunked-scrape fan-in: recover complete runs whose completion
    // event was lost, and partial-reduce abandoned runs.
    chunkScrapeReaper.start()
    // Incubate pending_movies through the queue: the reaper kicks new newcomers
    // and backstops stalled chains; the TaskWorker (above) drains the steps.
    stagingReaper.start()
    stagingStuckAlerter.foreach(_.start())
    // Census the corpus for the /metrics gauges (off-band, read-only paged scan).
    corpusMetrics.start()
    // Census the per-site never-run rating backlog (off-band, in-memory scan).
    ratingRunCensus.start()
  }

  /** Event-cascade drain order, producer→consumer (see monolith comment). Only
   *  the async stages need draining: the TMDB stage and the IMDb-id resolver.
   *  Rating refresh is synchronous (queue-driven), so the *Ratings own no pool. */
  def cascadeDrainOrder: Seq[Stoppable] = Seq(movieService, imdbIdResolver)

  def stop(): Unit = {
    envConfigService.stop()
    ratingRunCensus.stop()
    corpusMetrics.stop()
    stagingStuckAlerter.foreach(_.stop())
    stagingReaper.stop()
    scrapeReaper.stop()
    chunkScrapeReaper.stop()
    enrichmentReaper.stop()
    unresolvedTmdbReaper.stop()
    detailReaper.stop()
    settleReaper.stop()
    livenessWatchdog.stop()
    workerHeartbeat.stop()
    throttleStuckWatchdog.stop()
    cpuCreditPoller.foreach(_.stop())
    taskWorker.stop()
    taskQueue.close()
    freshnessStore.close()
    cascadeDrainOrder.foreach(_.stop())
    unscreenedCleanup.stop()
    readModelProjector.stop()
    movieCache.stop()
    readModelRepository.close()
    movieRepository.close()
    mongoConnection.close()
  }
}
