package modules

import org.mongodb.scala.MongoClient
import models.Country
import modules.wiring.{AlertingWiring, ChunkScrapeWiring, CorpusWiring, DetailWiring, EgressWiring, HttpWiring, MetricsWiring, OperatorWiring, RatingsWiring, ReadModelWiring, ResolutionWiring, ScrapeWiring, StagingWiring, TaskQueueWiring}
import services.cinemas.common.CinemaClientMarkers
import services.events.{EventBus, InProcessEventBus}
import services.freshness.{Freshness, FreshnessKind}
import services.{Drainable, MongoConnection, UptimeMonitor}
import services.cadence.RatingCadence
import services.metrics.WorkerMetrics
import services.tasks.{DueWindow, VenueCadenceStore}
import tools.{Env, ExecutionBudget, SharedExecutionBudget}

/**
 * Write composition root: cinema scraping + the enrichment cascade. Runs as its
 * own process (`WorkerMain`), writing through MovieCache to Mongo; the serving
 * app's cache picks the writes up via the Mongo change stream. Constructs the
 * shared data layer (Mongo, MovieCache, EventBus, UptimeMonitor) itself — the
 * two apps share the database, not in-process objects.
 *
 * This is the scrape/enrich half of what used to be the monolith's single
 * `Wiring`; the serving half now lives in the web app's `modules.Wiring`.
 *
 * Parametrized by [[Country]]: the worker instantiates ONE of these per country
 * it runs (from `KINOWO_COUNTRIES`, see `WorkerMain`), each scraping only its
 * own [[Country.cities]] and writing to its own [[Country.mongoDb]] database.
 * The two cross-country shared resources — the background concurrency budget and
 * the underlying `MongoClient` — are injected so every country draws from ONE
 * cap and ONE connection pool; both default to a self-owned instance so a
 * single-country boot / test constructs unchanged.
 *
 * Each subsystem's wiring lives in its own `modules.wiring.*` trait (self-typed
 * to this class, so every member stays a lazy val a test wiring can override by
 * name); this class keeps only the shared data layer, the EAGER members (the
 * three `DueWindow`s and the bus subscriptions, whose relative order is the boot
 * contract) and the start/stop lifecycle.
 */
class WorkerWiring(
    val country: Country = Country.default,
    // ONE shared background concurrency budget across countries: `WorkerMain`
    // builds it once and injects the SAME instance into every country's wiring,
    // so all countries draw run permits from one Semaphore/cap rather than each
    // spinning its own (see `backgroundBudget`). Defaulted so a single-country
    // boot / test constructs its own.
    injectedBackgroundBudget: ExecutionBudget =
      new SharedExecutionBudget(Env.positiveInt("KINOWO_BG_CONCURRENCY", 4)),
    // ONE shared `MongoClient` across countries: each country binds its OWN
    // database view (`country.mongoDb`) on this single client. `None` → this
    // wiring builds (and closes) its own client from `MONGODB_URI`, the
    // single-connection default.
    sharedMongoClient: Option[MongoClient] = None,
    // The process-wide worker metrics bundle (ONE registry + one set of metric
    // objects, shared across every country's wiring — see WorkerMetrics). WorkerMain
    // builds it once over ALL countries and injects the SAME instance so every
    // country's series lands on the single `/metrics` registry. `None` → a lone
    // boot / test builds its own single-country bundle (resolved in `workerMetrics`
    // below). Kept an Option — mirroring `sharedMongoClient` — because a default
    // referencing `country` can't live in the same parameter clause as `country`.
    injectedWorkerMetrics: Option[WorkerMetrics] = None) extends play.api.Logging
    with HttpWiring with EgressWiring with ScrapeWiring with ChunkScrapeWiring with DetailWiring
    with CorpusWiring with ResolutionWiring with RatingsWiring with ReadModelWiring
    with MetricsWiring with TaskQueueWiring with StagingWiring with AlertingWiring with OperatorWiring {

  /** The metrics bundle this wiring records into: the shared injected one, or a
   *  self-owned single-country bundle when none was injected (lone boot / test). */
  val workerMetrics: WorkerMetrics =
    injectedWorkerMetrics.getOrElse(
      WorkerMetrics.singleCountry(country, Env.positiveInt("KINOWO_WORKER_POOL_SIZE", 4)))
  lazy val uptimeMonitor = new UptimeMonitor(mongoConnection.database)

  // ── Filmweb (per-country) ───────────────────────────────────────────────────
  // Whether the Filmweb rating + fallback path is wired at all — a per-country
  // decision ([[Country.filmwebEnabled]]). A non-Filmweb country runs the whole
  // pipeline with NO Filmweb rating source, fallback scraper, drop-alerter, or
  // rating/bulk handler. `protected def` so a test can pin it independently of
  // the (sealed, single-country-today) Country set. When true, everything below
  // is unchanged from before this dimension existed.
  protected def filmwebEnabled: Boolean = country.filmwebEnabled

  // ── Background concurrency budget ───────────────────────────────────────────
  // Scrape + enrichment + the rating refreshers draw run permits from ONE shared
  // budget so a cold start / hourly rating walk can't peg the worker's vCPU.
  // Default 4 (was 8): a live A/B on 2026-06-27 showed halving the parallel-parse
  // cap ~halved the per-tick CPU burst (busy p95 156→58 centi-cores) at unchanged
  // scrape throughput, which is what drives the shared-cpu credit downslope — the
  // burst is the CPU of decoding/parsing scrape payloads that land together, not
  // network wait. Override with KINOWO_BG_CONCURRENCY if a bigger machine lands.
  // Injected (`injectedBackgroundBudget`) so EVERY country's wiring shares ONE
  // budget — one Semaphore, one cap across all of them — not one per country.
  lazy val backgroundBudget: ExecutionBudget = injectedBackgroundBudget

  // ── Events ────────────────────────────────────────────────────────────────
  // Per-country by construction (a fresh `InProcessEventBus` per wiring instance),
  // so one country's scrape/enrichment events never reach another's handlers.
  lazy val eventBus: EventBus = new InProcessEventBus()

  // ── Mongo ─────────────────────────────────────────────────────────────────
  // This country's Mongo database — explicit MONGODB_DB still wins for local dev,
  // else the country's own database. `protected def` so a test can read the
  // derivation without opening a connection.
  protected def mongoDbName: String = Country.dbNameFor(country)

  // The worker is the writer — Mongo is mandatory (opt out only for local dev
  // with MONGODB_OPTIONAL=true). Bound to THIS country's database, on the shared
  // `MongoClient` when WorkerMain injected one.
  lazy val mongoConnection: MongoConnection = {
    val optedOut = Env.flag("MONGODB_OPTIONAL")
    MongoConnection.fromEnvForDb(
      mongoDbName,
      required = MongoConnection.isRequired(testMode = false, optedOut = optedOut),
      sharedClient = sharedMongoClient)
  }

  /** The one clock a no-match `TmdbAttempt` is stamped from — `MovieService` on the
   *  movies path, `StagingSteps` on the staging path. The fixture harness pins it, so
   *  a replayed corpus is byte-identical across arrival orders. */
  lazy val clock: java.time.Clock = java.time.Clock.systemUTC()

  // ── Shared due schedules (eager) ──────────────────────────────────────────
  // The per-venue cadence override `scrapeDueWindow` reads and `ScrapeFreshnessPolicy`
  // writes after every landed scrape — see `VenueScrapeCadence`. Country-scoped
  // (this wiring IS one country), like `scrapeDueWindow` itself.
  val venueCadenceStore = new VenueCadenceStore(Freshness.defaultScrapeTtl)
  // ONE shared due schedule backs both the scrape reaper (enqueue) and the scrape
  // handler (pickup re-gate), so they agree on what's due and a cinema's scrapes
  // spread across the freshness window instead of falling due in a lockstep wave.
  // The period is PER-KEY (`venueCadenceStore.periodFor`), not the flat default:
  // most cinemas get the country's own cadence (the store's own fallback), but a
  // venue whose freshest listing runs dry sooner gets a shorter one — see
  // `VenueScrapeCadence`.
  val scrapeDueWindow = new DueWindow(venueCadenceStore.periodFor, Freshness.defaultScrapeTtl)
  // Shared detail refresh schedule. Its period IS the DetailEnrich TTL, read from
  // `Freshness.ttlFor` rather than repeated as a literal here: `CachingDetailFetch`'s
  // own TTL is defined as "shorter than this window" and pinned by a spec against
  // that same function, so a second copy of the number is a way for the window to
  // move while the guard keeps passing. The SAME instance backs the reaper (enqueue
  // gate) and the handler (pickup gate) so they agree on "due" — see
  // [[services.tasks.DueWindow]].
  val detailDueWindow = new DueWindow(
    Freshness.ttlFor(FreshnessKind.DetailEnrich)
      .getOrElse(throw new IllegalStateException(
        "DetailEnrich must carry a TTL: it is the detail refresh window the reaper, the handler " +
          "and CachingDetailFetch's TTL are all defined against.")))
  // The rating DueWindow resolves each key's period from its cadence stats instead
  // of a flat 4h (see RatingsWiring); the reaper + every handler share this one
  // instance so their due definitions agree.
  val ratingDueWindow = new DueWindow(
    key => RatingCadence.intervalFor(ratingCadenceStore.statsFor(key)),
    RatingCadence.BaseInterval
  )

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
  // A concluded newcomer folds into `movies` the moment the StagingFold handler
  // publishes — which rows are folded and what is announced afterwards is
  // `FoldOnStagingEnriched`'s decision (see its doc), not this root's.
  eventBus.subscribe(foldOnStagingEnriched.onStagingFilmEnriched)
  // The reaper advances the staging chain (detail → resolve → imdb → fold) one
  // step per finished staging task, and kicks a brand-new newcomer's first step
  // the moment it's diverted into `pending_movies` — so the whole chain runs off
  // events, with the periodic tick only a backstop for lost events / stalls.
  eventBus.subscribe(stagingReaper.onTaskFinished)
  eventBus.subscribe(stagingReaper.onNewcomerDiverted)
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
    // run at boot, but the heavy jobs are deferred off the boot window — the first
    // scrape pass (KINOWO_SCRAPE_INITIAL_DELAY_SECONDS) and the projector's orphan
    // prune (KINOWO_READMODEL_PRUNE_BOOT_DELAY_SECONDS).
    movieCache.start()
    // Publish this country's cache occupancy. Here rather than at construction
    // because it forces the lazy catalog, and a wiring that is built but never
    // started (tests, diagnostics) should not pay for a scraper graph.
    registerCacheMetrics()
    // Start the read-model projector after the cache so its state seed reads a
    // hydrated `movies` collection; it watches the change stream (with a persisted
    // resume token) independently of the cache's own watch.
    readModelProjector.start()
    // Ratings refresh via the queue (RatingHandlers + the EnrichmentReaper
    // backstop); refreshOneSync, which the handlers call, needs no start().
    unscreenedCleanup.start()
    strandedSideRowsCleanup.start()
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
    // Arm the last-resort restart backstop for a throttle spiral the backoff can't break.
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
    omdbBackfillReaper.foreach(_.start())
    scrapeReaper.start()
    // Backstop the chunked-scrape fan-in: recover complete runs whose completion
    // event was lost, and partial-reduce abandoned runs.
    chunkScrapeReaper.start()
    // Incubate pending_movies through the queue: newcomers and every step run off
    // events (subscribed above); this periodic tick only backstops stalled chains.
    // The TaskWorker (above) drains the steps.
    stagingReaper.start()
    stagingStuckAlerter.foreach(_.start())
    // Census the corpus for the /metrics gauges (off-band, read-only paged scan):
    // corpus coverage, per-city would-serve films (to overlay against the web's
    // read-model gauge) and per-city upcoming-showtime volume, all off ONE scan.
    // (The process-level jvmVitals sampler is started once by WorkerMain via the
    // shared WorkerMetrics bundle, not per-country here.)
    corpusScan.start()
    // Census the per-site never-run rating backlog (off-band, in-memory scan).
    ratingRunCensus.start()
    // Census the roster's worst-case scrape staleness (off-band, in-memory scan).
    cinemaScrapeCensus.start()
    cinemaContentCensus.start()
  }

  /** Event-cascade drain order, producer→consumer (see monolith comment). Only
   *  the async stages need draining: the TMDB stage and the IMDb-id resolver.
   *  Rating refresh is synchronous (queue-driven), so the *Ratings own no pool. */
  def cascadeDrainOrder: Seq[Drainable] = Seq(movieService, imdbIdResolver)

  def stop(): Unit = {
    envConfigService.stop()
    cinemaScrapeCensus.stop()
    cinemaContentCensus.stop()
    ratingRunCensus.stop()
    corpusScan.stop()
    // jvmVitals is process-level (shared WorkerMetrics bundle); WorkerMain stops it.
    stagingStuckAlerter.foreach(_.stop())
    stagingReaper.stop()
    scrapeReaper.stop()
    chunkScrapeReaper.stop()
    enrichmentReaper.stop()
    unresolvedTmdbReaper.stop()
    detailReaper.stop()
    settleReaper.stop()
    omdbBackfillReaper.foreach(_.stop())
    livenessWatchdog.stop()
    workerHeartbeat.stop()
    taskWorker.stop()
    taskQueue.close()
    freshnessStore.close()
    cascadeDrainOrder.foreach(_.stop())
    unscreenedCleanup.stop()
    strandedSideRowsCleanup.stop()
    readModelProjector.stop()
    movieCache.stop()
    readModelRepository.close()
    movieRepository.close()
    mongoConnection.close()
  }
}
