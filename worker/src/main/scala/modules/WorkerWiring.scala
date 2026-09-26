package modules

import org.mongodb.scala.MongoClient
import models.Country
import modules.wiring.{AlertingWiring, ChunkScrapeWiring, CorpusWiring, DetailWiring, EgressWiring, HttpWiring, IdentityCutoverWiring, InvariantAuditWiring, MetricsWiring, OperatorWiring, RatingsWiring, ReadModelWiring, ResolutionWiring, ScrapeWiring, ShareCardWiring, StagingWiring, TaskQueueWiring}
import services.cinemas.common.CinemaClientMarkers
import services.events.{EventBus, InProcessEventBus}
import services.freshness.{Freshness, FreshnessKind}
import services.{Drainable, MongoAddress, MongoConnection, MongoTuning, UptimeMonitor}
import settings.{BackgroundConcurrency, MongoDatabaseName, ProcessConfiguration, ScrapeFreshness}
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
    injectedBackgroundBudget: ExecutionBudget = SharedExecutionBudget.forBackground(WorkerWiring.DefaultBackgroundConcurrency),
    // ONE shared `MongoClient` across countries: each country binds its OWN
    // database view (`country.mongoDb`) on this single client. `None` → this
    // wiring builds (and closes) its own client at its `mongoAddress`, the
    // single-connection default.
    val sharedMongoClient: Option[MongoClient] = None,
    // The process-wide worker metrics bundle (ONE registry + one set of metric
    // objects, shared across every country's wiring — see WorkerMetrics). WorkerMain
    // builds it once over ALL countries and injects the SAME instance so every
    // country's series lands on the single `/metrics` registry. `None` → a lone
    // boot / test builds its own single-country bundle (resolved in `workerMetrics`
    // below). Kept an Option — mirroring `sharedMongoClient` — because a default
    // referencing `country` can't live in the same parameter clause as `country`.
    injectedWorkerMetrics: Option[WorkerMetrics] = None,
    // ONE poster-shrink gate across countries: the vips child it bounds shares the
    // process's memory cgroup, so `WorkerMain` builds it once and injects the SAME
    // instance into every country's wiring (see `VipsPosterShrinker`). Defaulted so a
    // single-country boot / test constructs its own.
    val posterShrinkGate: tools.PosterDecodeGate = services.sharecards.VipsPosterShrinker.newGate(),
    // The process's config (env vars + admin overrides) every knob in this wiring
    // reads. `WorkerMain` builds ONE and hands the same instance to every country's
    // wiring, so the override source EnvConfigService installs reaches all of them.
    // Defaulted to an EMPTY Env for test wirings — hermetic: every knob on its compiled-in
    // default, whatever the shell or `.env.local` holds. A run that wants the process's
    // (a convergence leg, a recorder) passes the one its root resolved.
    val env: Env = Env.of()) extends play.api.Logging
    with HttpWiring with EgressWiring with ScrapeWiring with ChunkScrapeWiring with DetailWiring
    with CorpusWiring with ResolutionWiring with RatingsWiring with ReadModelWiring
    with MetricsWiring with TaskQueueWiring with StagingWiring with AlertingWiring with OperatorWiring with ShareCardWiring
    with InvariantAuditWiring with IdentityCutoverWiring {

  /** Every setting this wiring reads, typed — resolved over its `env` (the process's in
   *  production, an empty one in a test wiring), so a flip installed into that Env reaches
   *  a value read per use. */
  lazy val configuration: ProcessConfiguration = new ProcessConfiguration(env)

  /** The metrics bundle this wiring records into: the shared injected one, or a
   *  self-owned single-country bundle when none was injected (lone boot / test). */
  val workerMetrics: WorkerMetrics =
    injectedWorkerMetrics.getOrElse(
      WorkerMetrics.singleCountry(country, workerPoolSize))
  lazy val uptimeMonitor = new UptimeMonitor(mongoConnection.database, clock = clock,
    ttlMismatches = workerMetrics.ttlIndexMismatches)

  // ── Identity observations (shadow) ──────────────────────────────────────────
  // The identity program's evidence store (docs/design/identity-resolver.md, "Phase 1"):
  // when `KINOWO_OBSERVATION_CAPTURE` is on, every identity lookup (`identityLookupFetch`), every venue
  // detail (`detailEnrichers`) and every scraped listing (`cinemaScrapeRunner`'s archive) is
  // also kept in this country's `obs_listings` / `obs_lookups`, which nothing serving reads.
  // OFF by default — a staged-migration switch, the one kind of flag the design allows — and
  // invisible either way: `ObservationCaptureEndToEndSpec` holds the corpus byte-identical
  // with it on. Retention needs no job: expiry is a TTL on a stamp the store computes.
  lazy val observationStore: Option[services.observations.ObservationStore] = capturedObservations
  /** The capture's store as the switch decides — what `observationStore` is unless a harness
   *  hands its own. */
  protected def capturedObservations: Option[services.observations.ObservationStore] =
    mongoConnection.database.filter(_ => configuration.observationCapture.value)
      .map(db => services.observations.MongoObservationBackend.store(db, clock, workerMetrics.ttlIndexMismatches))

  // ── Identity shadow run ─────────────────────────────────────────────────────
  // The identity resolver over the live corpus, on its own claimed schedule (`identityShadowSchedule`), from
  // the observations alone (docs/design/identity-resolver.md §8): `KINOWO_IDENTITY_SHADOW`, a
  // staged-migration switch, off by default. It writes only `identity_shadow_decisions` /
  // `identity_shadow_diff` and the `kinowo_worker_identity_*` gauges, and reaches no external
  // service — its lookups are the store's (`ObservedIdentityLookups`), so it answers from what the
  // capture (`KINOWO_OBSERVATION_CAPTURE`) filed. With the capture off it still reads what is
  // there; the store instance is the capture's when both are on.
  lazy val identityObservations: Option[services.observations.ObservationStore] =
    observationStore.orElse(mongoConnection.database.filter(_ => configuration.identityShadow.value)
      .map(db => services.observations.MongoObservationBackend.store(db, clock, workerMetrics.ttlIndexMismatches)))

  /** Where the shadow run persists its runs: this country's shadow collections (in memory
   *  without a database). */
  lazy val shadowRuns: services.identity.ShadowRunStore = new services.identity.ShadowRunStore(
    mongoConnection.database.fold[services.identity.ShadowRunBackend](new services.identity.InMemoryShadowRunBackend)(
      services.identity.MongoShadowRunBackend.writer(_, workerMetrics.ttlIndexMismatches)), clock)

  lazy val shadowIdentityReaper: Option[services.identity.ShadowIdentityReaper] = {
    import services.identity._
    identityObservations.filter(_ => configuration.identityShadow.value && !identityCutover).map(store => new ShadowIdentityReaper(
      listings      = shadowListings,
      pipelineFilms = () => movieCache.snapshot(),
      lookups       = () => ObservedIdentityLookups.over(store, tmdbClientOver, detailEnrichers),
      pins          = new MongoPinStore(mongoConnection.database),
      normalizer    = titleNormalizer,
      calibration   = IdentityCalibration.default,
      runs          = shadowRuns,
      retention     = ShadowRetention(services.observations.ObservationRetention.Window),
      metrics       = workerMetrics.identityShadow.forCountry(country.code),
      clock         = clock))
  }

  // The shadow run's OWN schedule — not the settle's: the settle is a self-heal near-no-op, and the
  // shadow diffs against the pipeline's films as they are when it ticks (`movieCache.snapshot()`).
  // One claimed window per `KINOWO_IDENTITY_SHADOW_INTERVAL_SECONDS` (30 min), first a few minutes
  // after boot so the hydrate has loaded the films it diffs against. Each tick is followed by a
  // fill round when the fill is on.
  def identityShadowInterval: settings.IdentityShadowInterval =
    configuration.identityShadowInterval(WorkerWiring.DefaultIdentityShadowInterval)
  def identityShadowTick(): Unit = shadowIdentityReaper.foreach { reaper =>
    reaper.tickQuietly()
    shadowLookupFill.foreach(_.start())
  }
  lazy val identityShadowSchedule: Option[services.tasks.ClaimedPeriodicTask] = shadowIdentityReaper.map(_ =>
    new services.tasks.ClaimedPeriodicTask("identity-shadow", () => identityShadowTick(), identityShadowInterval.value,
      configuration.identityShadowInitialDelay(WorkerWiring.DefaultIdentityShadowInitialDelay).value, scheduledRunStore, clock))

  // The shadow run's PACED LIVE LOOKUP FILL (docs/design/identity-resolver.md §19):
  // `KINOWO_IDENTITY_SHADOW_LOOKUPS`, a staged-migration switch, off by default, and only with the
  // shadow run on. After each shadow tick it asks the resolver's unobserved TMDB questions through
  // the pipeline's own lookup chain (`enrichmentFetch`: its 429 gate, breaker and pace), at most
  // `KINOWO_IDENTITY_SHADOW_LOOKUP_RATE` per minute, and files the answers ONLY in the observation
  // store. The default cap is 60/min: about 2% of TMDB's ~50 req/s ceiling.
  lazy val shadowLookupFill: Option[services.identity.ShadowLookupFill] =
    shadowIdentityReaper.flatMap(_ => identityObservations).filter(_ => configuration.identityShadowLookups.value).map(store =>
      new services.identity.ShadowLookupFill(
        listings    = shadowListings,
        store       = store,
        tmdb        = tmdbClientOver,
        liveFetch   = enrichmentFetch,
        enrichers   = detailEnrichers,
        normalizer  = titleNormalizer,
        calibration = services.identity.IdentityCalibration.default,
        rate        = configuration.identityShadowLookupRate(WorkerWiring.DefaultShadowLookupRate),
        window      = identityShadowInterval,
        metrics     = workerMetrics.identityShadow.lookupsForCountry(country.code),
        executor    = shadowLookupExecutor,
        sleep       = shadowLookupSleep))

  /** How the fill waits its pace between asks: real time, except in a replay harness. */
  protected def shadowLookupSleep: Long => Unit = Thread.sleep

  /** One daemon thread for the fill's rounds, which sleep their pace between asks. */
  protected lazy val shadowLookupExecutor: java.util.concurrent.ExecutorService =
    tools.DaemonExecutors.boundedEC(s"identity-shadow-lookups-${country.code}", 1)

  /** The shadow run's listing set: every listing of the scrape archive's latest scrape per live venue. */
  def shadowListings(): Seq[services.identity.Listing] = {
    val live = cinemaScrapers.map(_.cinema).toSet
    services.identity.Listing.corpus(scrapeArchive.findAll().filter(row => live(row.cinema)).map(row => row.cinema -> row.films), titleNormalizer)
  }

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
  // Where this worker's Mongo is — resolved HERE, the composition root, and nowhere below
  // it. The local stack overrides it with its own local address instead of rewriting the
  // process's MONGODB_URI.
  lazy val mongoAddress: MongoAddress = configuration.mongoAddress

  // This country's Mongo database — an explicit database on the address still wins for
  // local dev, else the country's own. `protected def` so a test can read the derivation
  // without opening a connection.
  protected def mongoDbName: MongoDatabaseName = mongoAddress.databaseFor(country)

  // The worker is the writer — Mongo is mandatory (opt out only for local dev
  // with MONGODB_OPTIONAL=true). Bound to THIS country's database, on the shared
  // `MongoClient` when WorkerMain injected one — and claimed for this country before
  // anything can prune or write against it (see [[services.DatabaseOwner]]).
  lazy val mongoConnection: MongoConnection = {
    MongoConnection.forCountry(country, mongoAddress.copy(database = Some(mongoDbName)),
      required = MongoConnection.isRequired(testMode = false, optedOut = configuration.mongoOptional),
      tuning = MongoTuning.from(configuration), sharedClient = sharedMongoClient)
  }

  /** The one clock a no-match `TmdbAttempt` is stamped from — `MovieService` on the
   *  movies path, `StagingSteps` on the staging path. The fixture harness pins it, so
   *  a replayed corpus is byte-identical across arrival orders. */
  lazy val clock: java.time.Clock = java.time.Clock.systemUTC()

  // ── Shared due schedules (eager) ──────────────────────────────────────────
  // The per-venue cadence override `scrapeDueWindow` reads and `ScrapeFreshnessPolicy`
  // writes after every landed scrape — see `VenueScrapeCadence`. Country-scoped
  // (this wiring IS one country), like `scrapeDueWindow` itself.
  /** `KINOWO_SCRAPE_FRESHNESS_MINUTES` — how long a venue's scrape stays fresh. */
  lazy val scrapeFreshness: ScrapeFreshness = configuration.scrapeFreshness(Freshness.DefaultScrapeFreshness)
  val venueCadenceStore = new VenueCadenceStore(scrapeFreshness)
  // ONE shared due schedule backs both the scrape reaper (enqueue) and the scrape
  // handler (pickup re-gate), so they agree on what's due and a cinema's scrapes
  // spread across the freshness window instead of falling due in a lockstep wave.
  // The period is PER-KEY (`venueCadenceStore.periodFor`), not the flat default:
  // most cinemas get the country's own cadence (the store's own fallback), but a
  // venue whose freshest listing runs dry sooner gets a shorter one — see
  // `VenueScrapeCadence`.
  val scrapeDueWindow = new DueWindow(venueCadenceStore.periodFor, scrapeFreshness.value)
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
  // A finished share-card render re-projects its film, so `web_movies` points at the new card
  // (and a film the first-publish gate holds is published).
  eventBus.subscribe(shareCardFollowUp.onTaskFinished)

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
    // A cut-over country's identity is the projection's (`IdentityCutoverWiring`): the old path's
    // reapers — the TMDB retry / concluding and the deferred detail that feeds the TMDB resolve —
    // are not started, and neither is staging's below.
    if (!identityCutover) { unresolvedTmdbReaper.start(); detailReaper.start() }
    settleReaper.start()
    identityShadowSchedule.foreach(_.start())
    omdbBackfillReaper.foreach(_.start())
    shareCardReapers.foreach(_.start())
    startFacebookRescrapes()
    auditReapers.foreach(_.start())
    scrapeReaper.start()
    // Backstop the chunked-scrape fan-in: recover complete runs whose completion
    // event was lost, and partial-reduce abandoned runs.
    chunkScrapeReaper.start()
    // Incubate pending_movies through the queue: newcomers and every step run off
    // events (subscribed above); this periodic tick only backstops stalled chains.
    // The TaskWorker (above) drains the steps.
    if (!identityCutover) { stagingReaper.start(); stagingStuckAlerter.foreach(_.start()) }
    // Say so, loudly, for any alerter a missing env var has wired off (gauge + WARN).
    reportAlerters()
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
    retiredVenueCensus.start()
    unstampedListingCensus.start()
    listingKeyShadowRead.foreach(_.start())
  }

  /** Event-cascade drain order, producer→consumer (see monolith comment). Only
   *  the async stages need draining: the TMDB stage and the IMDb-id resolver.
   *  Rating refresh is synchronous (queue-driven), so the *Ratings own no pool. */
  def cascadeDrainOrder: Seq[Drainable] = Seq(movieService, imdbIdResolver)

  def stop(): Unit = {
    shadowLookupFill.foreach(_ => shadowLookupExecutor.shutdownNow())
    envConfigService.stop()
    cinemaScrapeCensus.stop()
    cinemaContentCensus.stop()
    retiredVenueCensus.stop()
    unstampedListingCensus.stop()
    listingKeyShadowRead.foreach(_.stop())
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
    identityShadowSchedule.foreach(_.stop())
    omdbBackfillReaper.foreach(_.stop())
    shareCardReapers.foreach(_.stop())
    stopFacebookRescrapes()
    auditReapers.foreach(_.stop())
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

object WorkerWiring {

  /** The background concurrency budget every country's wiring shares, sized by
   *  `KINOWO_BG_CONCURRENCY` (default 4 — see `backgroundBudget`). */
  /** `KINOWO_BG_CONCURRENCY`'s compiled-in default. */
  val DefaultBackgroundConcurrency: BackgroundConcurrency = BackgroundConcurrency(4)

  /** `KINOWO_IDENTITY_SHADOW_LOOKUP_RATE`'s compiled-in default: 60 asks a minute, about 2% of
   *  TMDB's ~50 req/s ceiling, so the pipeline's own lookups keep the rest (see `shadowLookupFill`). */
  val DefaultShadowLookupRate: settings.IdentityShadowLookupRate = settings.IdentityShadowLookupRate(60)

  /** The identity shadow run's cadence: the settle's former 30 minutes, which its cost (§17: at
   *  most seconds a tick) and the fill's per-round allowance were measured against. */
  val DefaultIdentityShadowInterval: settings.IdentityShadowInterval =
    settings.IdentityShadowInterval(scala.concurrent.duration.Duration(30, "minutes"))
  /** Long enough after boot for the synchronous hydrate to have loaded the films the diff reads. */
  val DefaultIdentityShadowInitialDelay: settings.IdentityShadowInitialDelay =
    settings.IdentityShadowInitialDelay(scala.concurrent.duration.Duration(5, "minutes"))

  /** The ONE background budget a process shares across its countries' wirings. */
  def backgroundBudgetFrom(configuration: ProcessConfiguration): ExecutionBudget =
    SharedExecutionBudget.forBackground(configuration.backgroundConcurrency(DefaultBackgroundConcurrency))
}
