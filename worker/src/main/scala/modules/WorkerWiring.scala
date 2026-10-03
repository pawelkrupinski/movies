package modules

import org.mongodb.scala.MongoClient
import models.Country
import modules.wiring.{AlertingWiring, ChunkScrapeWiring, CorpusWiring, DetailWiring, EgressWiring, HttpWiring, IdentityCutoverWiring, InvariantAuditWiring, MetricsWiring, OperatorWiring, RatingsWiring, ReadModelWiring, ResolutionWiring, ScrapeWiring, ShareCardWiring, TaskQueueWiring}
import services.cinemas.common.CinemaClientMarkers
import services.events.{EventBus, InProcessEventBus}
import services.freshness.{Freshness, FreshnessKind}
import services.{Drainable, MongoAddress, MongoConnection, MongoTuning, UptimeMonitor}
import settings.{BackgroundConcurrency, MongoDatabaseName, ProcessConfiguration, ScrapeFreshness}
import services.cadence.RatingCadence
import services.metrics.WorkerMetrics
import services.tasks.{CostSpacedPhaseOffset, DueWindow, ScrapeCadence, VenueCadenceStore}
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
    with MetricsWiring with TaskQueueWiring with AlertingWiring with OperatorWiring with ShareCardWiring
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

  // ── The incremental identity model ─────────────────────────────────────────
  // The resolver kept current by events (docs/design/identity-resolver.md §20) instead of a
  // whole-corpus resolve per tick: each venue's accepted listing as the intake publishes it, each TMDB
  // answer the store files and each venue page the enrichment reads, drained on one thread; its
  // families persist in `identity_model_families`, so a restart takes them up and re-resolves only what
  // moved while it was down. The projection (`IdentityCutoverWiring`) writes its films.
  lazy val identityReads: services.identity.ObservationReads = new services.identity.ObservationReads

  // The identity model's TMDB and IMDb answers, normalized as they are fetched (`TmdbStore`): a film,
  // a person and a question each once, as native BSON holding only what the resolver reads. Filled
  // by the pipeline's client (`identityLookupFetch`) and the fill; read by the model. Where the model
  // runs, in the country's database.
  lazy val identityTmdbDocuments: Option[services.identity.TmdbDocuments] =
    Some(mongoConnection.database.fold[services.identity.TmdbDocuments](new services.identity.InMemoryTmdbDocuments)(db =>
      new services.identity.CoalescedTmdbDocuments(new services.identity.MongoTmdbDocuments(db))))
  lazy val identityTmdbStore: Option[services.identity.TmdbStore] = identityTmdbDocuments.map(new services.identity.TmdbStore(_, clock))
  lazy val identityTmdbNormalizer: Option[services.identity.TmdbNormalizer] = identityTmdbStore.map(new services.identity.TmdbNormalizer(_, tmdbJsonBodies))
  /** Keeps the store current from TMDB's change lists (`TmdbChangesSweep`), on demand: before a fill
   *  round, whenever the last complete sweep is from before today. */
  lazy val identityTmdbChanges: Option[services.identity.TmdbChangesSweep] =
    for { store <- identityTmdbStore; docs <- identityTmdbDocuments; normalizer <- identityTmdbNormalizer }
    yield new services.identity.TmdbChangesSweep(store, docs,
      tmdbClientOver(new services.identity.NormalizingHttpFetch(enrichmentFetch, normalizer)), country.language.toLanguageTag, clock)
  /** The model's questions due to be asked again (`TmdbRefreshes`). */
  lazy val identityTmdbRefreshes: Option[services.identity.TmdbRefreshes] =
    identityTmdbStore.map(new services.identity.TmdbRefreshes(_, country.language.toLanguageTag, clock))
  lazy val identityModel: Option[services.identity.IdentityModelService] = {
    import services.identity._
    // The model is the country's identity: the intake's accepted listings, answered from the model's
    // store and asked live on a gap (`cutoverLookups`).
    val sources: Option[(() => Seq[Listing], () => IdentityLookups)] = Some((
      () => Listing.distinct(Listing.all(identityListingIntake.listings(cinemaScrapers.map(_.cinema)), titleNormalizer)),
      () => cutoverLookups(identityReads)))
    sources.map { case (listings, lookups) =>
      val pinStore = new MongoPinStore(mongoConnection.database)
      var tracked  = Option.empty[TrackedLookups]
      val model = new IdentityModelService(
        newModel   = () => {
          val pins = services.movies.ListingConstraints.pinned(pinStore.all())
          val lookupsNow = new TrackedLookups(lookups(), identityReads, Some(identityPrefetchPool))
          tracked = Some(lookupsNow)
          new IncrementalResolver(lookupsNow, titleNormalizer, IdentityCalibration.resolver, pins,
            store = mongoConnection.database.fold[IdentityModelStore](new InMemoryIdentityModelStore)(new MongoIdentityModelStore(_)),
            rules = IncrementalResolver.rulesVersion(IdentityRules.codeVersion, IdentityCalibration.resolver, TitleDecorations.resolver, pins),
            traces = identityTraces)
        },
        reads      = identityReads,
        archive    = listings,
        normalizer = titleNormalizer,
        settle     = WorkerWiring.IdentityModelSettle,
        scheduler  = identityModelScheduler,
        metrics    = workerMetrics.identityModel.forCountry(country.code),
        reading    = () => tracked.fold("")(_.render),
        beforeDrain = () => venuePageIndex.settle(),
        // A new listing waits for its venue page, read into venue_pages by a ReadVenuePage task, so
        // its first resolve has the page's facts.
        pageWait   = new services.identity.VenuePageWait(detailEnrichers, venuePageIndex, taskQueue, freshnessStore,
                       WorkerWiring.VenuePageWaitLimit),
        clock      = clock)
      identityTmdbStore.foreach(_.onChanged(model.observed))
      model
    }
  }
  /** The model's TMDB and IMDb answers from its normalized store, and venue details from venue_pages
   *  ([[venuePageIndex]]) — what the shadow model reads, and what a cut-over one reads first. */
  def storedLookups(reads: services.identity.ObservationReads = services.identity.ObservationReads.Untracked): services.identity.IdentityLookups =
    new services.identity.StoredTmdbLookups(identityTmdbStore.getOrElse(new services.identity.TmdbStore(new services.identity.InMemoryTmdbDocuments, clock)),
      country.language.toLanguageTag, new services.identity.VenueDetailLookups(detailEnrichers, venuePageIndex, reads), reads, Some(identityProposals))

  /** What a language model proposed listings no rule took are (`identity_proposals`), as the model reads them: a new
   *  proposal re-resolves exactly the listings of its title. Read whether or not this worker asks the model. */
  lazy val identityProposals: services.identity.ProposalIndex =
    new services.identity.ProposalIndex(mongoConnection.database.fold[services.identity.ProposalStore](new services.identity.InMemoryProposalStore)(
      new services.identity.MongoProposalStore(_)), changed = key => identityModel.foreach(_.observed(key)))

  /** Asks the model about the unresolved listings' titles each round (`ProposalFill`) — only with `ANTHROPIC_API_KEY`
   *  set (`GatedIntegration.IdentityProposals`) and traces to read them from. */
  lazy val identityProposalFill: Option[services.identity.ProposalFill] =
    for { key <- configuration.anthropicApiKey; db <- mongoConnection.database }
    yield new services.identity.ProposalFill(new services.identity.MongoIdentityTraceReads(db), identityProposals,
      new services.identity.AnthropicProposer(key), clock)
  lazy val identityProposalSchedule: Option[services.tasks.ClaimedPeriodicTask] = identityProposalFill.map(fill =>
    new services.tasks.ClaimedPeriodicTask("identity-proposals", () => { fill.round(); () }, WorkerWiring.ProposalInterval,
      WorkerWiring.ProposalInitialDelay, scheduledRunStore, clock))

  /** A cut-over model's lookups: [[storedLookups]] first, and a TMDB or IMDb question the store has no
   *  answer to asked live through `identityLookupFetch`, which files the answer into the store. */
  def cutoverLookups(reads: services.identity.ObservationReads = services.identity.ObservationReads.Untracked): services.identity.IdentityLookups =
    new services.identity.StoredFirstLookups(storedLookups(reads),
      new services.identity.TmdbIdentityLookups(tmdbClientOver(identityLookupFetch), new services.enrichment.ImdbClient(identityLookupFetch), Nil))

  /** `venue_pages`: every venue detail page read, written once by page (`VenuePageReader`). */
  lazy val venuePageStore: services.venuepages.VenuePageStore =
    mongoConnection.database.fold[services.venuepages.VenuePageStore](new services.venuepages.InMemoryVenuePageStore)(
      new services.venuepages.MongoVenuePageStore(_))

  /** The venue detail pages read into venue_pages, as the identity model reads them. */
  lazy val venuePageIndex: services.identity.VenuePageIndex =
    new services.identity.VenuePageIndex(venuePageStore, changed = key => identityModel.foreach(_.observed(key)))

  /** Where the model files which rules decided each listing (`identity_traces`, read by the admin
   *  page only): beside its families in the country's database, or nowhere without one. A def, so each
   *  model a rebuild makes gets its own writer, as it always has. */
  protected def identityTraces: services.identity.IdentityTraceStore =
    mongoConnection.database.fold[services.identity.IdentityTraceStore](services.identity.IdentityTraceStore.Discard)(
      new services.identity.MongoIdentityTraceStore(_))

  /** The threads the model's lookups prefetch on: each question waits on store round-trips, so a
   *  take-up is bound by how many are in flight, not by CPU. Virtual, and many: the store coalesces
   *  the reads and writes they file answers with into one round-trip per batch
   *  (`CoalescedTmdbDocuments`), so they share connections of the pool instead of each holding one. */
  protected lazy val identityPrefetchPool: java.util.concurrent.ExecutorService =
    java.util.concurrent.Executors.newFixedThreadPool(WorkerWiring.IdentityPrefetchThreads,
      Thread.ofVirtual().name(s"identity-prefetch-${country.code}-", 0).factory())

  /** One daemon thread for the model: it is not thread-safe, and every event is drained on it. */
  protected lazy val identityModelScheduler: java.util.concurrent.ScheduledExecutorService =
    java.util.concurrent.Executors.newSingleThreadScheduledExecutor { (task: Runnable) =>
      val thread = new Thread(task, s"identity-model-${country.code}"); thread.setDaemon(true); thread }

  // Once a day, every venue through `VenueClosure`: a newly confirmed closure pages once on
  // the fallback channel and, for a data-driven roster (DE/ES/US), starts the retire-venues
  // workflow, which re-checks it live and opens the PR removing it. Without
  // KINOWO_GITHUB_DISPATCH_TOKEN the page asks for a hand retirement instead.
  lazy val closureLedger: services.closure.ClosureLedger =
    mongoConnection.database.fold[services.closure.ClosureLedger](new services.closure.InMemoryClosureLedger)(new services.closure.MongoClosureLedger(_))
  lazy val closureSweep = new services.closure.ClosureSweep(() => closureCandidates, scrapeArchive, filmwebFallbackStore,
    closureLedger, fallbackPager(services.alerts.TelegramAlertKind.VenueClosure),
    configuration.githubDispatchToken.map(token => new services.closure.GitHubRetirementDispatch(token,
      java.net.http.HttpClient.newBuilder().connectTimeout(java.time.Duration.ofSeconds(10)).sslContext(tlsContext).build())),
    clock)
  lazy val closureSchedule = new services.tasks.ClaimedPeriodicTask("venue-closure", () => closureSweep.sweep(),
    WorkerWiring.ClosureSweepInterval, WorkerWiring.ClosureSweepInitialDelay, scheduledRunStore, clock)

  // The model's PACED LIVE LOOKUP FILL (docs/design/identity-resolver.md §19), on its own claimed
  // schedule (`identityLookupRefreshSchedule`): the model asks live only what its store lacks, so an
  // answer the store holds is renewed only here — a search that found nothing before TMDB had the film
  // (`TmdbRefreshes`), a record TMDB changed since (`TmdbChangesSweep`) — and any question still open.
  // Through the lookup chain (`enrichmentFetch`: its 429 gate, breaker and pace), at most
  // `KINOWO_IDENTITY_SHADOW_LOOKUP_RATE` per minute, the answers filed in the model's TMDB store.
  lazy val shadowLookupFill: Option[services.identity.ShadowLookupFill] =
    for (normalizer <- identityTmdbNormalizer) yield
      new services.identity.ShadowLookupFill(
        questions   = () => identityModel.flatMap(_.peek(WorkerWiring.IdentityModelPeek)).fold(services.identity.AnswersChanged.Empty)(_.gaps),
        tmdb        = tmdbClientOver,
        liveFetch   = enrichmentFetch,
        normalizer  = normalizer,
        beforeRound = () => identityTmdbChanges.filter(_.behind).foreach(_.sweep()),
        gapMemory   = identityTmdbDocuments.map(new services.identity.TmdbGapMemory(_, country.language.toLanguageTag, clock)),
        refreshes   = () => identityTmdbRefreshes.fold(Seq.empty[services.identity.CandidateQuery])(r =>
          identityModel.flatMap(_.peek(WorkerWiring.IdentityModelPeek)).fold(Seq.empty[services.identity.CandidateQuery])(s => r.due(s.questions))),
        rate        = configuration.identityShadowLookupRate(WorkerWiring.DefaultShadowLookupRate),
        window      = identityShadowInterval,
        metrics     = workerMetrics.identityShadow.lookupsForCountry(country.code),
        executor    = shadowLookupExecutor,
        sleep       = shadowLookupSleep)

  def identityShadowInterval: settings.IdentityShadowInterval =
    configuration.identityShadowInterval(WorkerWiring.DefaultIdentityShadowInterval)
  /** The fill's rounds, every `KINOWO_IDENTITY_SHADOW_INTERVAL_SECONDS` (30 minutes). */
  lazy val identityLookupRefreshSchedule: Option[services.tasks.ClaimedPeriodicTask] =
    shadowLookupFill.map(fill =>
      new services.tasks.ClaimedPeriodicTask("identity-lookup-refresh", () => fill.start(), identityShadowInterval.value,
        configuration.identityShadowInitialDelay(WorkerWiring.DefaultIdentityShadowInitialDelay).value, scheduledRunStore, clock))

  /** How the fill waits its pace between asks: real time, except in a replay harness. */
  protected def shadowLookupSleep: Long => Unit = Thread.sleep

  /** One daemon thread for the fill's rounds, which sleep their pace between asks. */
  protected lazy val shadowLookupExecutor: java.util.concurrent.ExecutorService =
    tools.DaemonExecutors.boundedEC(s"identity-shadow-lookups-${country.code}", 1)

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
  //
  // Its phases are spaced by each cinema's measured COST rather than hashed evenly by
  // count, so heavy chunked venues don't land together and spike the queue — see
  // `CostSpacedPhaseOffset`; `scrapePhasePlanner` (ScrapeWiring) keeps the plan fresh.
  // Because a re-plan moves phases, a scrape counts toward its nearest boundary.
  val scrapePhases    = new CostSpacedPhaseOffset
  val scrapeDueWindow = new DueWindow(venueCadenceStore.periodFor, scrapeFreshness.value, scrapePhases, ScrapeCadence.Counting)
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
  // A venue page read into venue_pages: its answer is the model's now, and the next settle tells the
  // model of every page whose answer that changed.
  eventBus.subscribe { case services.events.VenueDetailRead(group, page) => venuePageIndex.pageRead(group, page) }
  eventBus.subscribe(imdbIdResolver.onImdbIdMissing)
  // The coordinator enqueues a chunked scrape's reduce once its last chunk task
  // finishes (the ChunkScrapeReaper backstop covers lost completions).
  eventBus.subscribe(chunkScrapeCoordinator.onTaskFinished)
  // A finished share-card render re-projects its film, so `web_movies` points at the new card
  // (and a film the first-publish gate holds is published).
  eventBus.subscribe(shareCardFollowUp.onTaskFinished)

  // The boot work a worker's readiness waits on (`BootReadiness`): the projector's prepare and the
  // identity take-up, both off the boot thread. Settled when each has finished, whether or not it
  // succeeded — a failure is logged where it happens, and waiting on it would only stall rollouts.
  @volatile private var projectorPrepared = false
  private val bootStarted = new java.util.concurrent.atomic.AtomicReference[Option[tools.Stopwatch.Started]](None)
  private val settledLogged = new java.util.concurrent.atomic.AtomicBoolean(false)

  /** Whether this country's boot work has settled: started, projector prepared, model taken up. */
  def bootSettled: Boolean = {
    val settled = bootStarted.get.isDefined && projectorPrepared && identityModel.forall(_.takeUpSettled)
    if (settled && settledLogged.compareAndSet(false, true))
      bootStarted.get.foreach(started => logger.info(f"[${country.code}] boot work settled in ${started.seconds}%.1fs"))
    settled
  }

  def start(): Unit = {
    bootStarted.set(Some(tools.Stopwatch.start()))
    val boot = new BootSteps(country.code)
    // Force Mongo at boot so connection errors surface in the boot timeline.
    boot.step("mongo")(mongoConnection.database)
    // Install the override source first so boot-time knob reads already see flips.
    boot.step("env config")(envConfigService.start())
    // The identity model's take-up (the longest part of a boot: 117–259 s on US) reads only the
    // scrape archive, the TMDB/observation stores, the pins and its own families — nothing the cache
    // hydrate or the projector produce — and `start` only schedules it on the model's own thread, so
    // it begins now rather than after the TaskWorker.
    boot.step("identity model")(identityModel.foreach(_.start()))
    // The read-model projector's boot reads (its state seed and missing-card heal: 28–57 s of a US
    // boot) read the read model and the `movies` store, never the cache, so they start NOW, beside
    // the cache hydrate, instead of after it with everything else waiting behind them. It attaches
    // to the change stream HERE, before the cache opens it: attached only once its reads were done,
    // it missed every change the stream applied to the cache first (2026-09-30: four first-sweep
    // heals, and a closed Chicago venue served for 22 hours). What arrives while its reads run is
    // held and projected after them; its sweeps still start only once the cache has.
    readModelProjector.attach()
    val cacheStarted = new java.util.concurrent.CountDownLatch(1)
    boot.inBackground("read-model projector") {
      try readModelProjector.prepare() finally projectorPrepared = true
      cacheStarted.await()
      readModelProjector.watch()
    }
    // The cache hydrate (synchronous findAll — the first scrape tick needs a populated cache for
    // sibling/redirect checks). The heavy jobs stay deferred off the boot window: the first
    // scrape pass (KINOWO_SCRAPE_INITIAL_DELAY_SECONDS) and the projector's orphan prune
    // (KINOWO_READMODEL_PRUNE_BOOT_DELAY_SECONDS).
    try boot.step("movie cache hydrate")(movieCache.start()) finally cacheStarted.countDown()
    // Publish this country's cache occupancy. Here rather than at construction
    // because it forces the lazy catalog, and a wiring that is built but never
    // started (tests, diagnostics) should not pay for a scraper graph.
    boot.step("cache metrics")(registerCacheMetrics())
    // Ratings refresh via the queue (RatingHandlers + the EnrichmentReaper
    // backstop); refreshOneSync, which the handlers call, needs no start().
    boot.step("unscreened cleanup")(unscreenedCleanup.start())
    boot.step("stranded side rows")(strandedSideRowsCleanup.start())
    // Tag each cinema with its scraper-client marker (shared platform client vs a
    // bespoke one) plus the FtFW chip if it's already in Filmweb fallback at boot
    // (transitions only fire on change, so an in-flight fallback would otherwise go
    // untagged until it next flips). Same rationale as above — the catalog is
    // worker-only, so the tags ride the UptimeMonitor tag channel.
    boot.step("client markers")(clientMarkers.foreach { case (cinema, marker) =>
      val inFallback = filmwebFallbackStore.get(cinema).exists(_.active)
      uptimeMonitor.tagService(cinema, CinemaClientMarkers.tagsFor(Some(marker), sourceUrls.get(cinema), inFallback))
    })
    // The task worker drains all queue work: scraping, deferred detail, and
    // queue-driven rating enrichment.
    boot.step("task worker")({ taskWorker.start(); workerHeartbeat.start() })
    // Arm AFTER the heartbeat (so the first pulse is already stamped) — restart a
    // wedged-but-alive JVM the throttle watchdog can't see.
    boot.step("liveness watchdog")(livenessWatchdog.start())
    boot.step("enrichment reaper")(enrichmentReaper.start())
    // A cut-over country's identity is the projection's (`IdentityCutoverWiring`): the old path's
    // reapers — the TMDB retry / concluding and the deferred detail that feeds the TMDB resolve —
    // are not started, and neither is staging's below.
    // A cut-over country still enriches venue detail pages (the model reads them from the slots);
    // only the TMDB re-try sweep is the old identity path's.
    boot.step("detail reaper")(detailReaper.start())
    boot.step("settle reaper")(settleReaper.start())
    boot.step("identity lookup refresh")(identityLookupRefreshSchedule.foreach(_.start()))
    boot.step("closure schedule")(closureSchedule.start())
    boot.step("identity proposals")(identityProposalSchedule.foreach(_.start()))
    boot.step("omdb backfill")(omdbBackfillReaper.foreach(_.start()))
    boot.step("share cards")(shareCardReapers.foreach(_.start()))
    boot.step("facebook rescrapes")(startFacebookRescrapes())
    boot.step("audit reapers")(auditReapers.foreach(_.start()))
    boot.step("scrape phase planner")(scrapePhasePlanner.start())
    boot.step("scrape reaper")(scrapeReaper.start())
    // Backstop the chunked-scrape fan-in: recover complete runs whose completion
    // event was lost, and partial-reduce abandoned runs.
    boot.step("chunk scrape reaper")(chunkScrapeReaper.start())
    // Incubate pending_movies through the queue: newcomers and every step run off
    // events (subscribed above); this periodic tick only backstops stalled chains.
    // The TaskWorker (above) drains the steps.
    // Say so, loudly, for any alerter a missing env var has wired off (gauge + WARN).
    boot.step("alerters")(reportAlerters())
    // Census the corpus for the /metrics gauges (off-band, read-only paged scan):
    // corpus coverage, per-city would-serve films (to overlay against the web's
    // read-model gauge) and per-city upcoming-showtime volume, all off ONE scan.
    // (The process-level jvmVitals sampler is started once by WorkerMain via the
    // shared WorkerMetrics bundle, not per-country here.)
    boot.step("corpus scan")(corpusScan.start())
    // Find the copied feeds that predate this boot, from each venue's latest archived scrape.
    boot.step("copied feed seed")(copiedFeedDetector.foreach(_.start(scrapeArchive, services.cinemas.roster.CopiedFeedDetector.SeedDelay)))
    // Census the per-site never-run rating backlog (off-band, in-memory scan).
    boot.step("rating run census")(ratingRunCensus.start())
    // Census the roster's worst-case scrape staleness (off-band, in-memory scan).
    boot.step("scrape census")(cinemaScrapeCensus.start())
    boot.step("content census")(cinemaContentCensus.start())
    boot.step("retired venue census")(retiredVenueCensus.start())
    logger.info(boot.summary)
  }

  /** Event-cascade drain order, producer→consumer (see monolith comment). Only the async stage
   *  needs draining: the IMDb-id resolver. Rating refresh is synchronous (queue-driven), so the
   *  *Ratings own no pool. */
  def cascadeDrainOrder: Seq[Drainable] = Seq(imdbIdResolver)

  def stop(): Unit = {
    shadowLookupFill.foreach(_ => shadowLookupExecutor.shutdownNow())
    envConfigService.stop()
    cinemaScrapeCensus.stop()
    cinemaContentCensus.stop()
    retiredVenueCensus.stop()
    ratingRunCensus.stop()
    corpusScan.stop()
    // jvmVitals is process-level (shared WorkerMetrics bundle); WorkerMain stops it.
    scrapeReaper.stop()
    scrapePhasePlanner.stop()
    chunkScrapeReaper.stop()
    enrichmentReaper.stop()
    detailReaper.stop()
    settleReaper.stop()
    identityLookupRefreshSchedule.foreach(_.stop())
    closureSchedule.stop()
    identityProposalSchedule.foreach(_.stop())
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

  /** How long the fill waits for the model's thread to catch up and hand it a snapshot — a drain,
   *  never a take-up. */
  val IdentityModelPeek: scala.concurrent.duration.FiniteDuration = scala.concurrent.duration.Duration(2, "minutes")

  /** How long the identity model lets events gather before one drain takes them together: a venue
   *  scraped twice, or a family several venues touch, in that window is resolved once. */
  val IdentityModelSettle: scala.concurrent.duration.FiniteDuration = scala.concurrent.duration.Duration(10, "seconds")
  /** How long a cut-over country's new listing waits for its venue page before it is taken in without it. */
  val VenuePageWaitLimit: scala.concurrent.duration.FiniteDuration = scala.concurrent.duration.Duration(1, "hour")
  /** Questions the model's prefetch keeps in flight: 64 fill a coalesced read's batch about eight
   *  times as full as the eight platform threads that each held a connection. */
  val IdentityPrefetchThreads = 64

  /** The fill's cadence: the settle's former 30 minutes, which its per-round allowance was measured
   *  against. */
  val DefaultIdentityShadowInterval: settings.IdentityShadowInterval =
    settings.IdentityShadowInterval(scala.concurrent.duration.Duration(30, "minutes"))
  /** Clear of the boot's scrape burst and the model's take-up. */
  val DefaultIdentityShadowInitialDelay: settings.IdentityShadowInitialDelay =
    settings.IdentityShadowInitialDelay(scala.concurrent.duration.Duration(5, "minutes"))

  /** The closure sweep's cadence: its evidence moves in days (a gone venue is re-probed
   *  daily), so a daily verdict loses nothing. */
  val ClosureSweepInterval: scala.concurrent.duration.FiniteDuration = scala.concurrent.duration.Duration(24, "hours")
  /** How often the model is asked about new unresolved titles, and how long after boot first. */
  val ProposalInterval: scala.concurrent.duration.FiniteDuration     = scala.concurrent.duration.Duration(60, "minutes")
  val ProposalInitialDelay: scala.concurrent.duration.FiniteDuration = scala.concurrent.duration.Duration(20, "minutes")
  /** Clear of the boot scrape burst, which it has no reason to compete with. */
  val ClosureSweepInitialDelay: scala.concurrent.duration.FiniteDuration = scala.concurrent.duration.Duration(30, "minutes")

  /** The ONE background budget a process shares across its countries' wirings. */
  def backgroundBudgetFrom(configuration: ProcessConfiguration): ExecutionBudget =
    SharedExecutionBudget.forBackground(configuration.backgroundConcurrency(DefaultBackgroundConcurrency))
}
