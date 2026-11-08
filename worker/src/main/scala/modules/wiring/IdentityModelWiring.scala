package modules.wiring

import modules.WorkerWiring

/** ── The incremental identity model ─────────────────────────────────────────
 * The resolver kept current by events (docs/design/identity-resolver.md §20) instead of a
 * whole-corpus resolve per tick: each venue's accepted listing as the intake publishes it, each TMDB
 * answer the store files and each venue page the enrichment reads, drained on one thread; its
 * families persist in `identity_model_families`, so a restart takes them up and re-resolves only what
 * moved while it was down. The projection (`IdentityProjectionWiring`) writes its films.
 */
trait IdentityModelWiring { self: WorkerWiring =>

  lazy val identityReads: services.identity.ObservationReads = new services.identity.ObservationReads

  // The identity model's TMDB and IMDb answers, normalized as they are fetched (`TmdbStore`): a film,
  // a person and a question each once, as native BSON holding only what the resolver reads. Filled
  // by the pipeline's client (`identityLookupFetch`) and the fill; read by the model. Where the model
  // runs, in the country's database — documents, store and family answers as one layer.
  protected lazy val identityTmdbLayer: services.identity.adapters.IdentityTmdbLayer =
    new services.identity.adapters.IdentityTmdbLayer(mongoConnection.database, clock, identityHistory, configuration.commit.value)
  /** The identity system's history (`identity_history`, a month of it): every answer, venue page and listing the model
   *  read as it moved, every read that failed, and every take the projection moved with what decided it — what
   *  `scripts/identity-replay` resolves a past moment from and `scripts/identity-why` explains a take change by. */
  // Written on a thread of its own (AsyncIdentityHistory): no caller waits on it, a full queue drops and counts --
  // except the take entries, written with the projection (a killed worker loses no take).
  lazy val identityHistory: services.identity.adapters.IdentityHistory =
    services.identity.adapters.IdentityHistory.composed(
      mongoConnection.database.fold[services.identity.adapters.IdentityHistory](services.identity.adapters.IdentityHistory.Discard)(db =>
        managedResources.stopping(new services.identity.adapters.AsyncIdentityHistory(
          new services.identity.adapters.MongoIdentityHistory(db, workerMetrics.ttlIndexMismatches),
          workerMetrics.identityModel.historyDropped(country.code)))),
      identityHistoryOverrides)
  /** The model's input gate (docs/design/identity-structural-fixes.md, fix 1): a listing's version is taken in once its
   *  never-attempted venue page (`pages`) and the questions its own facts put to the families were each attempted, asked
   *  ahead of the agreement's queue, and at most `ModelInputGate.MaxHold` -- while [[WorkerWiring.modelReadiness]] gates. */
  def identityModelReadiness(pages: services.identity.PageWait): services.identity.ListingReadiness =
    WorkerWiring.modelReadiness(services.identity.ListingReadiness.ofPage(pages), identityListingReadiness)
  /** The listings the model's input gate holds, persisted (`identity_pending`): what a take-up resumes. */
  lazy val identityPendingStore: services.identity.adapters.PendingListingStore =
    mongoConnection.database.fold[services.identity.adapters.PendingListingStore](new services.identity.adapters.PendingListingStore.InMemory())(
      new services.identity.adapters.MongoPendingListingStore(_))
  /** Every listing that left, with its facts and departure, persisted (`identity_retained`): the input ListingRetention keeps from. */
  lazy val identityRetainedStore: services.identity.adapters.RetainedListingStore =
    mongoConnection.database.fold[services.identity.adapters.RetainedListingStore](new services.identity.adapters.RetainedListingStore.InMemory())(
      services.identity.adapters.MongoRetainedListingStore.indexed(_, clock))
  /** The listings the identity history last saw the model hold: what the kept set starts from before it was ever
   *  persisted (`RetainedListingStore.fromHistory`). */
  protected def identityHeldBefore(): Seq[services.identity.Listing] =
    mongoConnection.database.fold(Seq.empty[services.identity.Listing]) { db =>
      val reader = new services.identity.adapters.MongoIdentityHistory(db, workerMetrics.ttlIndexMismatches)
      services.identity.adapters.RetainedListingStore.fromHistory(
        reader.since(Seq(services.identity.adapters.HistoryEntry.Listing), clock.millis() - services.identity.adapters.IdentityHistory.Kept.toMillis))
    }
  /** The take history's last film change of each listing within the reversal count's horizon, as the count holds them
   *  (`TakeHistory.reversalsHeld`): none without a database, or when the read fails (logged). */
  protected def identityReversalsHeld(): Seq[(services.movies.ListingKey, services.identity.TakeReversals.Held)] =
    mongoConnection.database.fold(Seq.empty[(services.movies.ListingKey, services.identity.TakeReversals.Held)]) { db =>
      val reader = new services.identity.adapters.MongoIdentityHistory(db, workerMetrics.ttlIndexMismatches)
      scala.util.Try(services.identity.adapters.TakeHistory.reversalsHeld(reader, clock.millis()))
        .recover { case scala.util.control.NonFatal(e) => logger.warn(s"take reversals: the take history not read, counting from none: $e"); Nil }.get
    }
  /** How this wiring's history differs from the worker's (`IdentityHistory.Override`): none in production; a harness's
   *  wiring names its own. */
  protected def identityHistoryOverrides: Set[services.identity.adapters.IdentityHistory.Override] = Set.empty
  lazy val identityTmdbDocuments: services.identity.adapters.TmdbDocuments = identityTmdbLayer.documents
  /** The layer's document cache, by cache name, for `registerCacheMetrics`. */
  lazy val identityTmdbCacheOccupancies: Seq[(String, () => services.metrics.CacheOccupancy)] = identityTmdbLayer.cacheOccupancies
  /** The store's retention (`TmdbStoreSweep`): answers the model no longer reads, and stale gap markers. */
  lazy val identityTmdbSweep: services.identity.adapters.TmdbStoreSweep =
    new services.identity.adapters.TmdbStoreSweep(identityTmdbLayer.backend,
      liveKeys = () => identityModel.peek(WorkerWiring.IdentityModelPeek).map(_ => identityReads.trackedKeys), clock)
  lazy val identityTmdbSweepSchedule: services.tasks.ClaimedPeriodicTask = managedResources.stopping(
    new services.tasks.ClaimedPeriodicTask("tmdb-store-sweep", () => { identityTmdbSweep.sweep(); tools.TickRun.Completed },
      services.identity.adapters.TmdbStoreSweep.Interval, WorkerWiring.TmdbSweepInitialDelay, scheduledRunStore, clock, ticks))
  lazy val identityTmdbStore: services.identity.adapters.TmdbStore = identityTmdbLayer.store
  lazy val identityTmdbNormalizer: services.identity.adapters.TmdbNormalizer =
    new services.identity.adapters.TmdbNormalizer(identityTmdbStore, tmdbJsonBodies,
      new services.identity.adapters.KeptFailures(identityHistory, clock, configuration.commit.value))
  /** Keeps the store current from TMDB's change lists (`TmdbChangesSweep`): each day's edits checked at a
   *  pace spread over the day, from the country's own hashed phase, through the lookup chain (`enrichmentFetch`). */
  lazy val identityTmdbChanges: services.identity.adapters.TmdbChangesSweep =
    new services.identity.adapters.TmdbChangesSweep(identityTmdbStore, identityTmdbDocuments,
      new services.identity.adapters.NormalizingHttpFetch(enrichmentFetch, identityTmdbNormalizer), tmdbClientOver,
      country.language.toLanguageTag, services.identity.adapters.TmdbChangesSweep.Schedule.forCountry(country.code), clock)
  /** The sweep's minute ticks, each spending a minute's allowance. */
  lazy val identityTmdbChangesSchedule: services.tasks.ClaimedPeriodicTask = managedResources.stopping(
    new services.tasks.ClaimedPeriodicTask("tmdb-changes-sweep", () => identityTmdbChanges.tick().run,
      services.identity.adapters.TmdbChangesSweep.TickInterval,
      configuration.identityLookupFillInitialDelay(WorkerWiring.DefaultIdentityLookupFillInitialDelay).value, scheduledRunStore, clock, ticks))
  /** The model's answers due to be asked again for their age (`QuestionRenewals`): its questions, then the records and
   *  people it reads, while it is taken up. */
  lazy val identityRenewals: services.identity.adapters.QuestionRenewals =
    new services.identity.adapters.QuestionRenewals(identityTmdbStore, identityTmdbLayer.backend, country.language.toLanguageTag,
      questions = () => identityModel.peek(WorkerWiring.IdentityModelPeek).fold(Seq.empty[(Set[services.identity.CandidateQuery], Boolean)])(_.questions),
      liveKeys = () => identityModel.peek(WorkerWiring.IdentityModelPeek).map(_ => identityReads.trackedKeys), clock)
  // The model is the country's identity: the intake's accepted listings, answered from the model's
  // store and asked live on a gap (`cutoverLookups`).
  lazy val identityModel: services.identity.adapters.IdentityModelService = {
    import services.identity._
    import services.identity.adapters.IdentityModelService
    val pinStore = new MongoPinStore(mongoConnection.database)
    var tracked  = Option.empty[TrackedLookups]
    // A new listing's unread venue page is asked for, read into venue_pages by a ReadVenuePage task; the model's input
    // gate takes the listing in once it is read or its read was attempted.
    val pageWait = new services.identity.adapters.VenuePageWait(detailEnrichers, venuePageIndex, taskQueue, freshnessStore, clock)
    val model = new IdentityModelService(
      newModel   = () => {
        // the one builder of a resolve's inputs, the drift checks' and the identity slice's too
        val inputs = ResolveInputs.of(pinStore.all())
        val lookupsNow = new TrackedLookups(cutoverLookups(identityReads), identityReads, Some(identityPrefetchPool))
        tracked = Some(lookupsNow)
        inputs.incremental(lookupsNow, titleNormalizer,
          store = mongoConnection.database.fold[IdentityModelStore](new InMemoryIdentityModelStore)(new MongoIdentityModelStore(_)),
          code = IdentityRules.codeVersion, traces = identityTraces, takeUpThreads = WorkerWiring.IdentityTakeUpThreads)
      },
      reads      = identityReads,
      archive    = () => Listing.distinct(Listing.all(identityListingIntake.identities(liveCinemas), titleNormalizer)),
      normalizer = titleNormalizer,
      settle     = WorkerWiring.IdentityModelSettle,
      scheduler  = identityModelScheduler,
      metrics    = workerMetrics.identityModel.forCountry(country.code),
      batched    = onModelBatch,
      reading    = () => tracked.fold("")(_.render),
      beforeDrain = () => venuePageIndex.settle(),
      pageWait   = pageWait,
      takenUp    = () => identityModelTakenUp(),
      clock      = clock,
      history    = identityHistory,
      build      = configuration.commit.value,
      retainedStore = identityRetainedStore,
      heldBefore = () => identityHeldBefore(),
      readiness  = identityModelReadiness(pageWait),
      pendingStore = identityPendingStore,
      retentionWindow = configuration.identityRetentionWindow(WorkerWiring.DefaultRetentionWindow).duration)
    identityTmdbStore.onChanged(model.filed)
    model
  }
  /** The model's TMDB and IMDb answers from its normalized store, and venue details from venue_pages
   *  ([[venuePageIndex]]) — what the model reads first ([[cutoverLookups]]). */
  def storedLookups(reads: services.identity.ObservationReads = services.identity.ObservationReads.Untracked): services.identity.IdentityLookups =
    new services.identity.adapters.StoredTmdbLookups(identityTmdbStore, country.language.toLanguageTag, venueDetailLookups(reads), reads)
  /** What the venues' own detail pages state of their listings ([[venuePageIndex]]). */
  def venueDetailLookups(reads: services.identity.ObservationReads = services.identity.ObservationReads.Untracked): services.identity.adapters.VenueDetailLookups =
    new services.identity.adapters.VenueDetailLookups(detailEnrichers, venuePageIndex, reads)

  /** What a drain of the identity model sets off: the projection of what moved and the fill of the gaps it found.
   *  Neither runs on a period. */
  protected[modules] def onModelBatch(batch: services.identity.adapters.ModelBatch): Unit = if (batch.moved) {
    identityProjection.asked(services.identity.adapters.IdentityProjection.Trigger.Scrape)
    identityProjectionTrigger.request(); identityFillTrigger.request()
  }

  /** A cut-over model's lookups: [[storedLookups]] first, and a TMDB or IMDb question the store has no
   *  answer to asked live through `identityLookupFetch`, which files the answer into the store. */
  def cutoverLookups(reads: services.identity.ObservationReads = services.identity.ObservationReads.Untracked): services.identity.IdentityLookups = {
    val counted = new services.identity.adapters.LiveAsks.Counted(identityLookupFetch)
    new services.identity.adapters.StoredFirstLookups(storedLookups(reads),
      new services.identity.adapters.TmdbIdentityLookups(tmdbClientOver(counted), new services.enrichment.ImdbClient(counted), Nil),
      services.identity.adapters.LiveAsks.remembered(identityQuestionMemory))
  }

  /** What was asked and left unanswered, and when it is due again: one memory for the fill, the model's live asks and
   *  the agreement's questions ([[services.identity.adapters.QuestionMemory]]). */
  lazy val identityQuestionMemory: services.identity.adapters.QuestionMemory =
    new services.identity.adapters.QuestionMemory(identityTmdbDocuments, country.language.toLanguageTag, clock)

  /** `venue_pages`: every venue detail page read, written once by page (`VenuePageReader`). */
  lazy val venuePageStore: services.venuepages.VenuePageStore =
    new services.venuepages.HistoryVenuePageStore(newVenuePageStore, identityHistory, clock, configuration.commit.value)
  /** The store [[venuePageStore]] holds, built once. */
  protected def newVenuePageStore: services.venuepages.VenuePageStore =
    mongoConnection.database.fold[services.venuepages.VenuePageStore](new services.venuepages.InMemoryVenuePageStore)(
      new services.venuepages.MongoVenuePageStore(_))

  /** The venue detail pages read into venue_pages, as the identity model reads them. */
  lazy val venuePageIndex: services.identity.adapters.VenuePageIndex = {
    import services.identity.adapters.VenuePageIndex
    identityTmdbLayer.feed.subscribe(key => if (VenuePageIndex.isKey(key)) identityModel.observed(key))
    new VenuePageIndex(venuePageStore, changed = identityTmdbLayer.feed.publish)
  }

  /** Where the model files which rules decided each listing (`identity_traces`, read by the admin
   *  page only): beside its families in the country's database, or nowhere without one. ONE for every model a
   *  rebuild makes: a store apiece left each replaced model's writer thread idle for the life of the process,
   *  and its queued writes racing the new model's. */
  protected lazy val identityTraces: services.identity.IdentityTraceStore =
    managedResources.register("identity traces", newIdentityTraces)(_.close())
  /** The store [[identityTraces]] holds, built once. */
  protected def newIdentityTraces: services.identity.IdentityTraceStore =
    identityTracesDatabase.fold[services.identity.IdentityTraceStore](services.identity.IdentityTraceStore.Discard)(
      new services.identity.MongoIdentityTraceStore(_))
  /** The database [[identityTraces]] writes into: the wiring's own. */
  protected def identityTracesDatabase: Option[org.mongodb.scala.MongoDatabase] = mongoConnection.database

  /** The threads the model's lookups prefetch on: each question waits on store round-trips, so a
   *  take-up is bound by how many are in flight, not by CPU. Virtual, and many: the store coalesces
   *  the reads and writes they file answers with into one round-trip per batch
   *  (`CoalescedTmdbDocuments`), so they share connections of the pool instead of each holding one. */
  protected lazy val identityPrefetchPool: java.util.concurrent.ExecutorService =
    managedResources.executor("identity prefetch")(tools.DaemonExecutors.boundedPool(s"identity-prefetch-${country.code}",
      WorkerWiring.IdentityPrefetchThreads, WorkerWiring.IdentityPrefetchQueue, tools.WhenFull.RunOnCaller, virtual = true))

  /** One daemon thread for the model: it is not thread-safe, and every event is drained on it. */
  protected lazy val identityModelScheduler: java.util.concurrent.ScheduledExecutorService =
    managedResources.executor("identity model")(tools.DaemonExecutors.scheduler(s"identity-model-${country.code}"))

  // The model's PACED LIVE LOOKUP FILL (docs/design/identity-resolver.md §19), on its own claimed
  // schedule (`identityLookupRefreshSchedule`): the model asks live only what its store lacks, so an
  // answer the store holds is renewed here — a search that found nothing before TMDB had the film
  // (`QuestionRenewals`) — and any question still open; a record TMDB changed since is `identityTmdbChanges`'.
  // Through the lookup chain (`enrichmentFetch`: its 429 gate, breaker and pace), at most
  // `KINOWO_IDENTITY_SHADOW_LOOKUP_RATE` per minute, the answers filed in the model's TMDB store.
  lazy val lookupFill: services.identity.adapters.LookupFill =
    new services.identity.adapters.LookupFill(
      questions   = () => identityModel.peek(WorkerWiring.IdentityModelPeek).fold(services.identity.AnswersChanged.Empty)(_.gaps),
      tmdb        = tmdbClientOver,
      liveFetch   = enrichmentFetch,
      normalizer  = identityTmdbNormalizer,
      memory      = identityQuestionMemory,
      renewals    = () => identityRenewals.due(),
      rate        = configuration.identityLookupFillRate(WorkerWiring.DefaultLookupFillRate),
      window      = identityLookupFillInterval,
      metrics     = workerMetrics.identityLookupFill.lookupsForCountry(country.code),
      executor    = lookupFillExecutor,
      sleep       = lookupFillSleep,
      clock       = clock,
      ticks       = ticks)

  /** Fills the model's gaps as a drain finds them, not only on the period below (which stays for what ages: refreshes
   *  due, gaps remembered as unanswered). A round asks what the rate allows since the last. */
  lazy val identityFillTrigger: services.identity.EventTrigger =
    new services.identity.EventTrigger(() => { lookupFill.start(); true }, services.movies.MovieChangeStream.Debounce.Worker,
      identityProjectionTriggerScheduler, clock)

  def identityLookupFillInterval: settings.IdentityLookupFillInterval =
    configuration.identityLookupFillInterval(WorkerWiring.DefaultLookupFillInterval)
  /** The fill's rounds, every `KINOWO_IDENTITY_SHADOW_INTERVAL_SECONDS` (30 minutes). */
  lazy val identityLookupRefreshSchedule: services.tasks.ClaimedPeriodicTask = managedResources.stopping(
    lookupFill.schedule(identityLookupFillInterval.value,
      configuration.identityLookupFillInitialDelay(WorkerWiring.DefaultIdentityLookupFillInitialDelay).value, scheduledRunStore))

  /** How the fill waits its pace between asks: real time, except in a replay harness. */
  protected def lookupFillSleep: Long => Unit = Thread.sleep

  /** One daemon thread for the fill's rounds, which sleep their pace between asks. */
  protected lazy val lookupFillExecutor: java.util.concurrent.ExecutorService =
    managedResources.executor("identity shadow lookups")(tools.DaemonExecutors.boundedEC(s"identity-shadow-lookups-${country.code}", 1))
}
