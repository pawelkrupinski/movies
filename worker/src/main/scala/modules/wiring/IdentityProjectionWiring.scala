package modules.wiring

import services.identity.{FilmIdCounterStore, IdentityListingIntake, InMemoryFilmIdCounterStore, InMemoryVenueSlotFingerprints, MongoFilmIdCounterStore, MongoVenueSlotFingerprints, VenueSlotFingerprints}
import services.identityadapters.IdentityProjection
import services.movies.{CinemaSlotBuilder, ScrapeHealth}
import services.scrapes.{MongoScrapeArchiveRepository, ScrapeArchiveRepository}
import services.tasks.SettleReaper
import settings.SettleInterval

/**
 * How a country's films are made (docs/design/identity-resolver.md §8; the cut-over itself in §18):
 *  - a scrape goes to [[IdentityListingIntake]] (the venue's accepted listing);
 *  - [[IdentityProjection]] writes the identity model's films: once the model is taken up, then as its batches move
 *    (`onModelBatch`), and hourly over the whole corpus (the settle reaper's reconcile).
 *  - the staging screen is told what its takes still wait on, from its change lists and the agreement's waits.
 * The agreement the projection's takes pass through is [[AgreementWiring]]'s.
 */
trait IdentityProjectionWiring { self: WorkerWiring =>

  /** Each venue's accepted listing (`identity_listings`). */
  lazy val acceptedListings: ScrapeArchiveRepository =
    new MongoScrapeArchiveRepository(mongoConnection.database, IdentityListingIntake.Collection,
      daysUnread = workerMetrics.identityAgreement.daysUnread(country.code, "accepted"))

  /** How long a projection waits on the model ([[IdentityProjectionWiring.ModelTimeout]]); a harness that must see the
   *  take-up through, however starved its runner, waits longer. */
  def identityModelTimeout: scala.concurrent.duration.FiniteDuration = IdentityProjectionWiring.ModelTimeout

  /** The persisted FilmId map (`identity_film_ids`), in memory without a database. */
  lazy val filmIdCounterStore: FilmIdCounterStore =
    mongoConnection.database.fold[FilmIdCounterStore](new InMemoryFilmIdCounterStore)(new MongoFilmIdCounterStore(_))

  // What each venue is taken to publish after every scrape reaches the identity model as its listings now.
  lazy val identityListingIntake: IdentityListingIntake =
    new IdentityListingIntake(acceptedListings, scrapeArchive, scrapeGuardLedger, titleNormalizer,
      ScrapeHealth.maxRejectionsFor(scrapeFreshness), clock, corpusMetrics,
      published = (cinema, films) =>
        identityModel.venueScraped(cinema.displayName, services.identity.ModelRead.corpus(Seq(cinema -> films), titleNormalizer)))

  // The venue pages as a venue slot is built from them, and as which of them moved since the projection last asked.
  private lazy val venuePageFacts =
    new services.identityadapters.IndexedVenuePageFacts(detailEnrichers, venuePageIndex, country.language, identityTmdbLayer.feed)

  // The projection reads the model's resolution.
  /** The answers a listing's own questions are filed under, as the take-change lines and the history name them. */
  private lazy val takeReads: services.identity.measures.Listing => Seq[String] =
    services.tmdb.TakeReads.of(country.language.toLanguageTag)
  lazy val identityProjection: IdentityProjection = {
    // every piece logs under the projection's own logger, as its lines always have
    val logger  = play.api.Logger(classOf[IdentityProjection])
    val metrics = workerMetrics.identityProjection.forCountry(country.code)
    given services.evidence.LatestTitleYear = services.evidence.LatestTitleYear(clock)
    // how a film's venue slots are built from the venues' rows: what the draft builds and the write completes them by
    val slotSource = new services.identity.VenueSlots.FromArchive(titleNormalizer,
      new CinemaSlotBuilder(country.language, workerMetrics.stringPool, venuePageFacts), screeningTokens, identityListingIntake.rowsOf)
    new IdentityProjection(
      listings    = () => identityListingIntake.projectedByVenue(liveCinemas),
      changedListings = () => identityListingIntake.projectedChanged(liveCinemas),
      // the incremental model, brought up to now on its own thread: no resolve here
      resolve     = { val (model, timeout) = (identityModel, identityModelTimeout)
        _ => model.current(timeout).map(snapshot =>
          IdentityProjection.Resolved(snapshot.resolution, snapshot.listings.view.map(_.key), snapshot.listings)) },
      cache       = movieCache,
      filmIds     = filmIdCounterStore,
      scoping     = new services.identityadapters.ProjectionScoping(titleNormalizer, venuePageFacts.takeMoved),
      drafter     = new services.identityadapters.ProjectionDrafter(titleNormalizer, slotSource, movieService.withFilmDetails,
        new services.identityadapters.RecordedSlotFingerprints(venueSlotFingerprints, logger)),
      writer      = new services.identityadapters.ProjectionWrite(movieCache, filmIdCounterStore, movieService.announceReidentified,
        titleNormalizer, slotSource, () => agreementStage.backlog, logger),
      drift       = new services.identityadapters.ProjectionDrift(titleNormalizer, metrics, logger),
      takeReports = new services.identityadapters.ProjectionTakeReports(metrics,
        changeFeed = { val projections = new java.util.concurrent.atomic.AtomicLong
          changes => {
            takeChangesTold(changes)
            val (projection, at) = (projections.incrementAndGet(), clock.millis())
            identityStagingEvents.offer(changes.map(services.identity.staging.StagingEvent.Changed(_, projection, at)))
          } },
        takeLines   = new services.identity.TakeChangeLines(country, configuration.commit,
          listing => titleNormalizer.firedRules(listing.cinema, listing.rawTitle), takeReads),
        takeHistory = new services.tmdb.TakeHistory(identityHistory, takeReads, clock, configuration.commit.value),
        reversals   = new services.identity.TakeReversals(clock, seed = identityReversalsHeld()), logger = logger),
      metrics     = metrics,
      clock       = clock,
      adopt       = identityListingIntake.adopt,
      // the agreement's takes, each a hand judgement calls wrong withdrawn from its own listing (TakeJudgements.withdraw)
      agreement   = (resolution, listingOf, republished) =>
        services.identity.TakeJudgements.withdraw(agreementStage.apply(resolution, listingOf, familyAnswerStore.version, republished), judged),
      suspects    = (resolution, listings) => { suspectTakeCount.submit(resolution, listings); ruleFiringCount.submit(resolution, listings) })
  }

  /** This country's hand judgements of takes, as the worker was built with them ([[BundledJudgements]]). */
  lazy val judged: services.identity.TakeJudgements.Index = BundledJudgements.index(country.code)

  /** Handed each written projection's change list, beside the staging publisher's feed: a harness overrides it to hold
   *  the change lists to what the projection wrote. */
  def takeChangesTold: Seq[services.identity.TakeChange] => Unit = _ => ()

  /** The suspect takes no hand judgement judges, and the takes one calls wrong, of each written projection of the whole
   *  corpus (`kinowo_worker_identity_suspect_takes`, `…_wrong`): its
   *  listings as their venue pages state them, as the agreement reads them, and the agreed films' records from the model's
   *  store, in one batch — counted on a thread of its own, put off while the heap is near its limit. */
  lazy val suspectTakeCount: services.identityadapters.SuspectTakeCount =
    new services.identityadapters.SuspectTakeCount(venueDetailLookups(),
      services.identityadapters.PrefetchedRecords.of(storedLookups()), clock, country.zone, workerMetrics.identityProjection.suspects(country.code),
      runner = managedResources.executor("identity suspect takes")(tools.DaemonExecutors.boundedPool(s"identity-suspect-takes-${country.code}",
        threads = 1, queueCapacity = 1, tools.WhenFull.WaitForRoom)),
      pressed = () => tools.HeapPressure.pressed(), judgements = judged,
      reportWrong = workerMetrics.identityProjection.judgedWrong(country.code))

  /** How often each rule fires on the live listings (`kinowo_worker_identity_rule_fires_total{rule}`), over each written
   *  projection of the whole corpus — counted on a thread of its own, put off while the heap is near its limit. */
  lazy val ruleFiringCount: services.identityadapters.RuleFiringCount =
    new services.identityadapters.RuleFiringCount(titleNormalizer, workerMetrics.identityProjection.ruleFirings(country.code),
      runner = managedResources.executor("identity rule firings")(tools.DaemonExecutors.boundedPool(s"identity-rule-firings-${country.code}",
        threads = 1, queueCapacity = 1, tools.WhenFull.WaitForRoom)),
      pressed = () => tools.HeapPressure.pressed())

  /** Projects what moved: as the identity model takes this worker's scrapes in, and as another writer moves a stored film
   *  (`MovieCache.onChanged`) — there is no period between. */
  lazy val identityProjectionTrigger: services.identity.vocabulary.EventTrigger = {
    val trigger = new services.identity.vocabulary.EventTrigger(() => identityProjection.tickChangedQuietly(),
      services.movies.MovieChangeStream.Debounce.Worker,
      identityProjectionTriggerScheduler, clock)
    movieCache.onChanged { _ => identityProjection.asked(IdentityProjection.Trigger.FilmChange); trigger.request() }
    trigger
  }

  /** Where the trigger runs its projections. A harness that projects by hand (`TestWiring.projectIdentity`) hands it one
   *  that never runs them: a projection on a timer beside the harness's own made its films hang on timing. */
  protected lazy val identityProjectionTriggerScheduler: java.util.concurrent.ScheduledExecutorService =
    managedResources.executor("identity projection")(tools.DaemonExecutors.scheduler(s"identity-projection-${country.code}"))

  /** The venue slots the projection last kept (`identity_slot_fingerprints`), in memory without a database. */
  lazy val venueSlotFingerprints: VenueSlotFingerprints =
    mongoConnection.database.fold[VenueSlotFingerprints](new InMemoryVenueSlotFingerprints)(new MongoVenueSlotFingerprints(_))

  // ── The staging screen (`scripts/identity-staging`): what this country's takes still wait on ──────────────────
  /** What the staging publisher drains as it happens: each written projection's change list, and the live agreement
   *  stage's waits starting and ending. */
  lazy val identityStagingEvents: services.identity.staging.ChangeFeed[services.identity.staging.StagingEvent] =
    new services.identity.staging.ChangeFeed(services.identityadapters.IdentityStagingPublisher.FeedCapacity)

  /** The staging screen's store (`identity_staging`), in this country's database, which the local screen reads beside
   *  every other country's; in memory without one. */
  lazy val identityStaging: services.identity.staging.IdentityStaging =
    services.identity.staging.IdentityStaging.in(mongoConnection.database, clock)
  /** What this country's takes still wait on, published for the local staging screen (`scripts/identity-staging`). */
  lazy val identityStagingPublisher: services.identityadapters.IdentityStagingPublisher = managedResources.stopping(
    new services.identityadapters.IdentityStagingPublisher(country.code, identityStaging,
      taken = () => identityProjection.lastProjected.map(_.taken), changes = () => identityStagingEvents.drain(),
      snapshots = stagingSnapshots, clock = clock, ticks = ticks,
      takeMetrics = workerMetrics.identityTakes.forCountry(country.code), threadPools = threadPools))
  /** What each staging publish works out ([[identityStagingPublisher]]): the clusters staging, what resolved, the beat. */
  private lazy val stagingSnapshots: services.identityadapters.StagingSnapshots =
    new services.identityadapters.StagingSnapshots(waitingOn = agreementStage.waits.waitingOn,
      outcomes = identityQuestionMemory.outcomes,
      storedFilm = { val records = storedLookups(); id => records.film(id).answered.flatten },
      read = () => identityProjection.lastProjected.fold(Iterator.empty[services.identity.measures.Listing])(_.read()),
      clusterKey = agreementStage.keyOf)

  // ── When it projects: on the trigger, at boot once the model is taken up, and the hourly reconcile ─────────────
  // The worker projects as its identity model takes its scrapes in (`identityProjectionTrigger`), from the boot's first
  // projection on; the clock runs only the whole corpus's reconciliation, every `IdentityProjection.ReconcileEvery` from
  // an hour after boot, so each has a projection before it to measure its drift against. One that did not settle is
  // tried again by the trigger, not an hour later.
  def settleTick(): Unit = if (!identityProjection.reconcileQuietly()) identityProjectionTrigger.retry()
  def bootProjectionTick(): Unit = if (!identityProjection.projectQuietly()) identityProjectionTrigger.retry()
  // The boot's projection runs the moment the identity model is taken up — the first moment one can succeed; a take-up
  // retried after a failure runs it then — on the trigger's scheduler, never behind the reconcile's claim: an hourly
  // window a previous pod already claimed skipped it, and every projection on scrapes waits for it — a restarted worker
  // projected nothing until the next hour (2026-10-04, US/UK/DE 20:10–21:00). Five minutes after boot before that, a
  // delay no measure chose: PL's model is up in seconds. It is a projection, not a reconcile: with none before it there
  // is no drift to measure.
  def identityModelTakenUp(): Unit = identityProjectionTrigger.once(scala.concurrent.duration.Duration.Zero)(bootProjectionTick())
  lazy val settleReaper = {
    managedResources.stopping(new SettleReaper(() => settleTick(),
    interval = SettleInterval(services.identityadapters.IdentityProjection.ReconcileEvery),
    initialDelay = SettleReaper.InitialDelay(services.identityadapters.IdentityProjection.ReconcileEvery),
    runStore = scheduledRunStore, clock = clock, ticks = ticks, threadPools = threadPools))
  }
}

object IdentityProjectionWiring {
  /** How long a projection waits for the model to catch up — a rebuild after a deploy included —
   *  before it refuses and the stored films keep serving. */
  val ModelTimeout: scala.concurrent.duration.FiniteDuration = scala.concurrent.duration.Duration(10, "minutes")
}
