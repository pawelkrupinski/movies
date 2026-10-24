package modules.wiring

import modules.WorkerWiring
import services.identity.{FilmIdCounterStore, IdentityListingIntake, InMemoryFilmIdCounterStore, InMemoryVenueSlotFingerprints, MongoFilmIdCounterStore, MongoVenueSlotFingerprints, VenueSlotFingerprints}
import services.identity.adapters.IdentityProjection
import services.identity.toAnswer
import services.movies.{CinemaSlotBuilder, ScrapeHealth}
import services.scrapes.{MongoScrapeArchiveRepository, ScrapeArchiveRepository}


/**
 * How a country's films are made (docs/design/identity-resolver.md §8; the cut-over itself in §18):
 *  - a scrape goes to [[IdentityListingIntake]] (the venue's accepted listing);
 *  - [[IdentityProjection]] writes the identity model's films: once the model is taken up, then as its batches move
 *    (`onModelBatch`), and hourly over the whole corpus (the settle reaper's reconcile).
 */
trait IdentityProjectionWiring { self: WorkerWiring =>

  /** Each venue's accepted listing (`identity_listings`). */
  lazy val acceptedListings: ScrapeArchiveRepository =
    new MongoScrapeArchiveRepository(mongoConnection.database, IdentityListingIntake.Collection,
      daysUnread = workerMetrics.identityAgreement.daysUnread(country.code, "accepted"))

  /** The persisted FilmId map (`identity_film_ids`), in memory without a database. */
  lazy val filmIdCounterStore: FilmIdCounterStore =
    mongoConnection.database.fold[FilmIdCounterStore](new InMemoryFilmIdCounterStore)(new MongoFilmIdCounterStore(_))

  // What each venue is taken to publish after every scrape reaches the identity model as its listings now.
  lazy val identityListingIntake: IdentityListingIntake =
    new IdentityListingIntake(acceptedListings, scrapeArchive, scrapeGuardLedger, titleNormalizer,
      ScrapeHealth.maxRejectionsFor(scrapeFreshness), clock, taskMetrics,
      published = (cinema, films) => identityModel.venueScraped(cinema, films))

  // The projection reads the model's resolution.
  lazy val identityProjection: IdentityProjection =
    new IdentityProjection(
      listings    = () => identityListingIntake.projectedByVenue(liveCinemas),
      changedListings = () => identityListingIntake.projectedChanged(liveCinemas),
      rows        = identityListingIntake.rowsOf,
      resolve     = IdentityProjection.modelled(identityModel, IdentityProjectionWiring.ModelTimeout),
      cache       = movieCache,
      filmIds     = filmIdCounterStore,
      details     = movieService.withFilmDetails,
      announce    = movieService.announceReidentified,
      normalizer  = titleNormalizer,
      slots       = new CinemaSlotBuilder(country.language, workerMetrics.stringPool,
        new services.identity.adapters.IndexedVenuePageFacts(detailEnrichers, venuePageIndex, country.language)),
      tokens      = screeningTokens,
      metrics     = workerMetrics.identityProjection.forCountry(country.code),
      clock       = clock,
      fingerprints = venueSlotFingerprints,
      takeLines   = new services.identity.TakeChangeLines(country, configuration.commit,
        listing => titleNormalizer.firedRules(listing.cinema, listing.rawTitle)),
      adopt       = identityListingIntake.adopt,
      agreement   = (resolution, listingOf, republished) => agreementStage.apply(resolution, listingOf, familyAnswerStore.version, republished),
      agreementBacklog = () => agreementStage.backlog,
      suspects    = (resolution, listings) => { suspectTakeCount.submit(resolution, listings); ruleFiringCount.submit(resolution, listings) })

  /** The suspect takes of each written projection of the whole corpus (`kinowo_worker_identity_suspect_takes`): its
   *  listings as their venue pages state them, as the agreement reads them, and the agreed films' records from the model's
   *  store, in one batch — counted on a thread of its own, put off while the heap is near its limit. */
  lazy val suspectTakeCount: services.identity.adapters.SuspectTakeCount =
    new services.identity.adapters.SuspectTakeCount(venueDetailLookups(),
      services.identity.adapters.SuspectTakeCount.recordsOf(storedLookups()), clock, country.zone, workerMetrics.identityProjection.suspects(country.code),
      runner = managedResources.executor("identity suspect takes")(tools.DaemonExecutors.boundedPool(s"identity-suspect-takes-${country.code}",
        threads = 1, queueCapacity = 1, tools.WhenFull.WaitForRoom)),
      pressed = () => tools.HeapPressure.pressed())

  /** How often each rule fires on the live listings (`kinowo_worker_identity_rule_fires_total{rule}`), over each written
   *  projection of the whole corpus — counted on a thread of its own, put off while the heap is near its limit. */
  lazy val ruleFiringCount: services.identity.adapters.RuleFiringCount =
    new services.identity.adapters.RuleFiringCount(titleNormalizer, workerMetrics.identityProjection.ruleFirings(country.code),
      runner = managedResources.executor("identity rule firings")(tools.DaemonExecutors.boundedPool(s"identity-rule-firings-${country.code}",
        threads = 1, queueCapacity = 1, tools.WhenFull.WaitForRoom)),
      pressed = () => tools.HeapPressure.pressed())

  // ── Agreement: the no-matches ≥3 other film database families agree on ─────────────────────
  /** What the other film database families answered (`identity_family_answers`), kept long. */
  lazy val familyAnswerStore: services.identity.adapters.FamilyAnswerStore = identityTmdbLayer.familyAnswers
  /** The families asked in this country: Filmweb only where it indexes the country's titles (measured 2026-10-04: it helps
   *  in PL, DE and ES, adds nothing in the UK, and its dissent blocks a right US agreement). */
  def agreementFamilies: Seq[services.identity.agreement.VoterFamily] = {
    import services.identity.agreement.VoterFamily
    Seq(VoterFamily.Imdb, VoterFamily.Wiki, VoterFamily.Metacritic, VoterFamily.RottenTomatoes) ++
      Option.when(IdentityProjectionWiring.FilmwebVoting(country.code))(VoterFamily.Filmweb)
  }
  lazy val agreementStage: services.identity.agreement.AgreementStage =
    newAgreementStage(agreementVerdicts, clock,
      ask = questions => services.identity.adapters.AgreementQuestions.enqueue(taskQueue, questions, clock, agreementQuestionMetrics),
      metrics = workerMetrics.identityAgreement.stage(country.code),
      askNow = new services.identity.adapters.InlineFamilyAsks(agreementAsker,
        managedResources.executor("identity inline family asks")(tools.DaemonExecutors.boundedPool(s"identity-inline-asks-${country.code}",
          threads = InlineFamilyAskThreads, queueCapacity = 256, tools.WhenFull.WaitForRoom, virtual = true)),
        stopwatch = stopwatch))
  /** How many of a new cluster's family questions are asked inline at once: one per family and kind, roughly. */
  private val InlineFamilyAskThreads = 16
  /** An agreement stage over this country's answers: the live one ([[agreementStage]]), and the drift check's, which keeps
   *  nothing, asks nothing and reads every take at once. */
  def newAgreementStage(stored: services.identity.agreement.AgreementVerdicts, stageClock: java.time.Clock,
                        ask: Set[services.identity.Question] => Unit = _ => (),
                        metrics: services.identity.agreement.AgreementStage.Metrics = services.identity.agreement.AgreementStage.Metrics.Silent,
                        budgets: Boolean = true,
                        askNow: services.identity.agreement.AgreementStage.AskNow = services.identity.agreement.AgreementStage.AskNow.Never)
      : services.identity.agreement.AgreementStage =
    new services.identity.agreement.AgreementStage(agreementFamilies.map(family => family -> familyAnswerStore.answers(family)).toMap,
      venueDetailLookups(),
      titleNormalizer, services.identity.IdentityCalibration.resolver,
      tmdbOf = imdbId => identityTmdbStore.foundFirst(imdbId).toAnswer,
      stored = stored, ask = ask, metrics = metrics, clock = stageClock, changes = familyAnswerStore,
      posters = posterAnswerStore,
      // the fill's resolve of a cluster alone asks what the model's whole-corpus resolve never did: read as the model
      // reads (the store, else live), never from the store alone, where those questions were gaps nothing asked
      tmdb = Some(cutoverLookups()), identities = IdentityProjectionWiring.identities(country.code),
      catalogue = catalogueAnswerStore,
      listedOn = filmwebProgrammes.flatMap(_ => IdentityProjectionWiring.listedOn(country.code)),
      correctionPostersAtOnce = if (budgets) services.identity.agreement.AgreementStage.CorrectionPostersAtOnce else Int.MaxValue,
      correctionsPerApply = if (budgets) services.identity.agreement.AgreementStage.CorrectionsPerApply else Int.MaxValue,
      askNow = askNow)

  /** The daily drift check: the live takes against a whole resolve of the same listings with nothing kept
   *  ([[services.identity.adapters.IdentityDriftCheck]]), its lines in the worker's log, its count a gauge. Report only
   *  unless `KINOWO_IDENTITY_DRIFT_REPAIR` asks it to adopt the whole resolve's takes. */
  lazy val identityDriftCheck: services.identity.adapters.IdentityDriftCheck = {
    import services.identity.adapters.IdentityDriftCheck
    new IdentityDriftCheck(
      live = () => identityProjection.lastProjected.flatMap { projected =>
        identityModel.current(IdentityProjectionWiring.ModelTimeout).map { snapshot =>
          val now = snapshot.resolution.decisionOf
          IdentityDriftCheck.Live(projected.taken, snapshot.listings.iterator.map(l => l.key -> l).toMap,
            key => now.get(key).exists(d => projected.model.decisionOf.get(key).exists(_ eq d)))
        }
      },
      whole = live => IdentityDriftCheck.wholeResolve(live, cutoverLookups(), titleNormalizer,
        services.identity.PinConstraints(new services.identity.MongoPinStore(mongoConnection.database).all()),
        (stageClock: java.time.Clock) => newAgreementStage(new services.identity.agreement.InMemoryAgreementVerdicts, stageClock, budgets = false),
        familyAnswerStore.version, clock),
      lines = new services.identity.TakeChangeLines(country, configuration.commit),
      metrics = workerMetrics.identityDrift.forCountry(country.code, clock),
      response = if (configuration.identityDriftRepair.value) new IdentityDriftCheck.Adopt(identityModel, agreementStage, identityProjectionTrigger)
                 else IdentityDriftCheck.Response.ReportOnly,
      pressed = () => tools.HeapPressure.pressed(), stopwatch = stopwatch)
  }
  /** Runs [[identityDriftCheck]] once a day, off-peak: an hourly tick runs it within the country's night
   *  ([[IdentityProjectionWiring.offPeak]]) once per local date, claimed so a restart that night does not run it again —
   *  a check timed from boot would never come round on a worker deployed several times a day. */
  lazy val identityDriftSchedule: services.tasks.ClaimedPeriodicTask = managedResources.stopping(
    new services.tasks.ClaimedPeriodicTask("identity-drift-check-tick", () => {
      val venueClock = new models.VenueClock(clock)
      if (IdentityProjectionWiring.offPeak(venueClock, country.zone) &&
          scheduledRunStore.claim(s"identity-drift-check:${venueClock.today(country.zone)}")) identityDriftCheck.run()
      ()
    }, IdentityProjectionWiring.DriftTick, IdentityProjectionWiring.DriftTick, scheduledRunStore, clock, ticks))
  /** The posters' hashes the agreement's poster evidence reads, filed among the families' answers. */
  lazy val posterAnswerStore: services.identity.adapters.PosterAnswerStore = new services.identity.adapters.PosterAnswerStore(familyAnswerStore, clock)
  /** The venue film pages whose catalogue links the catalogue take reads, each through the fetch its client scrapes with. */
  lazy val catalogueLinkReader: services.identity.adapters.CatalogueLinkReader = new services.identity.adapters.CatalogueLinkReader(Seq(
    services.cinemas.common.FlicksClient.CatalogueLinkPages -> flicksFetch,
    services.cinemas.pl.KinotekaClient.CatalogueLinkPages   -> httpFetch,
    services.cinemas.us.AlamoDrafthouseClient.CatalogueLinkPages -> cinemaScraperCatalog.alamoDetailHttp))
  /** The catalogue ids' mappings and the venue pages' catalogue links, filed among the families' answers. */
  lazy val catalogueAnswerStore: services.identity.adapters.CatalogueAnswerStore =
    new services.identity.adapters.CatalogueAnswerStore(familyAnswerStore, clock, catalogueLinkReader.pages)
  /** Maps a catalogue id to its film: Wikidata's statement of it, a Letterboxd slug by its own page else. */
  lazy val catalogueMapping: services.identity.adapters.CatalogueMapping = new services.identity.adapters.WikidataCatalogueMapping(wikidataClient, letterboxdClient)
  /** Hashes a venue's or a TMDB film's poster: the image through the enrichment fetch chain (its pacing and breakers; a
   *  host blocking the worker through its scrapes' egress), cut to the card's slot under the process's decode gate. */
  lazy val posterHashing: services.identity.adapters.PosterHashing = new services.identity.adapters.PosterHashing(
    services.sharecards.PosterDownload.routed(new services.sharecards.EgressPosterDownload(enrichmentFetch), posterEgressRoutes),
    posterShrinker, tmdbId => tmdbClient.posters(tmdbId, also = Seq("en")), country.language.getLanguage)
  /** How the agreement's questions are enqueued and asked, per family and outcome. */
  lazy val agreementQuestionMetrics: services.identity.adapters.AgreementQuestionMetrics = workerMetrics.identityAgreement.questions(country.code)
  /** The agreement's verdicts (`identity_agreements`), kept as the model's families are. */
  lazy val agreementVerdicts: services.identity.agreement.AgreementVerdicts =
    mongoConnection.database.fold[services.identity.agreement.AgreementVerdicts](new services.identity.agreement.InMemoryAgreementVerdicts)(
      new services.identity.agreement.MongoAgreementVerdicts(_))
  /** The families' live sources the agreement fill asks. */
  lazy val familySources: Map[services.identity.agreement.VoterFamily, services.identity.adapters.FamilySource] = {
    val all: Seq[services.identity.adapters.FamilySource] = Seq(new services.identity.adapters.ImdbFamily(imdbClient),
      new services.identity.adapters.WikiFamily(wikidataClient, country.language.getLanguage),
      new services.identity.adapters.FilmwebFamily(filmwebClient, venue => filmwebProgrammes.fold(Seq.empty[services.identity.agreement.Showing])(_.of(venue))),
      new services.identity.adapters.RottenTomatoesFamily(rottenTomatoesClient), new services.identity.adapters.MetacriticFamily(metacriticClient))
    all.filter(source => agreementFamilies.contains(source.family)).map(source => source.family -> source).toMap
  }
  /** Each venue's programme on Filmweb — its own, else its town's — in Poland alone, where Filmweb lists programmes: what
   *  the agreement joins the venues' listings to ([[services.identity.agreement.VenueListings]]). */
  lazy val filmwebProgrammes: Option[services.cinemas.pl.FilmwebProgrammes] =
    Option.when(IdentityProjectionWiring.listedOn(country.code).isDefined)(new services.cinemas.pl.FilmwebProgrammes(enrichmentFetch,
      venue => filmwebFallbackIds.collectFirst { case (cinema, id) if cinema.displayName == venue => id },
      services.cinemas.pl.FilmwebProgrammes.townsOf,
      () => new models.VenueClock(clock).todayInPoland))
  /** Asks the agreement's questions, each filed answer asking for a projection, which reads it: on the queue, and a new
   *  cluster's family questions inline ([[agreementStage]]'s `askNow`). */
  lazy val agreementAsker: services.identity.adapters.AgreementAsker =
    new services.identity.adapters.AgreementAsker(familyAnswerStore, familySources,
      find = imdbId => { tmdbClient.findByImdbId(imdbId); () },
      reread = film => { tmdbClient.readRecordAgain(film); familyAnswerStore.noteFiled(services.identity.agreement.AgreementStage.recordReadId(film)) },
      posterAnswerStore, posterHashing, catalogueAnswerStore, catalogueMapping, catalogueLinkReader,
      filed = () => {
        identityProjection.asked(IdentityProjection.Trigger.Answer)
        identityProjectionTrigger.request(services.identity.EventTrigger.Answer)
      }, clock, agreementQuestionMetrics)
  /** The agreement's questions as queue tasks, and the tasks of the types before them still queued. */
  lazy val agreementHandlers: Seq[services.tasks.TaskHandler] = agreementAsker +: agreementAsker.oldQueued
  /** Projects what moved: as the identity model takes this worker's scrapes in, and as another writer moves a stored film
   *  (`MovieCache.onChanged`) — there is no period between. */
  lazy val identityProjectionTrigger: services.identity.EventTrigger = {
    val trigger = new services.identity.EventTrigger(() => identityProjection.tickChangedQuietly(),
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
}

object IdentityProjectionWiring {
  /** How long a projection waits for the model to catch up — a rebuild after a deploy included —
   *  before it refuses and the stored films keep serving. */
  val ModelTimeout: scala.concurrent.duration.FiniteDuration = scala.concurrent.duration.Duration(10, "minutes")
  /** How often the drift check's tick asks whether tonight's check is due. */
  val DriftTick: scala.concurrent.duration.FiniteDuration = scala.concurrent.duration.Duration(1, "hour")
  /** The country's night, when the drift check runs: past the evening's scrapes, before the morning's (01:00–06:00). */
  def offPeak(clock: models.VenueClock, zone: java.time.ZoneId): Boolean = {
    val hour = clock.now(zone).getHour
    hour >= 1 && hour < 6
  }
  /** The countries whose titles Filmweb indexes well enough to vote (signal-combination experiment §9). */
  val FilmwebVoting: Set[String] = Set("pl", "de", "es")
  /** The families besides IMDb whose own id a film TMDB and IMDb hold no record of may stand on, in preference: Wikidata's
   *  everywhere, Filmweb's in Poland alone — a Polish database, its records of other countries' releases no identity.
   *  Replayed on the unmatched clusters (2026-10-05): DE "Jeder Acker, jede Fabrik" and "Chasing Hope – Der andere Blick"
   *  (Wikidata), PL "Płazy. Pionierzy życia na lądzie" (Filmweb), none wrong. */
  def identities(country: String): Seq[services.identity.agreement.VoterFamily] =
    Seq(services.identity.agreement.VoterFamily.Wiki) ++ Option.when(country == "pl")(services.identity.agreement.VoterFamily.Filmweb)
  /** The family whose site lists each venue's programme, that the agreement joins the venues' listings to: Filmweb, in
   *  Poland alone (it lists no other country's cinemas). */
  def listedOn(country: String): Option[services.identity.agreement.VoterFamily] =
    Option.when(country == "pl")(services.identity.agreement.VoterFamily.Filmweb)
}
