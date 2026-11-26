package modules.wiring

import services.identity.vocabulary.toAnswer
import services.identityadapters.IdentityProjection

/**
 * The agreement stage (docs/design/identity-questions.md): the no-matches ≥3 other film database families agree on, the
 * model's takes their evidence corrects, and every question it puts to the families, posters and catalogues -- asked on
 * the queue, inline for a new cluster, again once a host is back. [[IdentityProjectionWiring]]'s projection applies it.
 */
trait AgreementWiring { self: WorkerWiring =>

  /** What the other film database families answered (`identity_family_answers`), kept long. */
  lazy val familyAnswerStore: services.tmdb.FamilyAnswerStore = identityTmdbLayer.familyAnswers
  /** The families asked in this country: Filmweb only where it indexes the country's titles (measured 2026-10-04: it helps
   *  in PL, DE and ES, adds nothing in the UK, and its dissent blocks a right US agreement). */
  def agreementFamilies: Seq[services.identity.vocabulary.VoterFamily] = {
    import services.identity.vocabulary.VoterFamily
    Seq(VoterFamily.Imdb, VoterFamily.Wiki, VoterFamily.Metacritic, VoterFamily.RottenTomatoes) ++
      Option.when(AgreementWiring.FilmwebVoting(country.code))(VoterFamily.Filmweb)
  }
  /** The live agreement stage. The TMDB finds and records it waits on ([[services.identity.agreement.AgreementStage.wanted]])
   *  move its answers' version as the TMDB store files them, whoever asked: the agreement's asker or the model's fill. */
  lazy val agreementStage: services.identity.agreement.AgreementStage = {
    val stage = newAgreementStage(agreementVerdicts, clock,
      ask = questions => { enqueueAgreementQuestions(questions); () },
      metrics = {
        // the stage's series, and its previous takes standing beside the input gate's held listings: one bound, one alert
        val stage = workerMetrics.identityAgreement.stage(country.code); val standing = workerMetrics.identityModel.standing(country.code)
        applied => { stage.applied(applied); standing.applied(applied) }
      },
      askNow = new services.identityadapters.InlineFamilyAsks(agreementAsker,
        managedResources.executor("identity inline family asks")(tools.DaemonExecutors.boundedPool(s"identity-inline-asks-${country.code}",
          threads = InlineFamilyAskThreads, queueCapacity = 256, tools.WhenFull.WaitForRoom, virtual = true)),
        stopwatch = stopwatch, metrics = agreementQuestionMetrics),
      staging = Some(identityStagingEvents.offer), keys = agreementKeys)
    familyAnswerStore.awaitedBy(question => stage.wanted.contains(question))
    familyAnswerStore.readBy(stage.reads)
    stage
  }
  /** The agreement's questions queued as tasks, those inside the wait a failed ask left them held back. */
  private def enqueueAgreementQuestions(questions: Set[services.identity.vocabulary.Question]): services.tasks.Enqueued =
    services.identityadapters.AgreementQuestions.enqueue(taskQueue, questions, identityQuestionMemory, clock, agreementQuestionMetrics)
  /** The model's input gate (docs/design/identity-structural-fixes.md, fix 1): a listing's version is taken in once the
   *  questions its own facts put to the families were each attempted once (a poster: hashed, known gone or given up on, a
   *  failed one asked again on its backoff), asked ahead of the agreement's own. */
  lazy val identityListingReadiness: services.identity.ListingReadiness =
    new services.identityadapters.AgreementReadiness(listing => agreementStage.questionsOf(listing), identityQuestionMemory.admits,
      questions => { services.identityadapters.AgreementQuestions.enqueue(taskQueue, questions, identityQuestionMemory, clock,
        agreementQuestionMetrics, claimAhead = scala.concurrent.duration.Duration.Zero); () },
      retrying = identityQuestionMemory.attempted)
  /** The questions the live stage waits on that a lost task left unattempted, or that failed and are due again, queued
   *  again: under the settle rule an unattempted question holds its cluster. */
  lazy val outstandingQuestions: services.tmdb.OutstandingQuestions =
    new services.tmdb.OutstandingQuestions(() => agreementStage.wanted, identityQuestionMemory, enqueueAgreementQuestions, clock)
  /** Runs [[outstandingQuestions]] every [[services.tmdb.OutstandingQuestions.Interval]]. */
  lazy val outstandingQuestionsSchedule: services.tasks.ClaimedPeriodicTask = managedResources.stopping(
    new services.tasks.ClaimedPeriodicTask("identity-outstanding-questions", () => outstandingQuestions.reask().enqueued.run,
      services.tmdb.OutstandingQuestions.Interval, services.tmdb.OutstandingQuestions.Interval, scheduledRunStore, clock, ticks, threadPools = threadPools))
  /** How many of a new cluster's family questions are asked inline at once: one per family and kind, roughly. */
  private val InlineFamilyAskThreads = 16
  /** An agreement stage over this country's answers: the live one ([[agreementStage]]), and the drift check's, which keeps
   *  nothing, asks nothing and reads every take at once. Both read what was attempted from the one memory
   *  ([[identityQuestionMemory]]): a cluster settles once every question it waits on was. */
  def newAgreementStage(stored: services.identity.agreement.AgreementVerdicts, stageClock: java.time.Clock,
                        ask: Set[services.identity.vocabulary.Question] => Unit = _ => (),
                        metrics: services.identity.agreement.AgreementStage.Metrics = services.identity.agreement.AgreementStage.Metrics.Silent,
                        budgets: Boolean = true,
                        askNow: services.identity.agreement.AgreementStage.AskNow = services.identity.agreement.AgreementStage.AskNow.Never,
                        staging: Option[Seq[services.identity.staging.StagingEvent] => Unit] = None,
                        keys: services.identity.agreement.AgreementKeys = new services.identity.agreement.InMemoryAgreementKeys)
      : services.identity.agreement.AgreementStage =
    services.identity.agreement.AgreementStageComposition(
      services.identity.agreement.AgreementStageComposition.Sources(
        families = agreementFamilies.map(family => family -> familyAnswerStore.answers(family)).toMap,
        venuePages = venueDetailLookups(),
        // the fill's resolve of a cluster alone asks what the model's whole-corpus resolve never did: read as the model
        // reads (the store, else live), never from the store alone, where those questions were gaps nothing asked
        tmdb = cutoverLookups(),
        tmdbOf = imdbId => identityTmdbStore.foundFirst(imdbId).toAnswer, stored = stored, clock = stageClock,
        normalizer = titleNormalizer, identities = AgreementWiring.identities(country.code),
        listedOn = filmwebProgrammes.flatMap(_ => AgreementWiring.listedOn(country.code)),
        changes = familyAnswerStore.changes, posters = posterAnswerStore, catalogue = catalogueAnswerStore,
        attempted = identityQuestionMemory.attempted, givenUp = identityQuestionMemory.givenUp, attemptsMarked = Some(() => identityQuestionMemory.marks), keys = keys),
      services.identity.agreement.AgreementStageComposition.Effects(ask = ask, metrics = metrics, askNow = askNow, staging = staging,
        pressed = () => tools.HeapPressure.pressed()),
      overrides = if (budgets) Set.empty else Set(services.identity.agreement.AgreementStageComposition.Override.Unbudgeted))

  // No drift check runs on the worker: the one it had (deleted) resolved everything whole beside the model's own state, filled
  // worker-us's tenured gen and OOM-killed it at its first nightly check two nights running (2026-10-23/24, 525 -> 768 of
  // 768 MiB in ~75 s). Production's live takes are compared off the worker, on CI after each nightly recording
  // (scripts.IdentityLiveDrift, identity-live-drift.yml).
  /** The posters' hashes the agreement's poster evidence reads, filed among the families' answers; a film's release day
   *  read off its stored TMDB record when its posters are filed (a lookup made for the one filing, none kept). */
  lazy val posterAnswerStore: services.identityadapters.PosterAnswerStore = new services.identityadapters.PosterAnswerStore(familyAnswerStore, clock,
    released = tmdbId => storedLookups().film(tmdbId).answered.flatten.flatMap(_.released))
  /** The venue film pages whose catalogue links the catalogue take reads, each through the fetch its client scrapes with. */
  lazy val catalogueLinkReader: services.identityadapters.CatalogueLinkReader = new services.identityadapters.CatalogueLinkReader(Seq(
    services.cinemas.common.FlicksClient.CatalogueLinkPages -> flicksFetch,
    services.cinemas.pl.KinotekaClient.CatalogueLinkPages   -> httpFetch,
    services.cinemas.us.AlamoDrafthouseClient.CatalogueLinkPages -> cinemaScraperCatalog.alamoDetailHttp))
  /** The catalogue ids' mappings and the venue pages' catalogue links, filed among the families' answers. */
  lazy val catalogueAnswerStore: services.identityadapters.CatalogueAnswerStore =
    new services.identityadapters.CatalogueAnswerStore(familyAnswerStore, clock, catalogueLinkReader.pages)
  /** Maps a catalogue id to its film: Wikidata's statement of it, a Letterboxd slug by its own page else. */
  lazy val catalogueMapping: services.identityadapters.CatalogueMapping = new services.identityadapters.WikidataCatalogueMapping(wikidataClient, letterboxdClient)
  /** Hashes a venue's or a TMDB film's poster: the image through the enrichment fetch chain (its pacing and breakers; a
   *  host blocking the worker through its scrapes' egress), cut to the card's slot under the process's decode gate. */
  lazy val posterHashing: services.identityadapters.PosterHashing = new services.identityadapters.PosterHashing(
    services.sharecards.PosterDownload.routed(new services.sharecards.EgressPosterDownload(enrichmentFetch), posterEgressRoutes),
    posterShrinker, tmdbId => tmdbClient.posters(tmdbId, also = Seq("en")), country.language.getLanguage)
  /** How the agreement's questions are enqueued and asked, per family and outcome. */
  lazy val agreementQuestionMetrics: services.identityadapters.AgreementQuestionMetrics = workerMetrics.identityAgreement.questions(country.code)
  /** The agreement's verdicts (`identity_agreements`), kept as the model's families are. */
  lazy val agreementVerdicts: services.identity.agreement.AgreementVerdicts =
    mongoConnection.database.fold[services.identity.agreement.AgreementVerdicts](new services.identity.agreement.InMemoryAgreementVerdicts)(
      new services.identity.agreement.MongoAgreementVerdicts(_))
  /** The agreement's cluster keys (`identity_agreement_keys`), which its verdicts are kept under. */
  lazy val agreementKeys: services.identity.agreement.AgreementKeys =
    mongoConnection.database.fold[services.identity.agreement.AgreementKeys](new services.identity.agreement.InMemoryAgreementKeys)(
      new services.identity.agreement.MongoAgreementKeys(_))
  /** The families' live sources the agreement fill asks. */
  lazy val familySources: Map[services.identity.vocabulary.VoterFamily, services.identityadapters.FamilySource] = {
    val all: Seq[services.identityadapters.FamilySource] = Seq(new services.identityadapters.ImdbFamily(imdbClient, imdbSearch),
      new services.identityadapters.WikiFamily(wikidataClient, country.language.getLanguage),
      new services.identityadapters.FilmwebFamily(filmwebClient, venue => filmwebProgrammes.fold(Seq.empty[services.identity.agreement.Showing])(_.of(venue))),
      new services.identityadapters.RottenTomatoesFamily(rottenTomatoesClient), new services.identityadapters.MetacriticFamily(metacriticClient))
    all.filter(source => agreementFamilies.contains(source.family)).map(source => source.family -> source).toMap
  }
  /** Each venue's programme on Filmweb — its own, else its town's — in Poland alone, where Filmweb lists programmes: what
   *  the agreement joins the venues' listings to ([[services.identity.agreement.VenueListings]]). */
  lazy val filmwebProgrammes: Option[services.cinemas.pl.FilmwebProgrammes] =
    Option.when(AgreementWiring.listedOn(country.code).isDefined)(new services.cinemas.pl.FilmwebProgrammes(enrichmentFetch,
      venue => filmwebFallbackIds.collectFirst { case (cinema, id) if cinema.displayName == venue => id },
      services.cinemas.pl.FilmwebProgrammes.townsOf,
      () => new models.VenueClock(clock).todayInPoland))
  /** Asks the agreement's questions, each filed answer asking for a projection, which reads it: on the queue, and a new
   *  cluster's family questions inline ([[agreementStage]]'s `askNow`). */
  lazy val agreementAsker: services.identityadapters.AgreementAsker =
    new services.identityadapters.AgreementAsker(familyAnswerStore, familySources,
      // what they file reaches the stage through the TMDB store's own filings ([[familyAnswerStore]]'s `awaitedBy`)
      find = imdbId => { tmdbClient.findByImdbId(imdbId); () }, reread = film => { tmdbClient.readRecordAgain(film); () },
      new services.identityadapters.AgreementPosterQuestions(posterAnswerStore, posterHashing),
      new services.identityadapters.AgreementCatalogueQuestions(catalogueAnswerStore, catalogueMapping, catalogueLinkReader),
      filed = () => {
        identityProjection.asked(IdentityProjection.Trigger.Answer)
        identityProjectionTrigger.request(services.identity.vocabulary.EventTrigger.Answer)
        // a listing the model's input gate holds on this question is taken in at the model's next drain (fix 1)
        identityModel.wake()
      }, identityQuestionMemory, agreementQuestionMetrics)
  /** Asks again at once the questions that failed against an enrichment host that is back: the open agreement questions
   *  queued, and a fill round for the model's gaps ([[services.tmdb.HostRecovery]]). */
  lazy val identityHostRecovery: services.tmdb.HostRecovery =
    new services.tmdb.HostRecovery(identityQuestionMemory, () => agreementStage.wanted,
      askAgain = questions => { enqueueAgreementQuestions(questions); () },
      fill = () => identityFillTrigger.request())
  /** What the enrichment chain's host breakers tell the identity questions: a host back is handed to
   *  [[identityHostRecovery]] on a thread of its own, off the request that brought the news. */
  def identityHostTransition(host: String, transition: tools.HostCircuitBreakerHttpFetch.Transition): Unit =
    if (services.tmdb.HostRecovery.recovers(transition)) identityHostRecoveryExecutor.execute { () =>
      try identityHostRecovery.heard(host, transition)
      catch { case scala.util.control.NonFatal(e) => logger.warn(s"[${country.code}] identity questions: $host back, but asking again failed", e) }
    }
  /** One thread for the hosts' recoveries; a recovery past a full queue runs on the request that brought it. */
  protected lazy val identityHostRecoveryExecutor: java.util.concurrent.ExecutorService =
    managedResources.executor("identity host recovery")(tools.DaemonExecutors.boundedPool(s"identity-host-recovery-${country.code}",
      threads = 1, queueCapacity = 256, tools.WhenFull.RunOnCaller))
  /** The agreement's questions as queue tasks, and the tasks of the types before them still queued. */
  lazy val agreementHandlers: Seq[services.tasks.TaskHandler] = agreementAsker +: agreementAsker.retired
}

object AgreementWiring {
  /** The countries whose titles Filmweb indexes well enough to vote (signal-combination experiment §9). */
  val FilmwebVoting: Set[String] = Set("pl", "de", "es")
  /** The families besides IMDb whose own id a film TMDB and IMDb hold no record of may stand on, in preference: Wikidata's
   *  everywhere, Filmweb's in Poland alone — a Polish database, its records of other countries' releases no identity.
   *  Replayed on the unmatched clusters (2026-10-05): DE "Jeder Acker, jede Fabrik" and "Chasing Hope – Der andere Blick"
   *  (Wikidata), PL "Płazy. Pionierzy życia na lądzie" (Filmweb), none wrong. */
  def identities(country: String): Seq[services.identity.vocabulary.VoterFamily] =
    Seq(services.identity.vocabulary.VoterFamily.Wiki) ++ Option.when(country == "pl")(services.identity.vocabulary.VoterFamily.Filmweb)
  /** The family whose site lists each venue's programme, that the agreement joins the venues' listings to: Filmweb, in
   *  Poland alone (it lists no other country's cinemas). */
  def listedOn(country: String): Option[services.identity.vocabulary.VoterFamily] =
    Option.when(country == "pl")(services.identity.vocabulary.VoterFamily.Filmweb)
}
