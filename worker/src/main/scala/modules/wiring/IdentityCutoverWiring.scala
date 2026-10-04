package modules.wiring

import modules.WorkerWiring
import services.identity.{FilmIdCounterStore, IdentityListingIntake, IdentityProjection,
  InMemoryFilmIdCounterStore, InMemoryVenueSlotFingerprints, MongoFilmIdCounterStore, MongoVenueSlotFingerprints, VenueSlotFingerprints}
import services.movies.{CinemaSlotBuilder, ScrapeHealth}
import services.scrapes.{MongoScrapeArchiveRepository, ScrapeArchiveRepository}
import settings.IdentityProjectionInterval

import scala.concurrent.duration.DurationInt

/**
 * How a country's films are made (docs/design/identity-resolver.md §8; the cut-over itself is told in
 * docs/design/identity-cutover-runbook.md):
 *  - a scrape goes to [[IdentityListingIntake]] (the venue's accepted listing);
 *  - the settle tick runs [[IdentityProjection]], every `KINOWO_IDENTITY_PROJECTION_SECONDS` (5 minutes
 *    by default), which writes the identity model's films.
 */
trait IdentityCutoverWiring { self: WorkerWiring =>

  /** `KINOWO_IDENTITY_PROJECTION_SECONDS` — the projection's period. */
  def identityProjectionInterval: IdentityProjectionInterval =
    configuration.identityProjectionInterval(IdentityProjectionInterval(5.minutes))

  /** Each venue's accepted listing (`identity_listings`). */
  lazy val acceptedListings: ScrapeArchiveRepository =
    new MongoScrapeArchiveRepository(mongoConnection.database, IdentityListingIntake.Collection)

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
      listings    = () => identityListingIntake.projectedByVenue(cinemaScrapers.map(_.cinema)),
      changedListings = Some(() => identityListingIntake.projectedChanged(cinemaScrapers.map(_.cinema))),
      rows        = identityListingIntake.rowsOf,
      resolve     = IdentityProjection.modelled(identityModel, IdentityCutoverWiring.ModelTimeout),
      cache       = movieCache,
      filmIds     = filmIdCounterStore,
      details     = movieService.withFilmDetails,
      announce    = movieService.announceReidentified,
      normalizer  = titleNormalizer,
      slots       = new CinemaSlotBuilder(country.language, workerMetrics.stringPool),
      tokens      = screeningTokens,
      metrics     = workerMetrics.identityCutover.forCountry(country.code),
      clock       = clock,
      fingerprints = venueSlotFingerprints,
      adopt       = identityListingIntake.adopt,
      agreement   = (resolution, listingOf) => agreementStage.apply(resolution, listingOf, familyAnswerStore.version))

  // ── Agreement: the no-matches ≥3 other film database families agree on ─────────────────────
  /** What the other film database families answered (`identity_family_answers`), kept long. */
  lazy val familyAnswerStore: services.identity.FamilyAnswerStore = new services.identity.FamilyAnswerStore(identityTmdbDocuments, clock)
  /** The families asked in this country: Filmweb only where it indexes the country's titles (measured 2026-10-04: it helps
   *  in PL, DE and ES, adds nothing in the UK, and its dissent blocks a right US agreement). */
  def agreementFamilies: Seq[services.identity.agreement.VoterFamily] = {
    import services.identity.agreement.VoterFamily
    Seq(VoterFamily.Imdb, VoterFamily.Wiki, VoterFamily.Metacritic, VoterFamily.RottenTomatoes) ++
      Option.when(IdentityCutoverWiring.FilmwebVoting(country.code))(VoterFamily.Filmweb)
  }
  lazy val agreementStage: services.identity.agreement.AgreementStage =
    new services.identity.agreement.AgreementStage(agreementFamilies.map(family => family -> familyAnswerStore.answers(family)).toMap,
      new services.identity.VenueDetailLookups(detailEnrichers, venuePageIndex, services.identity.ObservationReads.Untracked),
      titleNormalizer, services.identity.IdentityCalibration.resolver,
      tmdbOf = imdbId => identityTmdbStore.get(services.identity.TmdbKind.Query, Seq(services.identity.TmdbStore.findId(imdbId))).get(services.identity.TmdbStore.findId(imdbId))
        .fold[services.identity.Answer[Option[Int]]](services.identity.Answer.Unknown)(d =>
          services.identity.Answer.Known(services.identity.TmdbStore.intsOf(d.get("ids")).headOption)),
      stored = agreementVerdicts,
      ask = open => services.identity.AgreementQuestions.enqueueOpen(taskQueue, open.questions, open.finds, clock))
  /** The agreement's verdicts (`identity_agreements`), kept as the model's families are. */
  lazy val agreementVerdicts: services.identity.agreement.AgreementVerdicts =
    mongoConnection.database.fold[services.identity.agreement.AgreementVerdicts](new services.identity.agreement.InMemoryAgreementVerdicts)(
      new services.identity.agreement.MongoAgreementVerdicts(_))
  /** The families' live sources the agreement fill asks. */
  lazy val familySources: Map[services.identity.agreement.VoterFamily, services.identity.FamilySource] = {
    val all: Seq[services.identity.FamilySource] = Seq(new services.identity.ImdbFamily(imdbClient),
      new services.identity.WikiFamily(wikidataClient, country.language.getLanguage), new services.identity.FilmwebFamily(filmwebClient),
      new services.identity.RottenTomatoesFamily(rottenTomatoesClient), new services.identity.MetacriticFamily(metacriticClient))
    all.filter(source => agreementFamilies.contains(source.family)).map(source => source.family -> source).toMap
  }
  /** The agreement's questions as queue tasks: each family question asked of its live source, and TMDB's find of an
   *  agreed IMDb id — each filed answer asking for a projection, which reads it. */
  lazy val agreementHandlers: Seq[services.tasks.TaskHandler] = Seq(
    new services.identity.AgreementQuestionHandler(familyAnswerStore, familySources,
      () => identityProjectionTrigger.request(services.identity.ProjectionTrigger.Answer), clock),
    new services.identity.AgreementFindHandler(imdbId => { tmdbClient.findByImdbId(imdbId); () },
      () => identityProjectionTrigger.request(services.identity.ProjectionTrigger.Answer), clock))
  /** Projects what moved: as the identity model takes this worker's scrapes in, and as another writer moves a stored film
   *  (`MovieCache.onChanged`) — there is no period between. */
  lazy val identityProjectionTrigger: services.identity.ProjectionTrigger = {
    val trigger = new services.identity.ProjectionTrigger(() => identityProjection.tickChangedQuietly(),
      services.movies.MovieChangeStream.Debounce.Worker,
      identityProjectionTriggerScheduler, clock)
    movieCache.onChanged(_ => trigger.request())
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

object IdentityCutoverWiring {
  /** How long a projection waits for the model to catch up — a rebuild after a deploy included —
   *  before it refuses and the stored films keep serving. */
  val ModelTimeout: scala.concurrent.duration.FiniteDuration = scala.concurrent.duration.Duration(10, "minutes")
  /** The countries whose titles Filmweb indexes well enough to vote (signal-combination experiment §9). */
  val FilmwebVoting: Set[String] = Set("pl", "de", "es")
}
