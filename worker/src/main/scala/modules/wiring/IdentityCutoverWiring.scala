package modules.wiring

import modules.WorkerWiring
import services.identity.{CutoverTaskHandlers, FilmIdCounterStore, IdentityCalibration, IdentityListingIntake,
  IdentityProjection, InMemoryFilmIdCounterStore, MongoFilmIdCounterStore, MongoPinStore}
import services.movies.{CinemaSlotBuilder, ScrapeHealth}
import services.scrapes.{MongoScrapeArchiveRepository, ScrapeArchiveRepository}
import services.tasks.TaskHandler
import settings.IdentityProjectionInterval

import scala.concurrent.duration.DurationInt

/**
 * The identity migration's PHASE 5 switch (docs/design/identity-resolver.md §8, "cutover, per
 * country"; the runbook is docs/design/identity-cutover-runbook.md). `KINOWO_IDENTITY_CUTOVER`
 * names the countries whose films are the identity projection's; it is read HERE, once, and
 * nothing below the composition root knows it exists — every class sees only the collaborators
 * this trait picks.
 *
 * For a cut-over country:
 *  - a scrape goes to [[IdentityListingIntake]] (the venue's accepted listing), not the landing;
 *  - the settle tick runs [[IdentityProjection]] instead of the settle, every
 *    `KINOWO_IDENTITY_PROJECTION_SECONDS` (5 minutes by default);
 *  - the old identity path's tasks (deferred detail, the TMDB resolve, the staging chain) are
 *    completed unrun, and its reapers (detail, unresolved-TMDB, staging) are not started.
 * Every other country is wired exactly as before.
 */
trait IdentityCutoverWiring { self: WorkerWiring =>

  /** Whether this country is cut over to the identity projection. */
  lazy val identityCutover: Boolean = configuration.identityCutover.covers(country)

  /** `KINOWO_IDENTITY_PROJECTION_SECONDS` — the projection's period. */
  def identityProjectionInterval: IdentityProjectionInterval =
    configuration.identityProjectionInterval(IdentityProjectionInterval(5.minutes))

  /** Each venue's accepted listing (`identity_listings`); written only in a cut-over country. */
  lazy val acceptedListings: ScrapeArchiveRepository =
    new MongoScrapeArchiveRepository(mongoConnection.database, IdentityListingIntake.Collection)

  /** The persisted FilmId map (`identity_film_ids`), in memory without a database. */
  lazy val filmIdCounterStore: FilmIdCounterStore =
    mongoConnection.database.fold[FilmIdCounterStore](new InMemoryFilmIdCounterStore)(new MongoFilmIdCounterStore(_))

  // What each venue is taken to publish after every scrape reaches the identity model as its listings now.
  lazy val identityListingIntake: Option[IdentityListingIntake] = Option.when(identityCutover)(
    new IdentityListingIntake(acceptedListings, scrapeArchive, scrapeGuardLedger, titleNormalizer,
      ScrapeHealth.maxRejectionsFor(scrapeFreshness), clock,
      published = (cinema, films) => identityModel.foreach(_.venueScraped(cinema, films))))

  lazy val identityProjection: Option[IdentityProjection] = identityListingIntake.map { intake =>
    new IdentityProjection(
      listings    = () => intake.listings(cinemaScrapers.map(_.cinema)),
      resolve     = identityModel.fold(IdentityProjection.resolving(
        () => cutoverLookups(),
        new MongoPinStore(mongoConnection.database), titleNormalizer, IdentityCalibration.resolver))(
        IdentityProjection.modelled(_, IdentityCutoverWiring.ModelTimeout)),
      cache       = movieCache,
      filmIds     = filmIdCounterStore,
      details     = movieService.withFilmDetails,
      announce    = movieService.announceResolvedNewMovie,
      normalizer  = titleNormalizer,
      slots       = new CinemaSlotBuilder(country.language, workerMetrics.stringPool),
      tokens      = screeningTokens,
      metrics     = workerMetrics.identityCutover.forCountry(country.code),
      clock       = clock,
      wrote       = () => venueDetailSlots.refresh())
  }

  /** `handlers` as this country runs them: unchanged, or — cut over — with the old identity
   *  path's task types completed unrun. */
  def identityPathHandlers(handlers: Seq[TaskHandler]): Seq[TaskHandler] =
    if (identityCutover) CutoverTaskHandlers.of(handlers) else handlers
}

object IdentityCutoverWiring {
  /** How long a projection waits for the model to catch up — a rebuild after a deploy included —
   *  before it refuses and the stored films keep serving. */
  val ModelTimeout: scala.concurrent.duration.FiniteDuration = scala.concurrent.duration.Duration(10, "minutes")
}
