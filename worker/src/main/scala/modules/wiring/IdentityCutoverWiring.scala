package modules.wiring

import modules.WorkerWiring
import services.identity.{CutoverIdentityLookups, CutoverTaskHandlers, FilmIdCounterStore, IdentityCalibration, IdentityListingIntake,
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

  lazy val identityListingIntake: Option[IdentityListingIntake] = Option.when(identityCutover)(
    new IdentityListingIntake(acceptedListings, scrapeArchive, scrapeGuardLedger, titleNormalizer,
      ScrapeHealth.maxRejectionsFor(scrapeFreshness), clock))

  lazy val identityProjection: Option[IdentityProjection] = identityListingIntake.map { intake =>
    new IdentityProjection(
      listings    = () => intake.listings(cinemaScrapers.map(_.cinema)),
      lookups     = () => CutoverIdentityLookups.over(observationStore, tmdbClientOver, lookupFetch, detailEnrichers),
      pins        = new MongoPinStore(mongoConnection.database),
      cache       = movieCache,
      filmIds     = filmIdCounterStore,
      details     = movieService.withFilmDetails,
      announce    = movieService.announceResolvedNewMovie,
      normalizer  = titleNormalizer,
      calibration = IdentityCalibration.default,
      slots       = new CinemaSlotBuilder(country.language, workerMetrics.stringPool),
      tokens      = screeningTokens,
      metrics     = workerMetrics.identityCutover.forCountry(country.code),
      clock       = clock)
  }

  /** `handlers` as this country runs them: unchanged, or — cut over — with the old identity
   *  path's task types completed unrun. */
  def identityPathHandlers(handlers: Seq[TaskHandler]): Seq[TaskHandler] =
    if (identityCutover) CutoverTaskHandlers.of(handlers) else handlers
}
