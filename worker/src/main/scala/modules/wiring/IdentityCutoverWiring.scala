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
      adopt       = identityListingIntake.adopt)

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
}
