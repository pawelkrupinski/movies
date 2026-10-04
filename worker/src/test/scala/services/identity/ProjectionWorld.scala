package services.identity

import models.{Cinema, CinemaMovie, CinemaShowing, Country, MovieRecord}
import services.movies.{CacheKey, CaffeineMovieCache, CinemaSlotBuilder, InMemoryMovieRepository, InMemoryScrapeGuardLedger,
  ScreeningTokens, SingleCountryNormalizer, StringPool}
import services.scrapes.{InMemoryScrapeArchiveRepository, ScrapeAttempt}

import java.time.{Clock, LocalDateTime}

/** A cut-over country's worker, as far as its identity projection goes: the real projection over the real cache, an
 *  in-memory store, intake and FilmId map. The resolver is the whole one over lookups that know no film (every
 *  cluster concluded unmatched) unless `resolve` says otherwise, and `details` answers the projection's TMDB fetches. */
private[identity] final class ProjectionWorld(
  val repository:    InMemoryMovieRepository,
  venues:            Seq[Cinema],
  clock:             Clock,
  resolve:           (() => Seq[Listing]) => Option[IdentityProjection.Resolved] = ProjectionWorld.unmatched,
  details:           (MovieRecord, Int) => Option[MovieRecord] = (_, _) => None,
  listingsRead:      () => Unit = () => (),
  announceFails:     Boolean = false,
  val archive:       InMemoryScrapeArchiveRepository = new InMemoryScrapeArchiveRepository,
  val accepted:      InMemoryScrapeArchiveRepository = new InMemoryScrapeArchiveRepository,
  val filmIds:       InMemoryFilmIdCounterStore = new InMemoryFilmIdCounterStore,
  val fingerprints:  VenueSlotFingerprints = new InMemoryVenueSlotFingerprints,
  scopedBetweenWhole: Int = IdentityProjection.ScopedBetweenWhole) {
  import ProjectionWorld.normalizer

  /** This world's worker restarted: what Mongo holds kept, everything in memory (the cache, the intake's held
   *  listings, the projection's slot memo and what it last read) built afresh. */
  def restarted: ProjectionWorld =
    new ProjectionWorld(repository, venues, clock, resolve, details, listingsRead, announceFails, archive, accepted, filmIds, fingerprints,
      scopedBetweenWhole)

  val refusals   = scala.collection.mutable.ListBuffer.empty[IdentityProjectionMetrics.Refusal]
  val drifts     = scala.collection.mutable.ListBuffer.empty[Int]
  /** What the last projection reported: its films and canary. */
  var reported: Option[(Int, Map[ShadowRelation, Int])] = None
  val cache      = new CaffeineMovieCache(repository, normalizer = normalizer, clock = _root_.tools.SpecClock.Pinned)
  val intake     = new IdentityListingIntake(accepted, archive, new InMemoryScrapeGuardLedger, normalizer, 3, clock,
    services.movies.ScrapeLandingMetrics.noop)
  val announced  = scala.collection.mutable.ListBuffer.empty[CacheKey]
  /** Every venue whose rows the projection read back, showtimes and all, to build its slots. */
  val rowsRead   = scala.collection.mutable.ListBuffer.empty[Cinema]
  val projection = new IdentityProjection(
    listings = () => { listingsRead(); intake.projectedByVenue(venues) }, rows = venues => { rowsRead ++= venues; intake.rowsOf(venues) },
    resolve = resolve, cache = cache, filmIds = filmIds, details = details,
    announce = (k, _) => { if (announceFails) throw new IllegalStateException(s"bus down for ${k.cleanTitle}"); announced += k; () },
    normalizer = normalizer, slots = new CinemaSlotBuilder(Country.Poland.language, new StringPool),
    tokens = ScreeningTokens.of(Country.Poland), metrics = new IdentityProjectionMetrics {
      def projected(films: Int, listings: Int, regroupings: Regroupings, canary: Map[ShadowRelation, Int], seconds: Double): Unit =
        reported = Some(films -> canary)
      def refused(reason: IdentityProjectionMetrics.Refusal): Unit = { refusals += reason; () }
      def drifted(films: Int): Unit = { drifts += films; () }
    }, clock = clock, fingerprints = fingerprints, scopedBetweenWhole = scopedBetweenWhole)

  def scrape(listings: Map[Cinema, Seq[CinemaMovie]]): Unit = listings.foreach { case (c, fs) =>
    archive.record(ScrapeAttempt(c, Cinema.cityOf(c), clock.instant(), listingComplete = true, fs))
    intake.recordCinemaScrape(c, fs)
  }

  /** Every (venue, showtime) the stored films hold. */
  def showtimes: Set[(String, LocalDateTime)] = repository.findAll().flatMap(_.record.data.collect {
    case (CinemaShowing(c, _), sd) => sd.showtimes.map(s => c.displayName -> s.dateTime)
  }.flatten).toSet
}

private[identity] object ProjectionWorld {
  val normalizer: services.movies.TitleNormalizer = SingleCountryNormalizer.titleNormalizer

  /** The whole resolver over lookups that know no film. */
  val unmatched: (() => Seq[Listing]) => Option[IdentityProjection.Resolved] =
    IdentityProjection.resolving(() => NoFilmLookups, new InMemoryPinStore, normalizer, IdentityCalibration.resolver)
}
