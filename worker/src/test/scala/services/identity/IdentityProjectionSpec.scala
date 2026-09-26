package services.identity

import models.{Cinema, CinemaMovie, CinemaShowing, Country, Helios, KinoApollo, KinoMuza, Movie, Multikino, Rialto, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.{CacheKey, CaffeineMovieCache, CinemaSlotBuilder, InMemoryMovieRepository, InMemoryScrapeGuardLedger,
  ScreeningTokens, SingleCountryNormalizer, StringPool}
import services.scrapes.{InMemoryScrapeArchiveRepository, ScrapeAttempt}

import java.time.{Clock, Instant, LocalDateTime, ZoneOffset}

/**
 * The identity projection wired over the real cache and an in-memory store: what a cut-over
 * country's worker writes, that a second projection over the same listings writes nothing (P2),
 * and that switching a country over and back loses no showtime and leaves rows each path reads.
 * The lookups know no film, so every cluster is concluded unmatched — the identity decisions
 * themselves are the resolver's and the plan's specs'.
 */
class IdentityProjectionSpec extends AnyFlatSpec with Matchers {

  private val normalizer = SingleCountryNormalizer.titleNormalizer
  private val clock      = Clock.fixed(Instant.parse("2026-09-26T10:00:00Z"), ZoneOffset.UTC)
  private val start      = LocalDateTime.of(2026, 9, 27, 18, 0)

  private object NoFilms extends IdentityLookups {
    def hasDetail(listing: Listing): Boolean                          = false
    def detail(listing: Listing): Answer[Option[DetailFacts]]         = Answer.Known(None)
    def candidates(query: CandidateQuery): Answer[Seq[Hit]]           = Answer.Known(Nil)
    def film(tmdbId: Int): Answer[Option[IdentityMeasures.Film]]      = Answer.Known(None)
  }

  private def film(cinema: Cinema, title: String, year: Option[Int], hours: Int*): CinemaMovie =
    CinemaMovie(Movie(title, releaseYear = year), cinema, None, None, None, Nil, Nil, hours.map(h => Showtime(start.plusHours(h.toLong), None)))

  private val programme: Map[Cinema, Seq[CinemaMovie]] = Map(
    Multikino  -> Seq(film(Multikino, "Lalka", Some(2026), 0, 3), film(Multikino, "Obcy", Some(1979), 1)),
    Helios     -> Seq(film(Helios, "Lalka", Some(2026), 2), film(Helios, "Diuna", Some(2021), 5)),
    KinoApollo -> Seq(film(KinoApollo, "Lalka", Some(2026), 4)),
    Rialto     -> Seq(film(Rialto, "Obcy", Some(1979), 6)),
    KinoMuza   -> Seq(film(KinoMuza, "Diuna", Some(2021), 7, 8)))

  private final class World {
    val repository = new InMemoryMovieRepository(normalizer = normalizer)
    val cache      = new CaffeineMovieCache(repository, normalizer = normalizer, clock = clock)
    val archive    = new InMemoryScrapeArchiveRepository
    val accepted   = new InMemoryScrapeArchiveRepository
    val filmIds    = new InMemoryFilmIdCounterStore
    val intake     = new IdentityListingIntake(accepted, archive, new InMemoryScrapeGuardLedger, normalizer, 3, clock)
    val announced  = scala.collection.mutable.ListBuffer.empty[CacheKey]
    val projection = new IdentityProjection(
      listings = () => intake.listings(programme.keys.toSeq), lookups = () => NoFilms, pins = new InMemoryPinStore, cache = cache,
      filmIds = filmIds, details = (_, _) => None, announce = (k, _) => { announced += k; () }, normalizer = normalizer,
      calibration = IdentityCalibration.default, slots = new CinemaSlotBuilder(Country.Poland.language, new StringPool),
      tokens = ScreeningTokens.of(Country.Poland), metrics = IdentityProjectionMetrics.noop, clock = clock)

    def scrape(listings: Map[Cinema, Seq[CinemaMovie]]): Unit = listings.foreach { case (c, fs) =>
      archive.record(ScrapeAttempt(c, Cinema.cityOf(c), clock.instant(), listingComplete = true, fs))
      intake.recordCinemaScrape(c, fs)
    }
    def landOldPath(listings: Map[Cinema, Seq[CinemaMovie]]): Unit = listings.toSeq.sortBy(_._1.displayName).foreach { case (c, fs) =>
      archive.record(ScrapeAttempt(c, Cinema.cityOf(c), clock.instant(), listingComplete = true, fs))
      cache.recordCinemaScrape(c, fs)
    }
    /** Every (venue, showtime) the stored films hold, and each listing's film id. */
    def showtimes: Set[(String, LocalDateTime)] = repository.findAll().flatMap(_.record.data.collect {
      case (CinemaShowing(c, _), sd) => sd.showtimes.map(s => c.displayName -> s.dateTime)
    }.flatten).toSet
  }

  private val allShowtimes: Set[(String, LocalDateTime)] =
    programme.values.flatten.flatMap(cm => cm.showtimes.map(s => cm.cinema.displayName -> s.dateTime)).toSet

  "A cut-over country's first projection" should "store one film per title, every showtime on it, and announce each" in {
    val w = new World
    w.scrape(programme)
    val tick = w.projection.tick()
    tick.refused shouldBe None
    w.repository.findAll().map(_.title).sorted shouldBe Seq("Diuna", "Lalka", "Obcy")
    w.showtimes shouldBe allShowtimes
    w.repository.findAll().forall(_.record.readyToProject) shouldBe true
    w.cache.snapshot().map(_.id).toSet shouldBe w.repository.findAll().map(_.id).toSet
    w.announced.map(_.cleanTitle).sorted shouldBe Seq("Diuna", "Lalka", "Obcy")
    w.filmIds.allChecked()._1.map(_.filmId).toSet shouldBe w.repository.findAll().map(_.id.value).toSet
  }

  "A second projection over the same listings" should "write nothing (P2)" in {
    val w = new World
    w.scrape(programme)
    w.projection.tick()
    val before = w.repository.findAll().map(r => (r.id, r.key(normalizer), r.record)).toSet
    val again  = w.projection.tick()
    again.wroteNothing shouldBe true
    again.plan.get.regroupings.isEmpty shouldBe true
    w.repository.findAll().map(r => (r.id, r.key(normalizer), r.record)).toSet shouldBe before
  }

  "A projection that would take a film off the site" should "be refused for the guard's grace, then written" in {
    val w = new World
    w.scrape(programme)
    w.projection.tick()
    // Rialto and Multikino stop listing "Obcy": the film vanishes — a third of the site.
    w.scrape(Map(Rialto -> Seq(film(Rialto, "Diuna", Some(2021), 9)), Multikino -> programme(Multikino).take(1)))
    (1 to ProjectionGuard.Grace).foreach(_ => w.projection.tick().refused should not be empty)
    w.repository.findAll().map(_.title) should contain("Obcy")
    w.projection.tick().refused shouldBe None
    w.repository.findAll().map(_.title) should not contain "Obcy"
    w.cache.snapshot().map(_.title) should not contain "Obcy"
  }

  "Switching a country OVER" should "seed ids from the old path's films and keep every showtime" in {
    val w = new World
    w.landOldPath(programme)
    val oldIds = w.repository.findAll().map(r => r.title -> r.id).toMap
    w.projection.tick().refused shouldBe None
    w.repository.findAll().map(r => r.title -> r.id).toMap shouldBe oldIds
    w.showtimes shouldBe allShowtimes
    w.filmIds.allChecked()._1.map(_.filmId).toSet shouldBe oldIds.values.map(_.value).toSet
  }

  "Switching a country BACK" should "leave rows the old landing reads and re-lands onto, with no showtime lost" in {
    val w = new World
    w.scrape(programme)
    w.projection.tick()
    val projected = w.repository.findAll().map(r => r.title -> r.id).toMap
    w.landOldPath(programme)
    w.repository.findAll().map(r => r.title -> r.id).toMap shouldBe projected
    w.showtimes shouldBe allShowtimes
    w.cache.keyCollisions.get() shouldBe 0
  }
}
