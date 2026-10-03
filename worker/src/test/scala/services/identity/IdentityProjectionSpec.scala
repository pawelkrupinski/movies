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

  private final class World(val repository: InMemoryMovieRepository = new InMemoryMovieRepository(normalizer = normalizer)) {
    val cache      = new CaffeineMovieCache(repository, normalizer = normalizer, clock = clock)
    val archive    = new InMemoryScrapeArchiveRepository
    val accepted   = new InMemoryScrapeArchiveRepository
    val filmIds    = new InMemoryFilmIdCounterStore
    val intake     = new IdentityListingIntake(accepted, archive, new InMemoryScrapeGuardLedger, normalizer, 3, clock,
      services.movies.ScrapeLandingMetrics.noop)
    val announced  = scala.collection.mutable.ListBuffer.empty[CacheKey]
    val projection = new IdentityProjection(
      listings = () => intake.listings(programme.keys.toSeq),
      resolve = IdentityProjection.resolving(() => NoFilms, new InMemoryPinStore, normalizer, IdentityCalibration.resolver), cache = cache,
      filmIds = filmIds, details = (_, _) => None, announce = (k, _) => { announced += k; () }, normalizer = normalizer,
      slots = new CinemaSlotBuilder(Country.Poland.language, new StringPool),
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

  // One at a time, a US boot's first projection waited on ~2,250 films' round-trips in a row.
  it should "write films nothing else holds or takes side by side" in {
    val inFlight = new java.util.concurrent.atomic.AtomicInteger
    val most     = new java.util.concurrent.atomic.AtomicInteger
    val slow = new InMemoryMovieRepository(normalizer = normalizer) {
      override def upsert(film: services.movies.FilmId, key: CacheKey, e: models.MovieRecord): services.movies.WriteOutcome = {
        most.accumulateAndGet(inFlight.incrementAndGet(), math.max)
        try { Thread.sleep(50); super.upsert(film, key, e) } finally { inFlight.decrementAndGet(); () }
      }
    }
    val w = new World(slow)
    w.scrape(programme)
    w.projection.tick().written shouldBe 3
    w.repository.findAll().map(_.title).sorted shouldBe Seq("Diuna", "Lalka", "Obcy")
    w.showtimes shouldBe allShowtimes
    most.get should be > 1
  }

  "The films a projection writes side by side" should "be only those no other film holds or takes" in {
    val w = new World
    w.scrape(programme)
    val first   = w.projection.tick().plan.get
    val stored  = w.repository.findAll()
    val renamed = first.films.map(f => if (f.title == "Obcy") f.copy(id = services.movies.FilmId("new-obcy")) else f)
    // Every film already stored: none is new. A fresh id under a key a stored film holds is not free either.
    IdentityProjection.independent(first.films, first.films, stored, normalizer) shouldBe empty
    IdentityProjection.independent(renamed, renamed, stored, normalizer) shouldBe empty
    // New to an empty store, each under its own key: all of them — unless two of them take one key.
    IdentityProjection.independent(first.films, first.films, Nil, normalizer) shouldBe first.films.map(_.id).toSet
    val twice = first.films :+ first.films.head.copy(id = services.movies.FilmId("twin"))
    IdentityProjection.independent(twice, twice, Nil, normalizer) shouldBe first.films.tail.map(_.id).toSet
  }

  "A projection" should "say how long each of its phases took and what it allocated" in {
    val w = new World
    w.scrape(programme)
    val phases = w.projection.tick().phases
    phases.map(_.name) shouldBe Seq("listings", "snapshot", "resolve", "draft", "guard", "details", "finish", "compare", "writes")
    phases.foreach(p => (p.seconds >= 0 && p.allocatedBytes >= 0) shouldBe true)
    phases.map(_.allocatedBytes).sum should be > 0L
  }

  // The first projection builds every slot over no stored film; the second over what the first stored
  // (the prior slots a slot carries detail forward from), and from then on only what moves is rebuilt.
  "A projection over unchanged listings" should "build no venue slot again, reusing the last projection's" in {
    val w = new World
    w.scrape(programme)
    w.projection.tick().slotsReused shouldBe 0
    val first = w.projection.tick()
    first.slotsBuilt should be > 0
    val again = w.projection.tick()
    again.slotsBuilt shouldBe 0
    again.slotsReused shouldBe first.slotsBuilt
    again.wroteNothing shouldBe true
  }

  it should "rebuild only the venue whose listing changed, and write that film with every showtime" in {
    val w = new World
    w.scrape(programme)
    w.projection.tick()
    val first = w.projection.tick()
    val later = film(KinoMuza, "Diuna", Some(2021), 7, 8, 9)
    w.scrape(Map(KinoMuza -> Seq(later)))
    val tick = w.projection.tick()
    tick.slotsBuilt shouldBe 1
    tick.slotsReused shouldBe first.slotsBuilt - 1
    tick.written shouldBe 1
    w.showtimes shouldBe allShowtimes + (KinoMuza.displayName -> start.plusHours(9))
  }

  it should "write in full a film whose stored showtimes went astray, though its slots came from the memo" in {
    val w = new World
    w.scrape(programme)
    w.projection.tick(); w.projection.tick()
    // The stored film loses its showtimes behind the projection's back; nothing it is built from moves.
    val obcy = w.repository.findAll().find(_.title == "Obcy").get
    w.repository.upsert(obcy.id, CacheKey.stored(obcy.title, obcy.key(normalizer)),
      obcy.record.copy(data = obcy.record.data.map { case (source, slot) => source -> slot.copy(showtimes = Nil) }))
    w.cache.rehydrate()
    w.showtimes should not be allShowtimes
    val tick = w.projection.tick()
    tick.slotsBuilt shouldBe 0
    tick.written shouldBe 1
    w.showtimes shouldBe allShowtimes
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

  it should "be written at once when the films leaving have no showtime still to come, as they are on no card" in {
    val w = new World
    // "Obcy" screened yesterday (start − 40 h is before the clock) and is listed nowhere now.
    w.scrape(programme.map { case (c, fs) => c -> fs.map(f => if (f.movie.title == "Obcy") film(c, "Obcy", Some(1979), -40) else f) })
    w.projection.tick()
    w.scrape(Map(Rialto -> Seq(film(Rialto, "Diuna", Some(2021), 9)), Multikino -> programme(Multikino).take(1)))
    w.projection.tick().refused shouldBe None
    w.repository.findAll().map(_.title) should not contain "Obcy"
  }

  it should "name the films it would take off the site" in {
    val w = new World
    w.scrape(programme)
    w.projection.tick()
    w.scrape(Map(Rialto -> Seq(film(Rialto, "Diuna", Some(2021), 9)), Multikino -> programme(Multikino).take(1)))
    w.projection.tick().refused.get should include("Obcy")
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

  /** A country's first projection fetches the TMDB details of every film it matched (~2,250 on a US
   *  boot): one at a time, it waited on each in a row. */
  "A projection's TMDB details" should "be fetched side by side, each onto its own draft" in {
    val drafts = (1 to 4).map(i => FilmDraft(i.toLong, None, Nil, models.MovieRecord(tmdbId = Some(100 + i)), s"Film $i")) :+
      FilmDraft(5L, None, Nil, models.MovieRecord(), "Unmatched")
    // Each fetch waits for another to be under way: fetched one at a time, the first never returns.
    val together = new java.util.concurrent.CyclicBarrier(2)
    val fetched  = IdentityProjection.detailed(drafts, (record, film) => {
      if (film <= 102) together.await(5, java.util.concurrent.TimeUnit.SECONDS)
      Some(record.copy(imdbId = Some(s"tt$film")))
    })
    fetched.map(_.record.imdbId) shouldBe Seq(Some("tt101"), Some("tt102"), Some("tt103"), Some("tt104"), None)
    fetched.map(_.anchor) shouldBe drafts.map(_.anchor)
  }
}
