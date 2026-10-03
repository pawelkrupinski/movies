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
 * country's worker writes, and that a second projection over the same listings writes nothing (P2).
 * The lookups know no film, so every cluster is concluded unmatched — the identity decisions
 * themselves are the resolver's and the plan's specs'.
 */
class IdentityProjectionSpec extends AnyFlatSpec with Matchers {

  private val normalizer = SingleCountryNormalizer.titleNormalizer
  private val clock      = Clock.fixed(Instant.parse("2026-09-26T10:00:00Z"), ZoneOffset.UTC)
  private val start      = LocalDateTime.of(2026, 9, 27, 18, 0)

  private def film(cinema: Cinema, title: String, year: Option[Int], hours: Int*): CinemaMovie =
    CinemaMovie(Movie(title, releaseYear = year), cinema, None, None, None, Nil, Nil, hours.map(h => Showtime(start.plusHours(h.toLong), None)))

  private val programme: Map[Cinema, Seq[CinemaMovie]] = Map(
    Multikino  -> Seq(film(Multikino, "Lalka", Some(2026), 0, 3), film(Multikino, "Obcy", Some(1979), 1)),
    Helios     -> Seq(film(Helios, "Lalka", Some(2026), 2), film(Helios, "Diuna", Some(2021), 5)),
    KinoApollo -> Seq(film(KinoApollo, "Lalka", Some(2026), 4)),
    Rialto     -> Seq(film(Rialto, "Obcy", Some(1979), 6)),
    KinoMuza   -> Seq(film(KinoMuza, "Diuna", Some(2021), 7, 8)))

  private final class World(val repository: InMemoryMovieRepository = new InMemoryMovieRepository(normalizer = normalizer),
                            venues: Seq[Cinema] = programme.keys.toSeq,
                            listingsRead: () => Unit = () => (), announceFails: Boolean = false) {
    val refusals   = scala.collection.mutable.ListBuffer.empty[IdentityProjectionMetrics.Refusal]
    val cache      = new CaffeineMovieCache(repository, normalizer = normalizer)
    val archive    = new InMemoryScrapeArchiveRepository
    val accepted   = new InMemoryScrapeArchiveRepository
    val filmIds    = new InMemoryFilmIdCounterStore
    val intake     = new IdentityListingIntake(accepted, archive, new InMemoryScrapeGuardLedger, normalizer, 3, clock,
      services.movies.ScrapeLandingMetrics.noop)
    val announced  = scala.collection.mutable.ListBuffer.empty[CacheKey]
    /** Every venue whose rows the projection read back, showtimes and all, to build its slots. */
    val rowsRead   = scala.collection.mutable.ListBuffer.empty[Cinema]
    val projection = new IdentityProjection(
      listings = () => { listingsRead(); intake.projected(venues) }, rows = venues => { rowsRead ++= venues; intake.rowsOf(venues) },
      resolve = IdentityProjection.resolving(() => NoFilmLookups, new InMemoryPinStore, normalizer, IdentityCalibration.resolver), cache = cache,
      filmIds = filmIds, details = (_, _) => None, announce = (k, _) => { if (announceFails) throw new IllegalStateException(s"bus down for ${k.cleanTitle}"); announced += k; () }, normalizer = normalizer,
      slots = new CinemaSlotBuilder(Country.Poland.language, new StringPool),
      tokens = ScreeningTokens.of(Country.Poland), metrics = new IdentityProjectionMetrics {
        def projected(films: Int, listings: Int, regroupings: Regroupings, canary: Map[ShadowRelation, Int], seconds: Double): Unit = ()
        def refused(reason: IdentityProjectionMetrics.Refusal): Unit = { refusals += reason; () }
      }, clock = clock)

    def scrape(listings: Map[Cinema, Seq[CinemaMovie]]): Unit = listings.foreach { case (c, fs) =>
      archive.record(ScrapeAttempt(c, Cinema.cityOf(c), clock.instant(), listingComplete = true, fs))
      intake.recordCinemaScrape(c, fs)
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
    w.filmIds.allChecked().required.map(_.filmId).toSet shouldBe w.repository.findAll().map(_.id.value).toSet
  }

  "A projection's failures" should "be logged WITH their stacks, the announce naming its film, and a failed projection counted" in {
    val name   = classOf[IdentityProjection].getName
    val warned = (body: World => Unit) => {
      val w = new World(announceFails = true)
      w.scrape(programme)
      tools.LogCapture.thisThread(name)(body(w)).filter(_.getLevel == ch.qos.logback.classic.Level.WARN)
    }
    val announces = warned(_.projection.tick()).filter(_.getFormattedMessage.contains("announcing"))
    announces should have size 3
    announces.map(e => Option(e.getThrowableProxy).map(_.getMessage)).flatten.sorted shouldBe
      Seq("bus down for Diuna", "bus down for Lalka", "bus down for Obcy")

    val down   = new World(listingsRead = () => throw new IllegalStateException("archive down"))
    val failed = tools.LogCapture.thisThread(name)(down.projection.tickQuietly()).filter(_.getLevel == ch.qos.logback.classic.Level.WARN)
    failed.map(e => Option(e.getThrowableProxy).map(_.getMessage)) shouldBe Seq(Some("archive down"))
    down.refusals.toSeq shouldBe Seq(IdentityProjectionMetrics.Refusal.Failed) // and counted: the stored films keep serving
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
    tick.slotMisses shouldBe ((1, 0, 0))   // the one venue's rows moved; no prior slot, no new listing
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

  // One film at many venues: each venue's key read the film's prior slots, so a film showing at N venues was
  // scanned N times a tick — quadratic in the venues, and a US release shows at hundreds.
  it should "read a widely shown film's prior slots once a tick, not once per venue" in {
    val venues = Cinema.all.distinct.take(400)
    val w      = new World(venues = venues)
    w.scrape(venues.map(c => c -> Seq(film(c, "Diuna", Some(2021), 1, 2, 3))).toMap)
    w.projection.tick(); w.projection.tick()
    val tick  = w.projection.tick()
    val draft = tick.phases.find(_.name == "draft").get
    info(f"draft over one film at ${venues.size} venues: ${draft.seconds}%.2fs, ${draft.allocatedBytes / 1e6}%.1f MB")
    tick.slotsBuilt shouldBe 0
    draft.allocatedBytes should be < 4_000_000L   // 7.2 MB scanning the film per venue, 2.2 MB once
  }

  it should "rebuild, writing a widely shown film that changed at one venue, only that venue — and serve every showtime" in {
    val venues = Cinema.all.distinct.take(400)
    // As production stores a film: its showtimes in `screenings`, its slots in `movie_slots`, the cache's stripped.
    val split  = new InMemoryMovieRepository(screenings = Some(new services.movies.InMemoryScreeningsRepository),
      slots = Some(new services.movies.InMemorySlotsRepository), normalizer = normalizer)
    val w      = new World(split, venues = venues)
    w.scrape(venues.map(c => c -> Seq(film(c, "Diuna", Some(2021), 1, 2, 3))).toMap)
    w.projection.tick(); w.projection.tick()
    w.scrape(Map(venues.head -> Seq(film(venues.head, "Diuna", Some(2021), 1, 2, 3, 4))))
    val tick    = w.projection.tick()
    val compare = tick.phases.find(_.name == "compare").get
    info(f"compare, one of ${venues.size} venues changed: ${compare.seconds}%.2fs, ${compare.allocatedBytes / 1e6}%.1f MB")
    compare.allocatedBytes should be < 1_000_000L   // 1.9 MB rebuilding all 400 venues, 0.1 MB the one
    tick.written shouldBe 1
    w.showtimes shouldBe venues.flatMap(c => Seq(1, 2, 3).map(h => c.displayName -> start.plusHours(h.toLong))).toSet +
      (venues.head.displayName -> start.plusHours(4))
  }

  // A US projection held every listing's showtimes (1.7M, ~560 MB live with their rows) and again in every film's
  // built slots, through the whole tick: 18 back-to-back full GCs a projection, on a heap of 853 MB.
  "A projection" should "hold no listing's showtimes, nor any in the films it plans, and read rows only for the venues it builds" in {
    // As production stores a film: its showtimes in `screenings`, its slots in `movie_slots`, the cache's stripped.
    val w = new World(new InMemoryMovieRepository(screenings = Some(new services.movies.InMemoryScreeningsRepository),
      slots = Some(new services.movies.InMemorySlotsRepository), normalizer = normalizer))
    w.scrape(programme)
    val first = w.projection.tick()
    first.plan.get.films.flatMap(_.record.data.values).flatMap(_.showtimes) shouldBe empty
    w.showtimes shouldBe allShowtimes
    w.rowsRead.toSet shouldBe programme.keySet
    w.projection.tick()
    w.rowsRead.clear()
    w.projection.tick().wroteNothing shouldBe true
    w.rowsRead shouldBe empty
    w.scrape(Map(KinoMuza -> Seq(film(KinoMuza, "Diuna", Some(2021), 7, 8, 9))))
    w.projection.tick().written shouldBe 1
    w.rowsRead.toSet shouldBe Set(KinoMuza)
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
