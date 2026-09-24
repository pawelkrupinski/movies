package services.movies

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import services.movies.SingleCountryNormalizer.titleNormalizer

/**
 * The scrape-health guards across a REWIRE and across a RESTART — the two things
 * Braniewo's Baszta hit on 2026-09-23. It moved from bilety24 organiser 477 (another
 * town's programme, 77 showtimes) to its own Filmweb listing (3 showtimes), and every
 * hourly Filmweb tick was discarded as "depth-degraded" against the old source's
 * rows; the rejection count that should eventually have let it through lived in
 * memory and reset with each of the day's pod changes.
 *
 * Production's storage shape (the `screenings` + `movie_slots` split), as in
 * [[DepthGuardUnderSplitSpec]], since only under it does the depth guard see counts.
 */
class RewiredVenueGuardSpec extends AnyFlatSpec with Matchers {

  private val OldSource = Some("bilety24.pl/organizator/477")
  private val NewSource = Some("filmweb.pl/cinema/2352")

  private def scrape(films: Int, showtimesEach: Int, firstFilm: Int = 1): Seq[CinemaMovie] =
    (firstFilm until firstFilm + films).map { i =>
      val times = DepthGuardTime.showtimes(showtimesEach)
      CinemaMovie(movie = Movie(s"Film $i", releaseYear = Some(2026)), cinema = Multikino,
        posterUrl = None, filmUrl = None, synopsis = None, cast = Nil, director = Nil, showtimes = times)
    }

  private def splitRepository() = new InMemoryMovieRepository(
    screenings = Some(new InMemoryScreeningsRepository), slots = Some(new InMemorySlotsRepository))

  private def storedShowtimes(repository: InMemoryMovieRepository, title: String): Int =
    repository.findAll().find(_.title.contains(title))
      .map(_.record.data.values.map(_.showtimes.size).sum).getOrElse(0)

  private def cacheOver(repository: InMemoryMovieRepository, ledger: ScrapeGuardLedger) =
    new CaffeineMovieCache(repository, normalizer = titleNormalizer, scrapeGuardLedger = ledger,
      clock = DepthGuardTime.clock)

  "a venue rewired to a new source" should "take the new source's thin listing as its baseline at once" in {
    val repository = splitRepository()
    val cache      = cacheOver(repository, new InMemoryScrapeGuardLedger)

    cache.recordCinemaScrape(Multikino, scrape(films = 10, showtimesEach = 8), sourceKey = OldSource)
    storedShowtimes(repository, "Film 1") shouldBe 8

    // Far below both floors — a degraded fetch, were it the same source. It is not.
    cache.recordCinemaScrape(Multikino, scrape(films = 1, showtimesEach = 3, firstFilm = 20), sourceKey = NewSource)

    storedShowtimes(repository, "Film 20") shouldBe 3
    // The old source's films are gone with it: the breadth guard must not spare them.
    storedShowtimes(repository, "Film 1") shouldBe 0
    storedShowtimes(repository, "Film 10") shouldBe 0
  }

  it should "guard the new source like any other once it is the baseline" in {
    val repository = splitRepository()
    val cache      = cacheOver(repository, new InMemoryScrapeGuardLedger)

    cache.recordCinemaScrape(Multikino, scrape(films = 1, showtimesEach = 8), sourceKey = OldSource)
    cache.recordCinemaScrape(Multikino, scrape(films = 10, showtimesEach = 8), sourceKey = NewSource)
    cache.recordCinemaScrape(Multikino, scrape(films = 10, showtimesEach = 1), sourceKey = NewSource)

    storedShowtimes(repository, "Film 1") shouldBe 8
  }

  "a venue with no recorded source" should "keep its guards, and record the source on the next landed scrape" in {
    val repository = splitRepository()
    val ledger     = new InMemoryScrapeGuardLedger
    val cache      = cacheOver(repository, ledger)

    // Landed before source keys were recorded.
    cache.recordCinemaScrape(Multikino, scrape(films = 10, showtimesEach = 8))
    ledger.get(Multikino).flatMap(_.sourceKey) shouldBe None

    cache.recordCinemaScrape(Multikino, scrape(films = 10, showtimesEach = 1), sourceKey = NewSource)
    storedShowtimes(repository, "Film 1") shouldBe 8 // rejected: an unknown past is no rewire
    ledger.get(Multikino).flatMap(_.sourceKey) shouldBe None    // and a discarded tick records nothing

    cache.recordCinemaScrape(Multikino, scrape(films = 10, showtimesEach = 7), sourceKey = NewSource)
    storedShowtimes(repository, "Film 1") shouldBe 7
    ledger.get(Multikino).flatMap(_.sourceKey) shouldBe NewSource
  }

  "a venue rewired before its source was ever recorded" should "be recognised from the stored rows' own links" in {
    // Braniewo's Baszta as production held it: the programme of bilety24 organiser 477
    // (Środa Wielkopolska's cinema, recorded 2026-09-23), landed before source keys
    // existed, so no key says where it came from — while the scraper now reads Baszta's
    // own Filmweb listing (id 2352, recorded 2026-06-07). Every stored row links to
    // bilety24, every fresh one to filmweb: a different source, not a thin fetch.
    val clock      = new tools.MutableClock(java.time.Instant.parse("2026-06-01T00:00:00Z"))
    val repository = splitRepository()
    val ledger     = new InMemoryScrapeGuardLedger
    val cache      = new CaffeineMovieCache(repository, normalizer = titleNormalizer,
      scrapeGuardLedger = ledger, clock = clock)

    val sroda = services.cinemas.pl.Bilety24OrganizerClient.parse(
      services.cinemas.roster.RosterAuditFixtures.page(services.cinemas.roster.RosterAuditFixtures.Sroda477),
      KinoBaszta, titleNormalizer)
    cache.recordCinemaScrape(KinoBaszta, sroda)
    ledger.get(KinoBaszta).flatMap(_.sourceKey) shouldBe None

    val filmweb = new services.cinemas.pl.FilmwebShowtimesClient(new clients.tools.FakeHttpFetch("filmweb-catchment"),
      2352, KinoBaszta, daysAhead = 0, today = java.time.LocalDate.of(2026, 6, 7))
    val baszta  = filmweb.fetch()
    baszta should not be empty
    cache.recordCinemaScrape(KinoBaszta, baszta, sourceKey = filmweb.sourceKey)

    val held = repository.findAll().flatMap(_.record.cinemaSlots).collect {
      case (CinemaShowing(KinoBaszta, _), slot) => slot.filmUrl.flatMap(ScrapeHealth.siteOf) }.flatten.toSet
    withClue("the other town's films must be gone after the first tick of the new source: ")(
      held shouldBe Set("filmweb.pl"))
    ledger.get(KinoBaszta).flatMap(_.sourceKey) shouldBe filmweb.sourceKey
  }

  it should "still guard a keyless venue whose thin tick links to the same site as its stored rows" in {
    val repository = splitRepository()
    val cache      = cacheOver(repository, new InMemoryScrapeGuardLedger)
    def linked(movies: Seq[CinemaMovie]) = movies.map(m => m.copy(filmUrl = Some(s"https://www.bilety24.pl/kino/477-${m.movie.title.replace(' ', '-')}")))

    cache.recordCinemaScrape(Multikino, linked(scrape(films = 10, showtimesEach = 8)))
    cache.recordCinemaScrape(Multikino, linked(scrape(films = 10, showtimesEach = 1)), sourceKey = OldSource)
    storedShowtimes(repository, "Film 1") shouldBe 8
  }

  "a source change neither guard can recognise" should "clear both guards on the SAME tick once depth gives up" in {
    // No keys at all, so nothing marks it as a rewire: the depth guard's grace is the
    // only way out. When it finally accepts, the breadth guard must not then open a
    // grace of its own — that ran the two in series (Baszta: 4 + 4 hourly ticks in PL,
    // 2 + 2 slow-cadence ticks elsewhere), keeping the old source's films on the site
    // for twice the hold either guard is sized for.
    val repository = splitRepository()
    val cache      = new CaffeineMovieCache(repository, normalizer = titleNormalizer, clock = DepthGuardTime.clock,
      maxConsecutiveGuardRejections = ScrapeHealth.MaxConsecutiveDepthRejections)

    cache.recordCinemaScrape(Multikino, scrape(films = 10, showtimesEach = 8))
    (1 to ScrapeHealth.MaxConsecutiveDepthRejections).foreach { _ =>
      cache.recordCinemaScrape(Multikino, scrape(films = 1, showtimesEach = 3, firstFilm = 20))
      storedShowtimes(repository, "Film 20") shouldBe 0
    }

    cache.recordCinemaScrape(Multikino, scrape(films = 1, showtimesEach = 3, firstFilm = 20))
    storedShowtimes(repository, "Film 20") shouldBe 3
    storedShowtimes(repository, "Film 1")  shouldBe 0
    storedShowtimes(repository, "Film 10") shouldBe 0
  }

  "the guards' rejection count" should "survive a worker restart" in {
    val repository = splitRepository()
    val ledger     = new InMemoryScrapeGuardLedger // stands in for the durable store
    cacheOver(repository, ledger).recordCinemaScrape(Multikino, scrape(films = 10, showtimesEach = 12))

    // One thin tick per "pod": each restart builds a fresh cache over the same stores.
    (1 to ScrapeHealth.MaxConsecutiveDepthRejections).foreach { _ =>
      cacheOver(repository, ledger).recordCinemaScrape(Multikino, scrape(films = 10, showtimesEach = 1))
      storedShowtimes(repository, "Film 1") shouldBe 12
    }
    cacheOver(repository, ledger).recordCinemaScrape(Multikino, scrape(films = 10, showtimesEach = 1))
    storedShowtimes(repository, "Film 1") shouldBe 1
  }

  /** The durable ledger with its reads failing while `unreadable` — Mongo timing out. What a
   *  failed read answers is exactly the question: the production ledger used to answer Fresh,
   *  which is what this double answered before the ledger could say None. */
  private final class FlakyReadLedger extends ScrapeGuardLedger {
    private val stored = new InMemoryScrapeGuardLedger
    @volatile var unreadable = false
    def get(cinema: Cinema): Option[ScrapeGuardState] = if (unreadable) None else stored.get(cinema)
    def put(cinema: Cinema, state: ScrapeGuardState): Unit = stored.put(cinema, state)
    def storedCount(cinema: Cinema): Int = stored.get(cinema).fold(-1)(_.depthRejections)
  }

  // A read that fails is not a venue with no history. Taken as Fresh, the tick's rejection was
  // then WRITTEN as the count — 1 over a stored 2 — so a transient Mongo blip reset the grace
  // the count exists to carry, and a genuinely degraded venue served its stale board longer.
  "the guards' rejection count" should "survive a tick whose ledger read failed" in {
    val repository = splitRepository()
    val ledger     = new FlakyReadLedger
    cacheOver(repository, ledger).recordCinemaScrape(Multikino, scrape(films = 10, showtimesEach = 12))
    (1 to ScrapeHealth.MaxConsecutiveDepthRejections - 1).foreach { _ =>
      cacheOver(repository, ledger).recordCinemaScrape(Multikino, scrape(films = 10, showtimesEach = 1))
    }
    val counted = ledger.storedCount(Multikino)
    counted shouldBe ScrapeHealth.MaxConsecutiveDepthRejections - 1

    ledger.unreadable = true
    cacheOver(repository, ledger).recordCinemaScrape(Multikino, scrape(films = 10, showtimesEach = 1))
    withClue("an unreadable ledger must leave the stored count alone: ") { ledger.storedCount(Multikino) shouldBe counted }
    storedShowtimes(repository, "Film 1") shouldBe 12   // judged conservatively meanwhile
  }

  "the scrape runner" should "hand the cache each scraper's source key, chunked path included" in {
    // A chunked venue reaches the cache as a PreScrapedCinemaScraper built from the
    // chunked scraper, which must carry the key along or its rewire goes unseen.
    val ledger = new InMemoryScrapeGuardLedger
    val runner = new services.cinemas.common.CinemaScrapeRunner(
      cacheOver(splitRepository(), ledger), new services.events.InProcessEventBus(), deferredCinemas = Set.empty)
    val live = new services.cinemas.common.CinemaScraper {
      val cinema: Cinema               = Multikino
      def scrapeHosts: Set[String]     = Set.empty
      def fetch(): Seq[CinemaMovie]    = scrape(films = 2, showtimesEach = 2)
      override def sourceUrl: Option[String] = Some("https://www.filmweb.pl/cinema/2352")
    }

    runner.run(services.cinemas.common.PreScrapedCinemaScraper.of(live, () => live.fetch()))
    ledger.get(Multikino).flatMap(_.sourceKey) shouldBe NewSource
  }
}
