package services.movies

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.LocalDateTime
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
      val times = (0 until showtimesEach).map(n =>
        Showtime(LocalDateTime.parse(f"2027-06-${8 + n / 12}%02dT${8 + n % 12}%02d:00"), None))
      CinemaMovie(movie = Movie(s"Film $i", releaseYear = Some(2026)), cinema = Multikino,
        posterUrl = None, filmUrl = None, synopsis = None, cast = Nil, director = Nil, showtimes = times)
    }

  private def splitRepository() = new InMemoryMovieRepository(
    screenings = Some(new InMemoryScreeningsRepository), slots = Some(new InMemorySlotsRepository))

  private def storedShowtimes(repository: InMemoryMovieRepository, title: String): Int =
    repository.findAll().find(_.title.contains(title))
      .map(_.record.data.values.map(_.showtimes.size).sum).getOrElse(0)

  private def cacheOver(repository: InMemoryMovieRepository, ledger: ScrapeGuardLedger) =
    new CaffeineMovieCache(repository, normalizer = titleNormalizer, scrapeGuardLedger = ledger)

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
    ledger.get(Multikino).sourceKey shouldBe None

    cache.recordCinemaScrape(Multikino, scrape(films = 10, showtimesEach = 1), sourceKey = NewSource)
    storedShowtimes(repository, "Film 1") shouldBe 8 // rejected: an unknown past is no rewire
    ledger.get(Multikino).sourceKey shouldBe None    // and a discarded tick records nothing

    cache.recordCinemaScrape(Multikino, scrape(films = 10, showtimesEach = 7), sourceKey = NewSource)
    storedShowtimes(repository, "Film 1") shouldBe 7
    ledger.get(Multikino).sourceKey shouldBe NewSource
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
    ledger.get(Multikino).sourceKey shouldBe NewSource
  }
}
