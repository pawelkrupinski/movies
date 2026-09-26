package services.cinemas.common

import models.{Cinema, CinemaMovie, Movie, Multikino}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.UptimeMonitor
import services.events.InProcessEventBus
import services.fallback.InMemoryFallbackStore
import services.movies.{CaffeineMovieCache, DepthGuardTime, InMemoryMovieRepository, InMemoryScrapeGuardLedger}
import services.movies.SingleCountryNormalizer.titleNormalizer

import scala.concurrent.duration.Duration

/**
 * A tick the FALLBACK served (Filmweb, Flicks) is still the primary venue's, merely covered
 * while its own source is down. Measured against the primary's full board, a sparse
 * fallback listing was discarded by the depth guard — the very ticks the fallback exists
 * to serve; read as a rewire instead, it pruned every primary film it does not list on
 * each outage. It must land additively: judged by neither guard, pruning nothing, and
 * leaving the primary as the venue's recorded source.
 */
class FallbackServedSourceSpec extends AnyFlatSpec with Matchers {

  private class Source(key: String, result: => Seq[CinemaMovie]) extends CinemaScraper {
    val cinema: Cinema                     = Multikino
    def scrapeHosts: Set[String]           = Set.empty
    def fetch(): Seq[CinemaMovie]          = result
    override def sourceKey: Option[String] = Some(key)
  }

  private val Primary  = "multikino.pl/cinemas/0011"
  private val Fallback = "filmweb.pl/cinema/-2180"
  private val listing  = Seq(CinemaMovie(Movie("Dune"), Multikino, None, None, None, Nil, Nil,
    DepthGuardTime.showtimes(1)))

  private def run(primary: CinemaScraper): Option[String] = {
    val ledger  = new InMemoryScrapeGuardLedger
    val cache   = new CaffeineMovieCache(new InMemoryMovieRepository(normalizer = titleNormalizer), new InProcessEventBus(),
      normalizer = titleNormalizer, scrapeGuardLedger = ledger, clock = DepthGuardTime.clock)
    val scraper = new SourceFallbackScraper(primary,
      fallback = () => Some(new Source(Fallback, listing)), fallbackName = "Filmweb", fallbackRef = () => Some("2180"),
      new UptimeMonitor(clock = DepthGuardTime.clock), new InMemoryFallbackStore, fallbackAfter = FallbackAfter.FailingFor(Duration.Zero))
    new CinemaScrapeRunner(cache, new InProcessEventBus(), deferredCinemas = Set.empty).run(scraper)
    ledger.get(Multikino).flatMap(_.sourceKey)
  }

  "a scrape the fallback served" should "never record the fallback as the venue's source" in {
    run(new Source(Primary, throw new RuntimeException("primary down"))) shouldBe None
  }

  "a scrape the primary served" should "be recorded under the primary's source" in {
    run(new Source(Primary, listing)) shouldBe Some(Primary)
  }

  // A fallback serves BECAUSE the primary is broken, and it is usually the thinner of the two.
  // Read as a rewire, its first tick landed as the new baseline with both guards off, and the
  // prune retired every primary film the fallback does not list — on every primary outage —
  // only for the primary's recovery to rewire them back. A fallback tick only ADDS.
  private def films(titles: String*)(showtimesEach: Int): Seq[CinemaMovie] = titles.map { t =>
    CinemaMovie(Movie(t, releaseYear = Some(2026)), Multikino, None, None, None, Nil, Nil,
      DepthGuardTime.showtimes(showtimesEach))
  }

  private final class Switchable(key: String) extends CinemaScraper {
    @volatile var listing: () => Seq[CinemaMovie] = () => Nil
    val cinema: Cinema                     = Multikino
    def scrapeHosts: Set[String]           = Set.empty
    def fetch(): Seq[CinemaMovie]          = listing()
    override def sourceKey: Option[String] = Some(key)
  }

  "a primary outage served from a thinner fallback" should "keep the primary's films, and recovery land normally" in {
    val repository = new InMemoryMovieRepository(screenings = Some(new services.movies.InMemoryScreeningsRepository),
      slots = Some(new services.movies.InMemorySlotsRepository), normalizer = titleNormalizer)
    val ledger  = new InMemoryScrapeGuardLedger
    val cache   = new CaffeineMovieCache(repository, new InProcessEventBus(), normalizer = titleNormalizer,
      scrapeGuardLedger = ledger, clock = DepthGuardTime.clock)
    val primary = new Switchable(Primary)
    val scraper = new SourceFallbackScraper(primary,
      fallback = () => Some(new Source(Fallback, films("Film 1", "Fallback Only")(2))), fallbackName = "Filmweb",
      fallbackRef = () => Some("2180"), new UptimeMonitor(clock = DepthGuardTime.clock), new InMemoryFallbackStore,
      baseBackoff = Duration.Zero, fallbackAfter = FallbackAfter.FailingFor(Duration.Zero)) // re-probe the primary on the very next tick
    val runner  = new CinemaScrapeRunner(cache, new InProcessEventBus(), deferredCinemas = Set.empty)
    def stored(title: String): Int = repository.findAll().find(_.title.contains(title))
      .map(_.record.data.values.map(_.showtimes.size).sum).getOrElse(0)
    val board = (1 to 10).map(i => s"Film $i")

    primary.listing = () => films(board*)(8)
    runner.run(scraper)
    stored("Film 5") shouldBe 8

    primary.listing = () => throw new RuntimeException("primary down")
    runner.run(scraper)
    stored("Fallback Only") shouldBe 2   // the fallback's listing lands…
    stored("Film 5") shouldBe 8          // …without pruning what only the primary lists
    ledger.get(Multikino).flatMap(_.sourceKey) shouldBe Some(Primary) // and is no new baseline

    primary.listing = () => films(board*)(8)
    runner.run(scraper)
    stored("Film 1") shouldBe 8
    stored("Film 5") shouldBe 8
    stored("Fallback Only") shouldBe 0   // the recovered primary's ordinary prune
  }
}
