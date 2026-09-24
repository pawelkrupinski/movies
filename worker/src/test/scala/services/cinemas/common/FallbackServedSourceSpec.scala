package services.cinemas.common

import models.{Cinema, CinemaMovie, Movie, Multikino, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.UptimeMonitor
import services.events.InProcessEventBus
import services.fallback.InMemoryFallbackStore
import services.movies.{CaffeineMovieCache, InMemoryMovieRepository, InMemoryScrapeGuardLedger}
import services.movies.SingleCountryNormalizer.titleNormalizer

import java.time.LocalDateTime
import scala.concurrent.duration.Duration

/**
 * The scrape guards judge a listing against the rows of the source that last served
 * the venue, keyed by `sourceKey`. A tick the FALLBACK served (Filmweb, Flicks) is the
 * fallback's listing, so it has to land under the fallback's key: recorded under the
 * primary's, a sparse Filmweb board was measured against the primary's full one — the
 * depth guard discarding the very ticks the fallback exists to serve — and the primary's
 * return after an outage never read as the change of source it is.
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
    Seq(Showtime(LocalDateTime.of(2027, 1, 1, 18, 0), None))))

  private def run(primary: CinemaScraper): Option[String] = {
    val ledger  = new InMemoryScrapeGuardLedger
    val cache   = new CaffeineMovieCache(new InMemoryMovieRepository(), new InProcessEventBus(),
      normalizer = titleNormalizer, scrapeGuardLedger = ledger)
    val scraper = new SourceFallbackScraper(primary,
      fallback = () => Some(new Source(Fallback, listing)), fallbackName = "Filmweb", fallbackRef = () => Some("2180"),
      new UptimeMonitor(), new InMemoryFallbackStore, fallbackAfter = Duration.Zero)
    new CinemaScrapeRunner(cache, new InProcessEventBus(), deferredCinemas = Set.empty).run(scraper)
    ledger.get(Multikino).sourceKey
  }

  "a scrape the fallback served" should "be recorded under the fallback's source, not the primary's" in {
    run(new Source(Primary, throw new RuntimeException("primary down"))) shouldBe Some(Fallback)
  }

  "a scrape the primary served" should "be recorded under the primary's source" in {
    run(new Source(Primary, listing)) shouldBe Some(Primary)
  }
}
