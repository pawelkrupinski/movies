package modules.wiring

import models.{CinemaMovie, KinoMikro, Movie, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.common.CinemaScraper
import tools.{FixtureTestWiring, TestWiring}

import java.time.LocalDateTime

/** The scrape wrappers judge a listing's thinness ("nothing in the next 72h") on
 *  the WIRING's clock, so the fixture harness — whose clock is pinned to the
 *  corpus day (`TestWiring.FixedInstant`) — sees its corpus as current. On the
 *  system clock every replayed June screening lies in the past, and every scrape
 *  read thin. */
class ScrapeWiringClockSpec extends AnyFlatSpec with Matchers {

  private val nextDay = Seq(CinemaMovie(
    movie = Movie("Film"), cinema = KinoMikro, posterUrl = None, filmUrl = None, synopsis = None,
    cast = Seq.empty, director = Seq.empty,
    // The day after the wiring's pinned instant, whatever that is.
    showtimes = Seq(Showtime(
      LocalDateTime.ofInstant(TestWiring.FixedInstant, java.time.ZoneId.of("Europe/Warsaw")).plusDays(1),
      Some("https://book")))))

  private val scraper: CinemaScraper = new CinemaScraper {
    val cinema = KinoMikro
    def scrapeHosts: Set[String] = Set("example.test")
    def fetch(): Seq[CinemaMovie] = nextDay
  }

  for (eligible <- Seq(false, true))
    s"recordingScraper (fallback-eligible = $eligible)" should "judge thinness on the wiring's clock" in {
      val wiring = new FixtureTestWiring("08-06-2026")
      wiring.recordingScraper(scraper, eligible).fetch()
      val bucket = wiring.uptimeMonitor.history(KinoMikro.displayName).head
      bucket.status shouldBe "green"
      bucket.thin shouldBe false
    }
}
