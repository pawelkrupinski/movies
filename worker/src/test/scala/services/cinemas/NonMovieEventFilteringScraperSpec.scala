package services.cinemas

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class NonMovieEventFilteringScraperSpec extends AnyFlatSpec with Matchers {

  private def cm(title: String): CinemaMovie = CinemaMovie(
    movie = Movie(title = title), cinema = KinoMuza,
    posterUrl = None, filmUrl = None, synopsis = None, cast = Nil, director = Nil, showtimes = Nil
  )

  private class FakeScraper(out: Seq[CinemaMovie]) extends CinemaScraper {
    def cinema: Cinema = KinoMuza
    def fetch(): Seq[CinemaMovie] = out
    def scrapeHosts: Set[String] = Set("example.test")
    override def maxFetchAttempts: Int = 7
    override def chain: Boolean = true
    override def sourceUrl: Option[String] = Some("https://example.test")
  }

  "NonMovieEventFilteringScraper" should "strip live events from fetch() but keep films and broadcasts" in {
    val scraper = new NonMovieEventFilteringScraper(new FakeScraper(Seq(
      cm("Dune: Part Two"),
      cm("Koncert Joscho Stephan Trio"),                 // event → dropped
      cm("Kabaret Ani Mru Mru - Mniej więcej"),          // event → dropped
      cm("André Rieu W KINIE — letni koncert"),          // broadcast → kept
      cm("Avatar")
    )))

    scraper.fetch().map(_.movie.title) shouldBe Seq("Dune: Part Two", "André Rieu W KINIE — letni koncert", "Avatar")
  }

  it should "collapse an all-events fetch to empty (recordCinemaScrape then keeps existing slots)" in {
    val scraper = new NonMovieEventFilteringScraper(new FakeScraper(Seq(
      cm("Edyta Geppert - recital"), cm("Piotr Bałtroczyk Stand-up")
    )))
    scraper.fetch() shouldBe empty
  }

  it should "delegate cinema/hosts/knobs to the inner scraper unchanged" in {
    val inner   = new FakeScraper(Nil)
    val scraper = new NonMovieEventFilteringScraper(inner)
    scraper.cinema shouldBe inner.cinema
    scraper.scrapeHosts shouldBe inner.scrapeHosts
    scraper.maxFetchAttempts shouldBe 7
    scraper.chain shouldBe true
    scraper.sourceUrl shouldBe Some("https://example.test")
  }
}
