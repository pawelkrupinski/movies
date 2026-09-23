package services.cinemas.common

import models.{Cinema, CinemaMovie, KinoKoneckieCentrumKultury, Movie, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.LocalDateTime

class MultiListingScraperSpec extends AnyFlatSpec with Matchers {

  private val Konskie = KinoKoneckieCentrumKultury
  private def film(title: String, at: String, room: String) =
    CinemaMovie(Movie(title), Konskie, None, None, None, Nil, Nil, Seq(Showtime(LocalDateTime.parse(at), None, Some(room))))

  private def listing(host: String, films: => Seq[CinemaMovie], venue: Cinema = Konskie): CinemaScraper = new CinemaScraper {
    def cinema: Cinema = venue
    def fetch(): Seq[CinemaMovie] = films
    def scrapeHosts: Set[String] = Set(host)
    override def sourceUrl: Option[String] = Some(s"https://$host/")
  }

  "a venue listed once per hall" should "be one cinema whose film carries every hall's showtimes" in {
    val venue = new MultiListingScraper(Konskie, Seq(
      listing("kinowa.test", Seq(film("Lalka", "2026-10-20T18:00", "sala kinowa"))),
      listing("widowiskowa.test", Seq(film("Lalka", "2026-10-21T17:00", "sala widowiskowa"),
                                      film("Popiełuszko", "2026-10-20T17:00", "sala widowiskowa")))))
    val films = venue.fetch()
    films.map(_.movie.title) shouldBe Seq("Lalka", "Popiełuszko")
    films.head.showtimes.map(_.room.get) shouldBe Seq("sala kinowa", "sala widowiskowa")
    venue.scrapeHosts shouldBe Set("kinowa.test", "widowiskowa.test")
    venue.sourceUrl shouldBe Some("https://kinowa.test/")
  }

  it should "fail the fetch when one hall's listing fails, rather than prune that hall" in {
    val venue = new MultiListingScraper(Konskie, Seq(
      listing("kinowa.test", Seq(film("Lalka", "2026-10-20T18:00", "sala kinowa"))),
      listing("widowiskowa.test", throw new RuntimeException("HTTP 503"))))
    an [RuntimeException] should be thrownBy venue.fetch()
  }

  it should "refuse a listing scraped for another cinema" in {
    an [IllegalArgumentException] should be thrownBy
      new MultiListingScraper(Konskie, Seq(listing("x.test", Nil, venue = models.KinoMuza)))
  }
}
