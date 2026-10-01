package services.movies

import models.{CinemaMovie, Helios, Movie, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.SingleCountryNormalizer.titleNormalizer

import java.time.LocalDateTime

class ScrapeListingSpec extends AnyFlatSpec with Matchers {

  private def listing(title: String, showtimes: Seq[Showtime], year: Option[Int] = None): CinemaMovie =
    CinemaMovie(Movie(title = title, releaseYear = year), cinema = Helios, posterUrl = None, filmUrl = None,
      synopsis = None, cast = Nil, director = Nil, showtimes = showtimes)

  private def at(hour: Int, format: List[String] = Nil): Showtime =
    Showtime(LocalDateTime.of(2026, 9, 7, hour, 0), bookingUrl = None, format = format)

  private def prepare(movies: CinemaMovie*) =
    ScrapeListing.prepare(Helios, movies, titleNormalizer, ScreeningTokens.forDefaultCountry())

  "ScrapeListing.prepare" should "fold a venue's several rows for one film onto one slot, keeping every showing" in {
    // Two event pages for the same film, each listing one session, one of them twice
    // under a different booking link.
    val p = prepare(
      listing("Ojczyzna", Seq(at(18))),
      listing("Ojczyzna", Seq(at(20), at(20)), year = Some(2024)))
    p.movies.map(p.cleaned) shouldBe Seq("Ojczyzna")
    p.movies.head.showtimes.map(_.dateTime.getHour) shouldBe Seq(18, 20)
  }

  it should "strip a format tag off the title into the showings' badges, through the shared vocabulary" in {
    val p = prepare(listing("Zwierzogród 2 (Napisy PL)", Seq(at(18))))
    p.movies.map(p.cleaned) shouldBe Seq("Zwierzogród 2")
    p.movies.head.showtimes.head.format should contain ("NAP")
  }

  it should "be a function of the listing, not of the order the scraper emitted it in" in {
    val a = listing("Diuna", Seq(at(18))); val b = listing("Diuna", Seq(at(21))); val c = listing("Guru", Seq(at(19)))
    prepare(a, b, c).movies shouldBe prepare(c, b, a).movies
  }

  // A cleaning runs every title rule of the venue; an identity projection prepares every venue's
  // listings and asks each film for its cleaned title again and again.
  it should "clean each of a venue's titles once, however often its rows are asked for it" in {
    val cleanings  = new java.util.concurrent.atomic.AtomicInteger(0)
    val counting   = new TitleNormalizer(titleNormalizer.rules) {
      override def cinemaClean(cinemaId: String, raw: String): String = { cleanings.incrementAndGet(); super.cinemaClean(cinemaId, raw) }
    }
    val p = ScrapeListing.prepare(Helios, Seq(listing("Ojczyzna", Seq(at(18))), listing("Ojczyzna", Seq(at(20))),
      listing("Diuna", Seq(at(19)))), counting, ScreeningTokens.forDefaultCountry())
    p.movies.map(p.cleaned).sorted shouldBe Seq("Diuna", "Ojczyzna")
    cleanings.get shouldBe 2
  }
}
