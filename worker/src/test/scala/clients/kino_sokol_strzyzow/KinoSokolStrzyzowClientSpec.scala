package clients.kino_sokol_strzyzow

import org.scalatest.OptionValues
import clients.tools.FakeHttpFetch
import models.KinoSokolStrzyzow
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas.pl.KinoSokolStrzyzowClient

import java.time.LocalDate

/** Replays the recorded `dksokol.eu/kino/` schedule board (captured
 *  2026-09-23) through the client — a fully server-rendered page, no AJAX
 *  round-trip needed. */
class KinoSokolStrzyzowClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val today  = LocalDate.of(2026, 9, 23)
  private val movies = new KinoSokolStrzyzowClient(new FakeHttpFetch("kino-sokol-strzyzow"), today = today).fetch()

  "KinoSokolStrzyzowClient" should "return a non-empty, single-cinema film list" in {
    movies should not be empty
    movies.map(_.cinema).toSet shouldBe Set(KinoSokolStrzyzow)
    all(movies.map(_.showtimes)) should not be empty
  }

  it should "infer the year off `today` for a yearless day/month showtime" in {
    val film = movies.find(_.movie.title == "Księga pustyni").value
    film.showtimes.map(_.dateTime) should contain allOf (
      java.time.LocalDateTime.of(2026, 9, 25, 16, 0), java.time.LocalDateTime.of(2026, 9, 27, 16, 0)
    )
  }

  it should "read the runtime off the free-text meta badges, dropping the age badge" in {
    val film = movies.find(_.movie.title == "Księga pustyni").value
    film.movie.runtimeMinutes.value shouldBe 92
    film.movie.genres should contain ("Familijny")
    film.movie.genres.exists(_.toLowerCase.contains("od 10 lat")) shouldBe false
  }

  it should "carry the venue's own seans page as the film URL and poster, with no booking link (sale not live)" in {
    val film = movies.find(_.movie.title == "Księga pustyni").value
    film.filmUrl.value shouldBe "https://dksokol.eu/seans/ksiega-pustyni/"
    film.posterUrl.value should include("dksokol.eu/wp-content/uploads")
    all(film.showtimes.map(_.bookingUrl)) shouldBe None
  }

  it should "list every film screening in the verified window" in {
    movies.map(_.movie.title) should contain allOf (
      "Księga pustyni", "100 Dni: Misja Zeus", "Pucio kocha zwierzaki",
      "Dzień dziecka księdza Jana Kaczkowskiego", "Toy Story 5", "Mistyczka"
    )
  }
}
