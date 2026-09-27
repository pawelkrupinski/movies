package clients.kino_metalowiec_nowa_deba

import clients.tools.{FailingHttpFetch, FakeHttpFetch}
import models.KinoMetalowiecNowaDeba
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.pl.KinoMetalowiecNowaDebaClient
import tools.HttpStatusException

import java.time.{LocalDate, LocalDateTime}

/** Replays SOK Nowa Dęba's `/repertuar-kina-metalowiec/` post as captured on
 *  2026-09-27: a sidebar schedule for 26–30 Sep (two films, times) and eleven
 *  film blocks, most of them October announcements with dates but no times.
 *  Fixture directory: test/resources/fixtures/kino-metalowiec-nowa-deba/ */
class KinoMetalowiecNowaDebaClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val today  = LocalDate.of(2026, 9, 27)
  private val movies = new KinoMetalowiecNowaDebaClient(new FakeHttpFetch("kino-metalowiec-nowa-deba"), KinoMetalowiecNowaDeba, today).fetch()

  "KinoMetalowiecNowaDebaClient" should "emit exactly the films the timed schedule lists" in {
    movies.map(_.movie.title) shouldBe Seq("Asterix i Obelix. Misja Kleopatra", "Mistyczka")
    movies.map(_.cinema).toSet shouldBe Set(KinoMetalowiecNowaDeba)
  }

  it should "anchor each yearless day header's times to a date" in {
    val asterix = movies.find(_.movie.title == "Asterix i Obelix. Misja Kleopatra").value
    asterix.showtimes.map(_.dateTime) shouldBe Seq(
      LocalDateTime.of(2026, 9, 26, 15, 30), LocalDateTime.of(2026, 9, 27, 15, 30),
      LocalDateTime.of(2026, 9, 29, 16, 30), LocalDateTime.of(2026, 9, 30, 16, 30)
    )
    all(asterix.showtimes.map(_.bookingUrl)) shouldBe None
  }

  it should "enrich a film from its block, joined across punctuation and casing" in {
    val asterix = movies.find(_.movie.title == "Asterix i Obelix. Misja Kleopatra").value
    asterix.movie.runtimeMinutes.value shouldBe 107
    asterix.movie.genres shouldBe Seq("przygodowy", "familijny", "komedia")
    asterix.ageRating.value shouldBe "9+"
    asterix.synopsis.value should startWith("Piramidalnie śmieszna komedia")
    asterix.posterUrl.value should include("soknowadeba.pl/wp-content/uploads/")
    asterix.movie.releaseYear shouldBe None
  }

  it should "take the production year from the film's own Filmweb link, not a wrapping one" in {
    val mistyczka = movies.find(_.movie.title == "Mistyczka").value
    mistyczka.showtimes.map(_.dateTime) shouldBe Seq(
      LocalDateTime.of(2026, 9, 26, 18, 0), LocalDateTime.of(2026, 9, 27, 18, 0),
      LocalDateTime.of(2026, 9, 29, 19, 0), LocalDateTime.of(2026, 9, 30, 19, 0)
    )
    mistyczka.movie.releaseYear.value shouldBe 2026
    mistyczka.movie.runtimeMinutes.value shouldBe 90
    mistyczka.movie.genres shouldBe Seq("dramat", "obyczajowy")
    mistyczka.ageRating.value shouldBe "12+"
    mistyczka.posterUrl.value shouldBe "https://www.soknowadeba.pl/wp-content/uploads/2024/02/mistyczka-728x1024.jpg"
    mistyczka.trailerUrl.value shouldBe "https://www.youtube.com/watch?v=RG_hCZBwcGU"
  }

  it should "propagate a fetch failure instead of reporting an empty (white) scrape" in {
    a[HttpStatusException] should be thrownBy
      new KinoMetalowiecNowaDebaClient(new FailingHttpFetch(503), KinoMetalowiecNowaDeba, today).fetch()
  }
}
