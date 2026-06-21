package clients.kino_studio

import clients.tools.FakeHttpFetch
import models.KinoStudio
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.KinoStudioClient

import java.time.{LocalDate, LocalDateTime}

/** Replays the recorded `mdk.opole.pl/kino-studio.html` page through the
 *  client — proving it finds titles, dates and times from the free-form CMS
 *  HTML. today is pinned to the fixture capture date (2026-06-21) so year
 *  inference is deterministic. */
class KinoStudioClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val today  = LocalDate.of(2026, 6, 21)
  private val movies = new KinoStudioClient(new FakeHttpFetch("kino-studio-opole"), KinoStudio, today).fetch()

  "KinoStudioClient" should "return a non-empty, single-cinema film list" in {
    movies should not be empty
    movies.map(_.cinema).toSet shouldBe Set(KinoStudio)
    all(movies.map(_.showtimes)) should not be empty
  }

  it should "parse the title and both showtime times for the current film" in {
    val film = movies.find(m => m.movie.title.contains("Lolita") || m.movie.title.contains("Lolitę")).value
    film.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 25, 18, 0))
    film.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 25, 20, 30))
    film.showtimes.flatMap(_.bookingUrl) shouldBe empty  // box-office only
  }

  it should "tag every film with KinoStudio cinema" in {
    all(movies.map(_.cinema)) shouldBe KinoStudio
  }

  it should "have non-empty titles" in {
    all(movies.map(_.movie.title)) should not be empty
  }

  it should "capture the genre" in {
    val film = movies.find(m => m.movie.title.contains("Lolita") || m.movie.title.contains("Lolitę")).value
    film.movie.genres should contain("dramat")
  }
}
