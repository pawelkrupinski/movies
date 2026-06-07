package clients.alternatywy

import clients.tools.FakeHttpFetch
import models.KinoAlternatywy
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.AlternatywyClient

import java.time.{LocalDate, LocalDateTime}

/**
 * Replays a recorded capture of Kino Alternatywy's own WordPress repertoire page
 * (2026-06, base64 image placeholders stripped). Asserts the auditorium ("sala:")
 * screenings are assembled with the right title/date/time/room, that the page's
 * non-auditorium rows (Galeria/inne) and the duplicate slider tiles are dropped,
 * and that the title cleanup handles the page's varied alt-text quoting.
 */
class AlternatywyClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val today  = LocalDate.of(2026, 6, 7)
  private val client = new AlternatywyClient(new FakeHttpFetch("alternatywy"), today = today)
  private lazy val movies = client.fetch()

  "AlternatywyClient" should "assemble the sala: auditorium screenings from the repertoire page" in {
    movies should not be empty
    movies.map(_.cinema).toSet shouldBe Set(KinoAlternatywy)
    all(movies.map(_.showtimes)) should not be empty
    all(movies.map(_.movie.title.trim)) should not be empty
    // Every showtime falls in the captured June 2026 window, with the year inferred.
    movies.flatMap(_.showtimes).map(_.dateTime.toLocalDate).foreach { d =>
      d.getYear shouldBe 2026
      d.getMonthValue shouldBe 6
    }
  }

  it should "keep exactly the seven sala: titles and drop Galeria/inne rows" in {
    movies.map(_.movie.title).toSet shouldBe Set(
      "Zemsta", "Sztuka Podróżowania", "Wielkie Artystki",
      "Sztuka Podróżowania – edycja specjalna", "Bravo lata 90!",
      "Głosem malowane", "Flying Lion Adam Święs Trio"
    )
    // Galeria ("Szukając…") and inne ("Wycinki", "…na dniach Ursynowa") are not screenings.
    val titles = movies.map(_.movie.title).mkString(" | ")
    titles should not include "Wycinki"
    titles should not include "Szukając"
  }

  it should "pin a concrete screening with its room, poster and detail link" in {
    val zemsta = movies.find(_.movie.title == "Zemsta").value
    val st     = zemsta.showtimes.find(_.dateTime == LocalDateTime.of(2026, 6, 7, 18, 0)).value
    st.room.value shouldBe "Szekspir"
    st.bookingUrl shouldBe None
    zemsta.filmUrl.value should startWith("https://alternatywy.art/")
    zemsta.posterUrl.value should startWith("https://alternatywy.art/")
  }

  "cleanTitle" should "strip the Okładka prefix and typographic quotes" in {
    AlternatywyClient.cleanTitle("Okładka „Zemsta”") shouldBe "Zemsta"
    AlternatywyClient.cleanTitle("Okładka Wielkie Artystki") shouldBe "Wielkie Artystki"
    AlternatywyClient.cleanTitle("Okładka \"Bravo lata 90!\"") shouldBe "Bravo lata 90!"
    AlternatywyClient.cleanTitle("Okładka „Flying Lion”  Adam Święs Trio") shouldBe "Flying Lion Adam Święs Trio"
  }
}
