package clients.filmweb_showtimes

import clients.tools.FakeHttpFetch
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.pl.FilmwebProgrammes
import services.identity.ScreeningDays
import services.identity.agreement.Showing

import java.time.LocalDate

/** Replays Filmweb programmes recorded 2026-10-05 (`test/resources/fixtures/filmweb-programmes`, through
 *  `/api/v1/showtimes/cinema/<id>?date=2026-10-04` and `/showtimes/city/<id>?date=2026-10-04` — a day with no seances,
 *  so every film comes back with its days — and `/api/v1/cities`). */
class FilmwebProgrammesSpec extends AnyFlatSpec with Matchers {

  private val today      = LocalDate.of(2026, 10, 5)
  private val programmes = new FilmwebProgrammes(new FakeHttpFetch("filmweb-programmes", strict = true),
    Map("Kozienicki Dom Kultury" -> 1913).get, FilmwebProgrammes.townsOf, () => today)
  private def days(ds: String*) = ScreeningDays.of(ds.map(LocalDate.parse))

  "A venue Filmweb lists" should "be answered by its own programme: each film with every day it screens it" in {
    // Kozienice screens Broken Voices (Filmweb's "Dyrygent", 10085635) once, on its Polish premiere day
    programmes.of("Kozienicki Dom Kultury") shouldBe Seq(
      Showing("10057628", days("2026-10-05", "2026-10-07")), Showing("10085635", days("2026-10-07")), Showing("10088463", days("2026-10-05")))
  }

  "A venue Filmweb does not list" should "be answered by its town's programme" in {
    // Kino CK Lublin: no Filmweb cinema; Lublin's programme screens Kawalski's "Lalka" (10057628), never Has's 1174
    val lublin = programmes.of("Kino CK Lublin")
    lublin.size shouldBe 47
    lublin.find(_.film == "10057628").map(_.days) shouldBe Some(days("2026-10-05", "2026-10-06", "2026-10-07", "2026-10-08", "2026-10-10", "2026-10-11"))
    lublin.map(_.film) should not contain "1174"
  }

  "A venue no city lists" should "have no programme" in {
    programmes.of("no such venue") shouldBe empty
  }

  "Programmes" should "merge into one, each film once with every day" in {
    FilmwebProgrammes.merged(Seq(Showing("2", days("2026-10-05")), Showing("1", days("2026-10-06")), Showing("2", days("2026-10-07")))) shouldBe
      Seq(Showing("1", days("2026-10-06")), Showing("2", days("2026-10-05", "2026-10-07")))
  }

  it should "throw on a body that is not JSON: a failed read is asked again, never an empty programme" in {
    an[Exception] should be thrownBy FilmwebProgrammes.parseProgramme("<html>blocked</html>")
  }
}
