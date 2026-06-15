package clients.kino_fenomen

import clients.tools.FakeHttpFetch
import models.KinoFenomen
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.KinoFenomenClient

import java.time.LocalDateTime

/** Replays the recorded `iframe639.biletyna.pl/?display=events` page
 *  (07-06-2026 capture) through the client. The listing is server-rendered and
 *  covers the next few weeks on a single page. */
class KinoFenomenClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val http   = new FakeHttpFetch("kino-fenomen")
  private val client = new KinoFenomenClient(http, KinoFenomen)

  "KinoFenomenClient" should "return a non-empty film list" in {
    val movies = client.fetch()
    movies should not be empty
  }

  it should "tag every film with KinoFenomen" in {
    val movies = client.fetch()
    movies.map(_.cinema).toSet shouldBe Set(KinoFenomen)
  }

  it should "give every film at least one showtime" in {
    val movies = client.fetch()
    all(movies.map(_.showtimes)) should not be empty
  }

  it should "pin a concrete screening: Orły Republiki on 2026-06-07 at 16:00" in {
    // Fixture: 07.06.2026 16:00 — event id 671638
    val movies = client.fetch()
    val orly   = movies.find(_.movie.title == "Orły Republiki").value
    orly.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 7, 16, 0))
  }

  it should "attach a booking URL to each showtime" in {
    val movies = client.fetch()
    movies.flatMap(_.showtimes).flatMap(_.bookingUrl).foreach { u =>
      u should startWith("https://iframe639.biletyna.pl/event/view/id/")
    }
  }

  it should "strip the format tag from titles" in {
    // Raw: "Niesamowite przygody skarpetek 3. Ale kosmos! (2D/oryginalny)"
    val movies = client.fetch()
    movies.map(_.movie.title) should contain("Niesamowite przygody skarpetek 3. Ale kosmos!")
  }

  // ── Director + production year from the artist-link metadata ───────────────

  it should "extract director(s) from the '| reżyseria: … |' segment" in {
    KinoFenomenClient.parseDirectors(
      "„Milcząca przyjaciółka” | reżyseria: Ildikó Enyedi | Francja, Niemcy, Węgry 2025 (2D/napisy)"
    ) shouldBe Seq("Ildikó Enyedi")
    KinoFenomenClient.parseDirectors("Film | reżyseria: A. Kowalski, B. Nowak | Polska 2024") shouldBe
      Seq("A. Kowalski", "B. Nowak")
    KinoFenomenClient.parseDirectors("Orły Republiki (2D/napisy)") shouldBe Seq.empty
  }

  it should "extract the production year from the metadata segment or a (YYYY) paren" in {
    // Trailing "Country YYYY" metadata, ignoring the "(2D/napisy)" format tag.
    KinoFenomenClient.parseYear(
      "„Milcząca przyjaciółka” | reżyseria: Ildikó Enyedi | Francja, Niemcy, Węgry 2025 (2D/napisy)"
    ) shouldBe Some(2025)
    // Year carried in a paren in the title itself.
    KinoFenomenClient.parseYear("Mikey i Nicky (1976) (2D/oryginalny)") shouldBe Some(1976)
    // A bare title with no pipe metadata and no paren-year yields None.
    KinoFenomenClient.parseYear("Orły Republiki (2D/napisy)") shouldBe None
  }

  it should "surface the director and year on the fetched film" in {
    val movies   = client.fetch()
    val milczaca = movies.find(_.movie.title.contains("Milcząca przyjaciółka")).value
    milczaca.director            shouldBe Seq("Ildikó Enyedi")
    milczaca.movie.releaseYear   shouldBe Some(2025)
    // A film whose listing carries no metadata keeps both empty.
    val orly = movies.find(_.movie.title == "Orły Republiki").value
    orly.director          shouldBe empty
    orly.movie.releaseYear shouldBe None
  }
}
