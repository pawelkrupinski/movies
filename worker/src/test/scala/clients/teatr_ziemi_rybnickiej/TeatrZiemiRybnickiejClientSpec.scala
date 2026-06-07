package clients.teatr_ziemi_rybnickiej

import clients.tools.FakeHttpFetch
import models.KinoTeatrZiemiRybnickiej
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.TeatrZiemiRybnickiejClient

import java.time.LocalDateTime

// Fixtures recorded from
// https://www.teatrziemirybnickiej.pl/wydarzenia?type[]=film&month=default
// and each linked /wydarzenia/<slug>.html detail page. The cinema runs two
// programmes whose detail pages format their metadata differently — the
// in-house "kino nie tylko dla seniora" screenings (labelled "Czas trwania: /
// Reż: / Prod. / Gatunek: / Obsada:", with a booking link) and the "dkf ekran"
// screenings ("Czas: / Gatunek - / Scenariusz i reżyseria: / Produkcja:", no
// booking link). The spec covers both shapes.
class TeatrZiemiRybnickiejClientSpec extends AnyFlatSpec with Matchers {

  private val client  = new TeatrZiemiRybnickiejClient(new FakeHttpFetch("teatr-ziemi-rybnickiej"))
  private val results = client.fetch()
  private val byTitle = results.map(cm => cm.movie.title -> cm).toMap

  // ── Totals ────────────────────────────────────────────────────────────────

  "TeatrZiemiRybnickiejClient.fetch" should "return all 8 films from the listing" in {
    results.size shouldBe 8
  }

  it should "assign KinoTeatrZiemiRybnickiej to every entry" in {
    results.map(_.cinema).toSet shouldBe Set(KinoTeatrZiemiRybnickiej)
  }

  it should "return exactly the expected set of titles" in {
    results.map(_.movie.title).toSet shouldBe Set(
      "Przepis na morderstwo",
      "Młode matki",
      "Dyrygent",
      "Orły Republiki",
      "Miroirs III. Barka na oceanie",
      "Werdykt",
      "Zawieście czerwone latarnie",
      "Zawodowcy",
    )
  }

  // ── Showtimes ───────────────────────────────────────────────────────────────

  it should "give every film exactly one showtime" in {
    results.foreach(cm => withClue(cm.movie.title)(cm.showtimes.size shouldBe 1))
  }

  it should "parse the dotted detail-page time into the right LocalDateTime" in {
    byTitle("Przepis na morderstwo").showtimes.head.dateTime shouldBe LocalDateTime.of(2026, 6, 8, 15, 30)
    byTitle("Dyrygent").showtimes.head.dateTime             shouldBe LocalDateTime.of(2026, 6, 15, 15, 30)
    byTitle("Zawodowcy").showtimes.head.dateTime            shouldBe LocalDateTime.of(2026, 6, 29, 19, 0)
  }

  it should "attach the booking link for screenings that have one" in {
    byTitle("Przepis na morderstwo").showtimes.head.bookingUrl shouldBe
      Some("https://bilety.teatrziemirybnickiej.pl/event/view/id/664576")
    byTitle("Zawieście czerwone latarnie").showtimes.head.bookingUrl shouldBe
      Some("https://bilety.teatrziemirybnickiej.pl/event/view/id/664584")
  }

  it should "leave bookingUrl None for dkf screenings that only show 'Kup bilet' text" in {
    byTitle("Młode matki").showtimes.head.bookingUrl shouldBe None
    byTitle("Zawodowcy").showtimes.head.bookingUrl   shouldBe None
  }

  // ── Synopsis ────────────────────────────────────────────────────────────────

  it should "extract a non-empty synopsis for every film, never a price line" in {
    results.foreach { cm =>
      withClue(cm.movie.title) {
        cm.synopsis should not be empty
        cm.synopsis.get.length should be > 40
        cm.synopsis.get should not include "zł"
      }
    }
  }

  it should "extract the exact synopsis for Młode matki" in {
    byTitle("Młode matki").synopsis shouldBe Some(
      "Jessica, Perla, Julie, Ariane i Naïma zamieszkują razem dom samotnej matki. " +
        "Mają nadzieję na lepsze jutro dla siebie i swoich dzieci."
    )
  }

  // ── Runtime (both metadata shapes) ──────────────────────────────────────────

  it should "parse runtime from the senior 'Czas trwania:' block" in {
    byTitle("Przepis na morderstwo").movie.runtimeMinutes shouldBe Some(105)
    byTitle("Dyrygent").movie.runtimeMinutes              shouldBe Some(106)
    byTitle("Miroirs III. Barka na oceanie").movie.runtimeMinutes shouldBe Some(86)
    byTitle("Zawieście czerwone latarnie").movie.runtimeMinutes   shouldBe Some(125)
  }

  it should "parse runtime from the dkf 'Czas:' block" in {
    byTitle("Młode matki").movie.runtimeMinutes  shouldBe Some(105)
    byTitle("Orły Republiki").movie.runtimeMinutes shouldBe Some(127)
    byTitle("Werdykt").movie.runtimeMinutes      shouldBe Some(89)
    byTitle("Zawodowcy").movie.runtimeMinutes    shouldBe Some(98)
  }

  // ── Director (colon, dash, and "Scenariusz i reżyseria" variants) ───────────

  it should "parse the director from every metadata variant" in {
    byTitle("Przepis na morderstwo").director shouldBe Seq("John Patton Ford")          // "Reż:"
    byTitle("Młode matki").director           shouldBe Seq("Jean-Pierre Dardenne", "Luc Dardenne") // "Scenariusz i reżyseria:"
    byTitle("Orły Republiki").director        shouldBe Seq("Tarik Saleh")               // "Scenariusz i reżyseria -"
    byTitle("Werdykt").director               shouldBe Seq("David Merriman", "Jim Sheridan") // "Reżyseria -"
  }

  // ── Production country + year ────────────────────────────────────────────────

  it should "parse production countries from both colon and comma-before-year forms" in {
    byTitle("Przepis na morderstwo").movie.countries shouldBe Seq("Francja", "Wielka Brytania", "USA")
    byTitle("Młode matki").movie.countries           shouldBe Seq("Belgia", "Francja")
    byTitle("Orły Republiki").movie.countries        shouldBe Seq("Dania", "Francja", "Szwecja")
    byTitle("Zawodowcy").movie.countries             shouldBe Seq("USA", "Wielka Brytania")
  }

  it should "parse the release year, keeping the original year of a restoration" in {
    byTitle("Przepis na morderstwo").movie.releaseYear shouldBe Some(2026)
    byTitle("Dyrygent").movie.releaseYear              shouldBe Some(2025)
    byTitle("Zawieście czerwone latarnie").movie.releaseYear shouldBe Some(1991) // "1991/2026"
  }

  it should "leave releaseYear None when the dkf block omits it" in {
    byTitle("Młode matki").movie.releaseYear shouldBe None
  }

  // ── Genre + cast ─────────────────────────────────────────────────────────────

  it should "parse genres, splitting multi-genre lines" in {
    byTitle("Przepis na morderstwo").movie.genres shouldBe Seq("czarna komedia")
    byTitle("Orły Republiki").movie.genres        shouldBe Seq("thriller polityczny")
    byTitle("Zawodowcy").movie.genres             shouldBe Seq("dramat", "akcja")
  }

  it should "parse cast only from the senior block that lists it" in {
    byTitle("Przepis na morderstwo").cast shouldBe Seq("Glen Powell", "Margaret Qualley")
    byTitle("Dyrygent").cast              shouldBe Seq("Kateřina Falbrová", "Juraj Loj")
    byTitle("Młode matki").cast           shouldBe Seq.empty // dkf block has no "Obsada"
  }

  // ── Poster + film URL ────────────────────────────────────────────────────────

  it should "return an absolute poster URL" in {
    byTitle("Przepis na morderstwo").posterUrl shouldBe
      Some("https://www.teatrziemirybnickiej.pl/files/events/przepisnamorderstwo.jpg")
  }

  it should "return the detail-page URL as filmUrl" in {
    byTitle("Młode matki").filmUrl shouldBe
      Some("https://www.teatrziemirybnickiej.pl/wydarzenia/mlode-matki.html")
  }
}
