package clients.teatr_ziemi_rybnickiej

import clients.tools.FakeHttpFetch
import models.KinoTeatrZiemiRybnickiej
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.TeatrZiemiRybnickiejClient

import java.time.LocalDateTime

// Fixtures recorded from
// https://www.teatrziemirybnickiej.pl/wydarzenia?type[]=film&month=default
// and each linked /wydarzenia/<slug>.html detail page. The cinema runs two
// film programmes: the in-house "kino nie tylko dla seniora" senior matinées
// and the "dkf ekran" club screenings. We keep only the latter — the senior
// tiles are dropped at the listing stage (so their detail pages are never
// fetched). The kept dkf detail pages format metadata as "Czas: / Gatunek - /
// Scenariusz i reżyseria: / Produkcja:" and carry no booking link.
class TeatrZiemiRybnickiejClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val http    = new FakeHttpFetch("teatr-ziemi-rybnickiej")
  private val client  = new TeatrZiemiRybnickiejClient(http)
  private val results = client.fetch()
  private val byTitle = results.map(cm => cm.movie.title -> cm).toMap

  // ── Programme filtering ─────────────────────────────────────────────────────

  "TeatrZiemiRybnickiejClient.fetch" should "return only the dkf screenings, not the senior matinées" in {
    results.size shouldBe 4
    results.map(_.movie.title).toSet shouldBe Set(
      "Młode matki",
      "Orły Republiki",
      "Werdykt",
      "Zawodowcy",
    )
  }

  it should "drop every 'kino nie tylko dla seniora' film" in {
    val titles = results.map(_.movie.title)
    titles should contain noneOf (
      "Przepis na morderstwo",
      "Dyrygent",
      "Miroirs III. Barka na oceanie",
      "Zawieście czerwone latarnie",
    )
  }

  it should "assign KinoTeatrZiemiRybnickiej to every entry" in {
    results.map(_.cinema).toSet shouldBe Set(KinoTeatrZiemiRybnickiej)
  }

  // ── Showtimes ───────────────────────────────────────────────────────────────

  it should "give every kept film exactly one showtime" in {
    results.foreach(cm => withClue(cm.movie.title)(cm.showtimes.size shouldBe 1))
  }

  it should "parse the dotted detail-page time into the right LocalDateTime" in {
    byTitle("Młode matki").showtimes.head.dateTime shouldBe LocalDateTime.of(2026, 6, 8, 19, 0)
    byTitle("Orły Republiki").showtimes.head.dateTime shouldBe LocalDateTime.of(2026, 6, 15, 19, 0)
    byTitle("Zawodowcy").showtimes.head.dateTime    shouldBe LocalDateTime.of(2026, 6, 29, 19, 0)
  }

  it should "leave bookingUrl None — dkf screenings only show 'Kup bilet' text, no link" in {
    results.foreach(cm => withClue(cm.movie.title)(cm.showtimes.head.bookingUrl shouldBe None))
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

  // ── Runtime (dkf "Czas:" block) ─────────────────────────────────────────────

  it should "parse runtime from the dkf 'Czas:' block" in {
    byTitle("Młode matki").movie.runtimeMinutes  shouldBe Some(105)
    byTitle("Orły Republiki").movie.runtimeMinutes shouldBe Some(127)
    byTitle("Werdykt").movie.runtimeMinutes      shouldBe Some(89)
    byTitle("Zawodowcy").movie.runtimeMinutes    shouldBe Some(98)
  }

  // ── Director (colon, dash, and "Scenariusz i reżyseria"/"Reżyseria" variants) ─

  it should "parse the director from every metadata variant" in {
    byTitle("Młode matki").director    shouldBe Seq("Jean-Pierre Dardenne", "Luc Dardenne") // "Scenariusz i reżyseria:"
    byTitle("Orły Republiki").director shouldBe Seq("Tarik Saleh")                           // "Scenariusz i reżyseria -"
    byTitle("Werdykt").director        shouldBe Seq("David Merriman", "Jim Sheridan")        // "Reżyseria -"
    byTitle("Zawodowcy").director      shouldBe Seq("Guy Ritchie")                           // "Scenariusz i reżyseria -"
  }

  // ── Production country + year ────────────────────────────────────────────────

  it should "parse production countries from both colon and comma-before-year forms" in {
    byTitle("Młode matki").movie.countries    shouldBe Seq("Belgia", "Francja")
    byTitle("Orły Republiki").movie.countries shouldBe Seq("Dania", "Francja", "Szwecja")
    byTitle("Zawodowcy").movie.countries      shouldBe Seq("USA", "Wielka Brytania")
  }

  it should "parse the release year, leaving it None when the block omits it" in {
    byTitle("Orły Republiki").movie.releaseYear shouldBe Some(2025)
    byTitle("Werdykt").movie.releaseYear        shouldBe Some(2025)
    byTitle("Młode matki").movie.releaseYear    shouldBe None
  }

  // ── Genre + cast ─────────────────────────────────────────────────────────────

  it should "parse genres, splitting multi-genre lines" in {
    byTitle("Młode matki").movie.genres    shouldBe Seq("dramat")
    byTitle("Orły Republiki").movie.genres shouldBe Seq("thriller polityczny")
    byTitle("Zawodowcy").movie.genres      shouldBe Seq("dramat", "akcja")
  }

  it should "leave cast empty — dkf blocks list no 'Obsada'" in {
    results.foreach(cm => withClue(cm.movie.title)(cm.cast shouldBe Seq.empty))
  }

  // ── Poster + film URL ────────────────────────────────────────────────────────

  it should "return an absolute poster URL" in {
    byTitle("Młode matki").posterUrl shouldBe
      Some("https://www.teatrziemirybnickiej.pl/files/events/modematki.jpg")
  }

  it should "return the detail-page URL as filmUrl" in {
    byTitle("Młode matki").filmUrl shouldBe
      Some("https://www.teatrziemirybnickiej.pl/wydarzenia/mlode-matki.html")
  }

  // ── parseDetail is programme-agnostic ────────────────────────────────────────
  //
  // The senior matinées are dropped at the listing stage, not by the parser:
  // parseDetail still reads a senior detail page's "Czas trwania: / Reż: /
  // Prod. / Gatunek: / Obsada:" block. This keeps the tolerant label parsing
  // exercised and documents that the drop is a programme policy, reversible by
  // editing parseListing alone.

  it should "still parse a senior detail page's metadata when handed one directly" in {
    val url   = "https://www.teatrziemirybnickiej.pl/wydarzenia/przepis-na-morderstwo.html"
    val movie = client.parseDetail(http.get(url), url).value

    movie.movie.title          shouldBe "Przepis na morderstwo"
    movie.movie.runtimeMinutes shouldBe Some(105)
    movie.director             shouldBe Seq("John Patton Ford")
    movie.movie.countries      shouldBe Seq("Francja", "Wielka Brytania", "USA")
    movie.movie.releaseYear    shouldBe Some(2026)
    movie.movie.genres         shouldBe Seq("czarna komedia")
    movie.cast                 shouldBe Seq("Glen Powell", "Margaret Qualley")
    movie.showtimes.head.bookingUrl shouldBe
      Some("https://bilety.teatrziemirybnickiej.pl/event/view/id/664576")
  }
}
