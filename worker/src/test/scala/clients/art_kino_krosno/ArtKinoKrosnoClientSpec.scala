package clients.art_kino_krosno

import models.KinoArtKino
import org.scalatest.OptionValues
import clients.tools.FakeHttpFetch
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas.pl.ArtKinoKrosnoClient

import java.time.{LocalDate, LocalDateTime}

/** Replays the recorded `artkino.rckp.krosno.pl/strona-375-repertuar.html`
 *  article for artKino (Krosno) through the client. `today` is pinned to each
 *  capture's date so the year-less page dates resolve deterministically.
 *
 *  Two captures, because the venue has published its screening lines in two
 *  different shapes and the parser has to read both:
 *   - `art-kino-krosno` (2026-08-04) — each time sits in its OWN coloured
 *     `<span>` and the anchor is sometimes buried several spans deeper, so the
 *     time is nowhere near the anchor's previous sibling.
 *   - `art-kino-krosno-plain-time-lines` (2026-06-23) — the older, flat
 *     `HH:MM - <a>TITLE</a>` line.
 *
 *  Previously scraped from Filmweb, which had silently gone empty for it. */
class ArtKinoKrosnoClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val movies =
    new ArtKinoKrosnoClient(new FakeHttpFetch("art-kino-krosno"), today = LocalDate.of(2026, 8, 4)).fetch()

  "ArtKinoKrosnoClient" should "return a non-empty, single-cinema film list" in {
    movies should not be empty
    movies.map(_.cinema).toSet shouldBe Set(KinoArtKino)
    all(movies.map(_.showtimes)) should not be empty
  }

  it should "pair every screening on the page with a time" in {
    // 34 film anchors across the 12 day headers, each with its own HH:MM.
    movies.flatMap(_.showtimes) should have size 34
  }

  it should "sentence-case the ALL-CAPS titles off the repertoire" in {
    movies.map(_.movie.title) should contain allOf ("Zaproszenie", "Kronika wypadków miłosnych")
  }

  it should "read a time out of its own <span>, not just the anchor's previous sibling" in {
    val film = movies.find(_.movie.title == "Pucio").value
    film.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 8, 2, 13, 45))
  }

  it should "pin a concrete screening run" in {
    val film = movies.find(_.movie.title == "Zaproszenie").value
    film.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 8, 7, 19, 15))
    film.showtimes should have size 7
  }

  it should "still date a day whose header misspells the month" in {
    // The venue published "4 sieprnia (wtorek)" — a transposed "sierpnia".
    val film = movies.find(_.movie.title == "Dobry chłopiec").value
    film.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 8, 4, 19, 40))
  }

  it should "keep reading the older flat 'HH:MM - <a>' screening lines" in {
    val older =
      new ArtKinoKrosnoClient(new FakeHttpFetch("art-kino-krosno-plain-time-lines"),
                              today = LocalDate.of(2026, 6, 23)).fetch()

    older.map(_.movie.title) should contain allOf ("Toy story 5", "Ojczyzna")
    val film = older.find(_.movie.title == "Toy story 5").value
    film.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 26, 14, 15))
    film.showtimes should have size 17
  }
}
