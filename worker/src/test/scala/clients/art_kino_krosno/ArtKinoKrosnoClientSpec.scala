package clients.art_kino_krosno

import clients.tools.FakeHttpFetch
import models.KinoArtKino
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.ArtKinoKrosnoClient

import java.time.{LocalDate, LocalDateTime}

/** Replays the recorded `artkino.rckp.krosno.pl/strona-375-repertuar.html`
 *  article for artKino (Krosno) through the client. `today` is pinned to the
 *  capture date so the year-less page dates resolve deterministically.
 *
 *  Previously scraped from Filmweb, which had silently gone empty for it. */
class ArtKinoKrosnoClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val movies =
    new ArtKinoKrosnoClient(new FakeHttpFetch("art-kino-krosno"), today = LocalDate.of(2026, 6, 23)).fetch()

  "ArtKinoKrosnoClient" should "return a non-empty, single-cinema film list" in {
    movies should not be empty
    movies.map(_.cinema).toSet shouldBe Set(KinoArtKino)
    all(movies.map(_.showtimes)) should not be empty
  }

  it should "sentence-case the ALL-CAPS titles off the repertoire" in {
    movies.map(_.movie.title) should contain allOf ("Toy story 5", "Ojczyzna")
  }

  it should "pin a concrete screening with the year inferred from today" in {
    val film = movies.find(_.movie.title == "Toy story 5").value
    film.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 26, 14, 15))
    film.showtimes should have size 17
  }
}
