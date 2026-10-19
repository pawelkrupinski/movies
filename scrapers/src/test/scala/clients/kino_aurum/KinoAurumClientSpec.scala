package clients.kino_aurum

import org.scalatest.OptionValues
import clients.tools.FakeHttpFetch
import models.KinoAurum
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas.pl.KinoAurumClient

import java.time.LocalDateTime

/** Replays the recorded Firestore `seanse` collection for Kino Aurum
 *  (Złotoryja) through the client. Each screening document carries the title, date
 *  and time directly, so one fetch is the whole programme.
 *
 *  Previously scraped from Filmweb, which had silently gone empty for it. */
class KinoAurumClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val movies = new KinoAurumClient(new FakeHttpFetch("kino-aurum")).fetch()

  "KinoAurumClient" should "return a non-empty, single-cinema film list" in {
    movies should not be empty
    movies.map(_.cinema).toSet shouldBe Set(KinoAurum)
    all(movies.map(_.showtimes)) should not be empty
  }

  it should "pin a concrete screening decoded from the Firestore JSON" in {
    val film = movies.find(_.movie.title == "Dzień objawienia").value
    film.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 12, 17, 30))
  }
}
