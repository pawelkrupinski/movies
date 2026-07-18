package clients.kino_sdk_sanok

import org.scalatest.OptionValues
import clients.tools.FakeHttpFetch
import models.KinoSDK
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas.pl.KinoSDKSanokClient

import java.time.{LocalDate, LocalDateTime}

/** Replays the recorded `bilety.sdksanok.pl` MSI/Bilety24 culture-house listing
 *  through the client. The portal lists one day per fetch, so the fixture set is
 *  the entry day (2026-06-16) plus every date its picker enumerates; pinning
 *  `today` makes the entry fetch deterministic.
 *
 *  Kino SDK Sanok was previously scraped from Filmweb (id 2118); these fixtures
 *  prove its programme is real and reachable on its own ticketing portal. */
class KinoSDKSanokClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val movies =
    new KinoSDKSanokClient(new FakeHttpFetch("kino-sdk-sanok"), KinoSDK, today = LocalDate.of(2026, 6, 16)).fetch()

  "KinoSDKSanokClient" should "return a non-empty, single-cinema event list with showtimes" in {
    movies should not be empty
    movies.map(_.cinema).toSet shouldBe Set(KinoSDK)
    all(movies.map(_.showtimes)) should not be empty
  }

  it should "pin a concrete film and one of its showtimes" in {
    // Fixture: "Gwiezdne wojny: Mandalorian i Grogu" screens 2026-06-21 at 16:30.
    val film = movies.find(_.movie.title == "Gwiezdne wojny: Mandalorian i Grogu").value
    film.cinema shouldBe KinoSDK
    film.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 21, 16, 30))
  }
}
