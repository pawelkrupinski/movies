package clients.kino_lot_swidnik

import org.scalatest.OptionValues
import clients.tools.FakeHttpFetch
import models.KinoLotSwidnik
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas.pl.KinoLotSwidnikClient

import java.time.LocalDateTime

/** Replays the recorded `iframe22.biletyna.pl/if/index/?ifid=22&q=` biletyna
 *  "widget" page — the schedule MOK Świdnik's own site embeds via an
 *  `easyXDM` cross-domain iframe — through the client.
 *
 *  2026-09-23 nearby-towns sweep, assigned to Lublin's catchment. This is an
 *  UNRELATED biletyna integration from Kino Lot Jelenia Góra (a
 *  `kino-lot.bilety24.pl` subdomain), and a different page shape from the
 *  JSON-LD `biletyna.pl/<City>/<Venue>` place page [[services.cinemas.pl.BiletynaClient]]
 *  parses. */
class KinoLotSwidnikClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val movies = new KinoLotSwidnikClient(new FakeHttpFetch("kino-lot-swidnik")).fetch()

  "KinoLotSwidnikClient" should "return a non-empty, single-cinema film list" in {
    movies should not be empty
    movies.map(_.cinema).toSet shouldBe Set(KinoLotSwidnik)
    all(movies.map(_.showtimes)) should not be empty
  }

  it should "pin a concrete screening read off the date + time bold spans" in {
    val film = movies.find(_.movie.title.toLowerCase.contains("gwiazdozbiór psa")).value
    film.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 9, 23, 19, 15))
  }

  it should "strip the trailing format tag into Showtime.format" in {
    // Raw title in the fixture: "Gwiazdozbiór psa (2D/napisy)".
    val film = movies.find(_.movie.title.toLowerCase.contains("gwiazdozbiór psa")).value
    film.movie.title should not include "("
    film.showtimes.map(_.format).head should contain allOf ("2D", "NAP")
  }

  it should "carry a per-screening booking link and poster" in {
    val film = movies.find(_.movie.title.toLowerCase.contains("gwiazdozbiór psa")).value
    film.showtimes.flatMap(_.bookingUrl).head should include("/event/view/id/")
    film.posterUrl.value should include("/file/get/id/")
  }
}
