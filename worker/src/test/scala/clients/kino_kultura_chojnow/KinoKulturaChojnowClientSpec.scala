package clients.kino_kultura_chojnow

import org.scalatest.OptionValues
import clients.tools.FakeHttpFetch
import models.KinoKulturaChojnow
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas.pl.KinoKulturaChojnowClient

import java.time.LocalDateTime

/** Replays the recorded `kino.chojnow.eu/repertuar,m15,s1500.html` card grid
 *  (MOKSiR Chojnów's "Kursorek" CMS) through the client.
 *
 *  2026-09-23 nearby-towns sweep, assigned to Legnica's catchment. The page
 *  carries only two programmed items at fetch time: an André Rieu concert
 *  broadcast spanning a multi-week date range (`Rozpoczęcie:` bare-dated, no
 *  time — the true per-date times are buried in free-text prose the client
 *  deliberately does not parse) and a single dated film screening. */
class KinoKulturaChojnowClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val movies = new KinoKulturaChojnowClient(new FakeHttpFetch("kino-kultura-chojnow")).fetch()

  "KinoKulturaChojnowClient" should "return a non-empty, single-cinema film list" in {
    movies should not be empty
    movies.map(_.cinema).toSet shouldBe Set(KinoKulturaChojnow)
    all(movies.map(_.showtimes)) should not be empty
  }

  it should "pin the single dated screening read off the structured Rozpoczęcie field" in {
    val film = movies.find(_.movie.title.toLowerCase.contains("vivaldi")).value
    film.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 9, 25, 12, 30))
  }

  it should "drop the multi-date event whose Rozpoczęcie carries no time" in {
    // The André Rieu card's Rozpoczęcie is a bare "2026-09-13" (no HH:MM) — it
    // spans multiple screening dates only described in free-text prose, so the
    // card yields no showtime rather than a guessed one.
    movies.map(_.movie.title.toLowerCase).exists(_.contains("rieu")) shouldBe false
  }

  it should "carry no booking link (ticketing is off-site, with no per-event URL on this page)" in {
    all(movies.flatMap(_.showtimes).map(_.bookingUrl)) shouldBe None
  }
}
