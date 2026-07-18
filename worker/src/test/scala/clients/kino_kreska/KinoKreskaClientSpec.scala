package clients.kino_kreska

import models.KinoKreska
import org.scalatest.OptionValues
import clients.tools.FakeHttpFetch
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas.pl.KinoKreskaClient

import java.time.LocalDateTime

/** Replays the recorded SFR/Kino Kreska response (2026-06-21 capture of
 *  `www.sfr.pl/heroapp/terms/rest/load`, category "Repertuar kinowy")
 *  through the client. The endpoint returns a JSON envelope with an `items`
 *  HTML fragment; each `<li>` tile carries ISO date + time, so no year
 *  inference is needed. */
class KinoKreskaClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val movies = new KinoKreskaClient(new FakeHttpFetch("kino-kreska"), KinoKreska).fetch()

  "KinoKreskaClient" should "return a non-empty, single-cinema film list" in {
    movies should not be empty
    movies.map(_.cinema).toSet shouldBe Set(KinoKreska)
  }

  it should "produce non-empty titles for every film" in {
    all(movies.map(_.movie.title)) should not be empty
  }

  it should "give every film at least one showtime" in {
    all(movies.map(_.showtimes)) should not be empty
  }

  it should "produce plausible dates and times for all showtimes" in {
    val showtimes = movies.flatMap(_.showtimes)
    all(showtimes.map(_.dateTime.getYear)) shouldBe 2026
    all(showtimes.map(_.dateTime.getHour)) should (be >= 8 and be <= 23)
  }

  it should "pin a concrete film from the captured repertoire" in {
    // "Drzewo magii" (family film) screens on 2026-06-21 at 15:00
    val drewMagii = movies.find(_.movie.title == "Drzewo magii").value
    drewMagii.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 21, 15, 0))
  }

  it should "carry a booking URL for showtimes that have a ticket button" in {
    // The captured repertoire includes bookable screenings
    movies.flatMap(_.showtimes).exists(_.bookingUrl.isDefined) shouldBe true
  }
}
