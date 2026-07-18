package clients.kino_kika

import org.scalatest.OptionValues
import clients.tools.FakeHttpFetch
import models.KinoKika
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas.pl.KinoKikaClient

import java.time.LocalDateTime

/** Replays the recorded `bilety.kinokika.pl/` page (07-06-2026 capture) through
 *  the client. The portal renders a week of screenings server-side in a single
 *  HTML response; rows carry the date as a CSS class and the time in a clock-icon
 *  span inside `div.date`. */
class KinoKikaClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val http   = new FakeHttpFetch("kino-kika")
  private val client = new KinoKikaClient(http, KinoKika)

  "KinoKikaClient" should "return a non-empty film list" in {
    client.fetch() should not be empty
  }

  it should "tag every film with KinoKika" in {
    client.fetch().map(_.cinema).toSet shouldBe Set(KinoKika)
  }

  it should "give every film at least one showtime" in {
    all(client.fetch().map(_.showtimes)) should not be empty
  }

  it should "pin a concrete screening: Drzewo Magii on 2026-06-07 at 12:00" in {
    val movies = client.fetch()
    val drzewo = movies.find(_.movie.title.toLowerCase.contains("drzewo magii")).value
    drzewo.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 7, 12, 0))
    drzewo.showtimes.flatMap(_.bookingUrl).head should include("bilety.kinokika.pl")
  }
}
