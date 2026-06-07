package clients.kino_kijow

import clients.tools.FakeHttpFetch
import models.KinoKijow
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.KinoKijowClient

import java.time.{LocalDate, LocalDateTime, YearMonth}

/** Replays the recorded `kupbilet.kijow.pl/MSI/mvc/pl?sort=Date&date=2026-06`
 *  page (07-06-2026 capture) through the client. The page covers June 2026;
 *  `today` is pinned to the capture date so the "also fetch next month" logic
 *  stays deterministic (no July fetch is triggered when `daysLeft > 14`). */
class KinoKijowClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val http   = new FakeHttpFetch("kino-kijow")
  private val client = new KinoKijowClient(http, KinoKijow, LocalDate.of(2026, 6, 7))

  "KinoKijowClient" should "return a non-empty film list" in {
    client.fetch() should not be empty
  }

  it should "tag every film with KinoKijow" in {
    client.fetch().map(_.cinema).toSet shouldBe Set(KinoKijow)
  }

  it should "give every film at least one showtime" in {
    all(client.fetch().map(_.showtimes)) should not be empty
  }

  it should "pin a concrete screening: Niesamowite przygody skarpetek 3 on 2026-06-07 at 10:30" in {
    val movies   = client.fetch()
    val skarpety = movies.find(_.movie.title.contains("skarpetek")).value
    skarpety.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 7, 10, 30))
    skarpety.showtimes.flatMap(_.bookingUrl).head should include("/MSI/Default.aspx?event_id=")
  }
}
