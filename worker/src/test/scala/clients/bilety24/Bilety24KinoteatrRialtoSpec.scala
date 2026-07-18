package clients.bilety24

import org.scalatest.matchers.should.Matchers
import clients.tools.FakeHttpFetch
import org.scalatest.flatspec.AnyFlatSpec
import models.KinoteatrRialto
import services.cinemas.pl.Bilety24Client

import java.time.LocalDateTime

/** Kinoteatr Rialto Katowice — the third Silesia Film venue, hosted on
 *  bilety24.pl at kinoteatrrialto.bilety24.pl (confirmed from live site
 *  2026-06-07: film-event pages on the venue's own WordPress carry ticket
 *  purchase links pointing to kinoteatrrialto.bilety24.pl, so Bilety24Client
 *  covers it without a bespoke scraper).
 *
 *  Fixtures recorded 2026-06-07 from the live bilety24 site. */
class Bilety24KinoteatrRialtoSpec extends AnyFlatSpec with Matchers {

  private val client = new Bilety24Client(
    new FakeHttpFetch("kinoteatr-rialto"),
    "https://kinoteatrrialto.bilety24.pl",
    KinoteatrRialto
  )
  private val result = client.fetch()
  private val byTitle = result.map(cm => cm.movie.title -> cm).toMap

  "Bilety24Client (Kinoteatr Rialto Katowice)" should "return 26 films and 40 showtimes" in {
    result.size shouldBe 26
    result.flatMap(_.showtimes).size shouldBe 40
  }

  it should "assign KinoteatrRialto to every entry and book on its Bilety24 host" in {
    result.map(_.cinema).toSet shouldBe Set(KinoteatrRialto)
    result.flatMap(_.showtimes).flatMap(_.bookingUrl).foreach { u =>
      u should startWith("https://kinoteatrrialto.bilety24.pl/kup-bilety/")
    }
  }

  it should "parse runtime + the schedule for a film" in {
    val m = byTitle("Diabeł ubiera się u Prady 2")
    m.movie.runtimeMinutes shouldBe Some(120)
    m.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 7, 20, 0))
    m.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 11, 15, 30))
  }
}
