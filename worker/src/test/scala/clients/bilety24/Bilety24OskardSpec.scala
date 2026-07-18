package clients.bilety24

import org.scalatest.matchers.should.Matchers
import clients.tools.FakeHttpFetch
import org.scalatest.flatspec.AnyFlatSpec
import models.{KinoOskard, Showtime}
import services.cinemas.pl.Bilety24Client

import java.time.LocalDateTime

/** CKiS Konin "Oskard" uses the Bilety24 platform but books via
 *  /b24-do-miejsc-numerowanych-i-nienumerowanych/ instead of the usual
 *  /kup-bilety/ path. Recorded 2026-06-07 via WriteOskard. */
class Bilety24OskardSpec extends AnyFlatSpec with Matchers {

  private val oskard    = new Bilety24Client(new FakeHttpFetch("kino-oskard"), "https://ckis-konin.bilety24.pl", KinoOskard)
  private val oskardRes = oskard.fetch()
  private val oskardByT = oskardRes.map(cm => cm.movie.title -> cm).toMap

  "Bilety24Client (Oskard)" should "return 32 films and 144 showtimes" in {
    oskardRes.size shouldBe 32
    oskardRes.flatMap(_.showtimes).size shouldBe 144
  }

  it should "assign Kino Oskard to every entry" in {
    oskardRes.map(_.cinema).toSet shouldBe Set(KinoOskard)
  }

  it should "parse runtime + the schedule for a film via the b24-do-miejsc URL" in {
    // The per-cinema `xtra-oskard-kino-cafe` rule strips the venue's '/Kino Cafe'
    // suffix, so BOTH the base "Drugie życie" event and its "Drugie życie /Kino Cafe"
    // sibling now clean to the same title (they fold into one row downstream, by
    // sanitize key — not in the client). That makes the title an ambiguous lookup
    // key, so select the base event by its distinctive b24-do-miejsc showtime.
    val knownShowtime = Showtime(
      LocalDateTime.of(2026, 6, 13, 20, 30),
      Some("https://ckis-konin.bilety24.pl/b24-do-miejsc-numerowanych-i-nienumerowanych/?id=937926"),
      None, List("2D", "NAP"))
    val m = oskardRes.find(_.showtimes.contains(knownShowtime))
      .getOrElse(fail("no 'Drugie życie' event carrying the 2026-06-13 b24-do-miejsc showtime"))
    m.movie.title          shouldBe "Drugie życie"
    m.movie.runtimeMinutes shouldBe Some(116)
    m.movie.genres         shouldBe Seq("Dramat")
    m.filmUrl.getOrElse("") should include("/wydarzenie/?id=")
  }

  it should "build booking URLs starting with the venue base URL" in {
    oskardRes.flatMap(_.showtimes).flatMap(_.bookingUrl).foreach { u =>
      u should startWith("https://ckis-konin.bilety24.pl/")
    }
  }
}
