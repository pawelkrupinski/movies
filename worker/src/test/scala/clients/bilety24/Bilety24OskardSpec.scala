package clients.bilety24

import clients.tools.FakeHttpFetch
import models.{KinoOskard, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.Bilety24Client

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
    val m = oskardByT("Drugie życie")
    m.movie.runtimeMinutes shouldBe Some(116)
    m.movie.genres         shouldBe Seq("Dramat")
    m.showtimes.size       shouldBe 4
    m.filmUrl.getOrElse("") should include("/wydarzenie/?id=")
    m.showtimes.head shouldBe
      Showtime(LocalDateTime.of(2026, 6, 13, 20, 30), Some("https://ckis-konin.bilety24.pl/b24-do-miejsc-numerowanych-i-nienumerowanych/?id=937926"), None, List("2D", "NAP"))
  }

  it should "build booking URLs starting with the venue base URL" in {
    oskardRes.flatMap(_.showtimes).flatMap(_.bookingUrl).foreach { u =>
      u should startWith("https://ckis-konin.bilety24.pl/")
    }
  }
}
