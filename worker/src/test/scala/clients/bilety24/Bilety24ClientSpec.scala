package clients.bilety24

import clients.tools.FakeHttpFetch
import models.{KinoLuna, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.Bilety24Client

import java.time.LocalDateTime

/** One shared client, exercised against two recorded Bilety24-hosted cinemas. */
class Bilety24ClientSpec extends AnyFlatSpec with Matchers {

  // ── Kino Luna ──────────────────────────────────────────────────────────────
  private val luna     = new Bilety24Client(new FakeHttpFetch("kino-luna"), "https://kinoluna.bilety24.pl", KinoLuna)
  private val lunaRes  = luna.fetch()
  private val lunaByT  = lunaRes.map(cm => cm.movie.title -> cm).toMap

  "Bilety24Client (Luna)" should "return 18 films and 69 showtimes" in {
    lunaRes.size shouldBe 18
    lunaRes.flatMap(_.showtimes).size shouldBe 69
  }

  it should "assign Kino Luna to every entry" in {
    lunaRes.map(_.cinema).toSet shouldBe Set(KinoLuna)
  }

  it should "parse genres + runtime from movie-parameters and showtimes from the buy buttons" in {
    val m = lunaByT("Erupcja")
    m.movie.runtimeMinutes shouldBe Some(71)
    m.movie.genres         shouldBe Seq("Dramat", "Komedia")
    m.showtimes.size       shouldBe 6
    m.filmUrl.getOrElse("") should include ("/wydarzenie/?id=")
    m.synopsis.getOrElse("").length should be > 20
    m.showtimes.head shouldBe
      Showtime(LocalDateTime.of(2026, 6, 5, 16, 50), Some("https://kinoluna.bilety24.pl/kup-bilety/?id=938663"), None, List("2D", "NAP"))
  }

  it should "build kup-bilety booking URLs" in {
    lunaRes.flatMap(_.showtimes).flatMap(_.bookingUrl).foreach { u =>
      u should startWith ("https://kinoluna.bilety24.pl/kup-bilety/")
    }
  }

  // Regression: cycle/concert event descriptions carry a "czytaj więcej" toggle
  // and organiser links (Instagram/Facebook, "Więcej: www…"); none of it may
  // land in the synopsis.
  it should "keep organiser links and URLs out of synopses" in {
    val synopses = lunaRes.flatMap(_.synopsis)
    synopses.exists(_.contains("Jest upalne warszawskie lato")) shouldBe true // prose preserved
    synopses.foreach { s =>
      s should not include "http"
      s should not include "www."
      s should not include "czytaj więcej"
    }
  }
}
