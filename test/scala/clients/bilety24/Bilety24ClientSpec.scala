package clients.bilety24

import clients.tools.FakeHttpFetch
import models.{KinoElektronik, KinoLuna, Showtime}
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

  // ── Kino Elektronik ─────────────────────────────────────────────────────────
  private val elek    = new Bilety24Client(new FakeHttpFetch("kino-elektronik"), "https://kinoelektronik.pl", KinoElektronik, "/")
  private val elekRes = elek.fetch()
  private val elekByT = elekRes.map(cm => cm.movie.title -> cm).toMap

  "Bilety24Client (Elektronik)" should "assign Kino Elektronik to every entry" in {
    elekRes should not be empty
    elekRes.map(_.cinema).toSet shouldBe Set(KinoElektronik)
  }

  it should "parse runtime + the full-week schedule for a film" in {
    val m = elekByT("Orły republiki")
    m.movie.runtimeMinutes shouldBe Some(127)
    m.showtimes.size       shouldBe 7
    m.showtimes.head shouldBe
      Showtime(LocalDateTime.of(2026, 6, 5, 18, 15), Some("https://kinoelektronik.pl/kup-bilety/?id=938420"), None, Nil)
  }
}
