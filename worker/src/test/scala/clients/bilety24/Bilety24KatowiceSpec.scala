package clients.bilety24

import clients.tools.FakeHttpFetch
import models.{KinoKosmos, KinoSwiatowid, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.LocalDateTime
import services.cinemas.Bilety24Client

/** The same shared Bilety24Client, exercised against the two Katowice Silesia
 *  Film art-house cinemas (recorded 06-06-2026 via `WriteKatowiceBilety24`).
 *  Both publish a `/repertuar/` listing linking per-film `/wydarzenie/?id=N`
 *  pages on their Bilety24 subdomain — identical to Kino Luna / Elektronik, so
 *  this proves the new instances actually parse real Katowice repertoire,
 *  not that the parser changed. */
class Bilety24KatowiceSpec extends AnyFlatSpec with Matchers {

  // ── Kino Kosmos ──────────────────────────────────────────────────────────
  private val kosmos    = new Bilety24Client(new FakeHttpFetch("kino-kosmos"), "https://kinokosmos.bilety24.pl", KinoKosmos)
  private val kosmosRes = kosmos.fetch()
  private val kosmosByT = kosmosRes.map(cm => cm.movie.title -> cm).toMap

  "Bilety24Client (Kosmos)" should "return 61 films and 95 showtimes" in {
    kosmosRes.size shouldBe 61
    kosmosRes.flatMap(_.showtimes).size shouldBe 95
  }

  it should "assign Kino Kosmos to every entry and book on its Bilety24 host" in {
    kosmosRes.map(_.cinema).toSet shouldBe Set(KinoKosmos)
    kosmosRes.flatMap(_.showtimes).flatMap(_.bookingUrl).foreach { u =>
      u should startWith("https://kinokosmos.bilety24.pl/kup-bilety/")
    }
  }

  it should "parse runtime + the schedule for a film" in {
    val m = kosmosByT("Szepty lasu")
    m.movie.runtimeMinutes shouldBe Some(84)
    m.showtimes should contain(
      Showtime(LocalDateTime.of(2026, 6, 8, 14, 40), Some("https://kinokosmos.bilety24.pl/kup-bilety/?id=935530"), None, List("2D")))
  }

  // ── Kino Światowid ─────────────────────────────────────────────────────────
  private val swiatowid    = new Bilety24Client(new FakeHttpFetch("kino-swiatowid"), "https://swiatowid-katowice.bilety24.pl", KinoSwiatowid)
  private val swiatowidRes  = swiatowid.fetch()
  private val swiatowidByT  = swiatowidRes.map(cm => cm.movie.title -> cm).toMap

  "Bilety24Client (Światowid)" should "return 40 films and 79 showtimes" in {
    swiatowidRes.size shouldBe 40
    swiatowidRes.flatMap(_.showtimes).size shouldBe 79
  }

  it should "assign Kino Światowid to every entry and book on its Bilety24 host" in {
    swiatowidRes.map(_.cinema).toSet shouldBe Set(KinoSwiatowid)
    swiatowidRes.flatMap(_.showtimes).flatMap(_.bookingUrl).foreach { u =>
      u should startWith("https://swiatowid-katowice.bilety24.pl/kup-bilety/")
    }
  }

  it should "parse runtime + the schedule for a film" in {
    val m = swiatowidByT("K-popowe łowczynie demonów")
    m.movie.runtimeMinutes shouldBe Some(99)
    m.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 13, 17, 45))
  }
}
