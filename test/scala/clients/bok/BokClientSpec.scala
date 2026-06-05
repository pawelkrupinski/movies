package clients.bok

import clients.tools.FakeHttpFetch
import models.{KinoGlebocka66, KinoNaBoku, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.BokClient

import java.time.{LocalDate, LocalDateTime}

class BokClientSpec extends AnyFlatSpec with Matchers {

  private val today = LocalDate.of(2026, 6, 5)

  // ── Kino na Boku ─────────────────────────────────────────────────────────
  private val naBoku    = new BokClient(new FakeHttpFetch("kino-na-boku"), "kino-na-boku", KinoNaBoku, today)
  private val naBokuRes = naBoku.fetch()
  private val naBokuByT = naBokuRes.map(cm => cm.movie.title -> cm).toMap

  "BokClient (na Boku)" should "return 4 films and 24 showtimes" in {
    naBokuRes.size shouldBe 4
    naBokuRes.flatMap(_.showtimes).size shouldBe 24
  }

  it should "assign Kino na Boku to every entry" in {
    naBokuRes.map(_.cinema).toSet shouldBe Set(KinoNaBoku)
  }

  it should "strip the promo suffix, enrich runtime, and build biletyna booking URLs" in {
    val m = naBokuByT("Drzewo magii")
    m.movie.runtimeMinutes shouldBe Some(110)
    m.showtimes.size       shouldBe 5
    naBokuByT("Zawodowcy").showtimes.size shouldBe 11
    m.showtimes.head shouldBe
      Showtime(LocalDateTime.of(2026, 6, 5, 15, 30), Some("https://iframe17.biletyna.pl/event/view/id/665160"), None, Nil)
  }

  it should "read the cast list off the detail page" in {
    naBokuByT("Drzewo magii").cast shouldBe
      Seq("Andrew Garfield", "Claire Foy", "Rebecca Ferguson", "Nicola Coughlan", "Jessica Gunning")
  }

  // ── Kino Głębocka 66 (same client, different slug prefix) ─────────────────
  private val gleb    = new BokClient(new FakeHttpFetch("kino-glebocka-66"), "kino-glebocka-66", KinoGlebocka66, today)
  private val glebRes = gleb.fetch()

  "BokClient (Głębocka 66)" should "return 2 films and 6 showtimes, all tagged Głębocka 66" in {
    glebRes.size shouldBe 2
    glebRes.flatMap(_.showtimes).size shouldBe 6
    glebRes.map(_.cinema).toSet shouldBe Set(KinoGlebocka66)
  }
}
