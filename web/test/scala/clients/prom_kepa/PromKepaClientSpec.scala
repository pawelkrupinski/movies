package clients.prom_kepa

import clients.tools.FakeHttpFetch
import models.{KinoKepa, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.PromKepaClient

import java.time.LocalDateTime

class PromKepaClientSpec extends AnyFlatSpec with Matchers {

  private val client  = new PromKepaClient(new FakeHttpFetch("kino-kepa"))
  private val results = client.fetch()
  private val byTitle = results.map(cm => cm.movie.title -> cm).toMap

  "PromKepaClient.fetch" should "return 4 films and 5 showtimes" in {
    results.size shouldBe 4
    results.flatMap(_.showtimes).size shouldBe 5
  }

  it should "assign Kino Kępa to every entry" in {
    results.map(_.cinema).toSet shouldBe Set(KinoKepa)
  }

  it should "merge the inconsistently-cased duplicate into one well-cased title" in {
    results.map(_.movie.title) should contain ("Diabeł ubiera się u Prady 2")
    results.map(_.movie.title) should not contain "DIABEŁ UBIERA SIĘ U PRADY 2"
    byTitle("Diabeł ubiera się u Prady 2").showtimes.size shouldBe 2
  }

  it should "carry date+time and a PROM booking URL on each showtime" in {
    byTitle("Diabeł ubiera się u Prady 2").showtimes.head shouldBe
      Showtime(
        LocalDateTime.of(2026, 6, 5, 19, 0),
        Some("https://bilety.promkultury.pl/rezerwacja/nienumerowane.html?id=2953&idt=20b159bb6f50dc1e20082209ff425481&d=3"),
        None, Nil
      )
  }
}
