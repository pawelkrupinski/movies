package clients.cytadela

import clients.tools.FakeHttpFetch
import models.{KinoCytadela, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.CytadelaClient

import java.time.LocalDateTime

class CytadelaClientSpec extends AnyFlatSpec with Matchers {

  private val client  = new CytadelaClient(new FakeHttpFetch("kino-cytadela"))
  private val results = client.fetch()
  private val byTitle = results.map(cm => cm.movie.title -> cm).toMap

  "CytadelaClient.fetch" should "return 9 films and 10 showtimes from /repertuar" in {
    results.size shouldBe 9
    results.flatMap(_.showtimes).size shouldBe 10
  }

  it should "assign Kino Cytadela to every entry" in {
    results.map(_.cinema).toSet shouldBe Set(KinoCytadela)
  }

  it should "read genre/year/runtime off the listing and date off the day header" in {
    val m = byTitle("Pucio")
    m.movie.runtimeMinutes shouldBe Some(45)
    m.movie.releaseYear    shouldBe Some(2026)
    m.movie.genres         shouldBe Seq("Animacja")
    m.showtimes.head shouldBe
      Showtime(LocalDateTime.of(2026, 6, 6, 11, 0), Some("https://sklep.muzhp.pl/rezerwacja/rezerwacja/numerowane.html?id=17404&idt=98dd73275f980753c878e6a513e22226"), None, Nil)
  }
}
