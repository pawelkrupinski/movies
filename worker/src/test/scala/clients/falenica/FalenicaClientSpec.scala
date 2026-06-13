package clients.falenica

import clients.tools.FakeHttpFetch
import models.{Showtime, StacjaFalenica}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.FalenicaClient

import java.time.LocalDateTime

class FalenicaClientSpec extends AnyFlatSpec with Matchers {

  private val client  = new FalenicaClient(new FakeHttpFetch("kino-falenica"))
  private val results = client.fetch()
  private val byTitle = results.map(cm => cm.movie.title -> cm).toMap

  "FalenicaClient.fetch" should "return 26 films and 58 showtimes" in {
    results.size shouldBe 26
    results.flatMap(_.showtimes).size shouldBe 58
  }

  it should "assign Stacja Falenica to every entry" in {
    results.map(_.cinema).toSet shouldBe Set(StacjaFalenica)
  }

  it should "read runtime off the listing and showtimes off the detail page" in {
    val m = byTitle("Łowca jeleni")
    m.movie.runtimeMinutes shouldBe Some(182)
    m.showtimes.head shouldBe
      Showtime(LocalDateTime.of(2026, 6, 10, 19, 45), Some("https://ksf.systembiletowy.pl/index.php/repertoire.html?id=33335"), None, Nil)
  }

  it should "read the YouTube trailer off the detail page's WordPress video block" in {
    val detail = client.fetchFilmDetail(byTitle("Łowca jeleni").filmUrl.getOrElse(fail("no filmUrl for Łowca jeleni")))
      .getOrElse(fail("no detail for Łowca jeleni"))
    detail.trailerUrl shouldBe Some("https://www.youtube.com/watch?v=_08Qy34w2T4")
  }
}
