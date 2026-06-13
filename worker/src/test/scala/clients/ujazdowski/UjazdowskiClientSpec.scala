package clients.ujazdowski

import clients.tools.FakeHttpFetch
import models.{Showtime, Ujazdowski}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.UjazdowskiClient

import java.time.LocalDateTime

class UjazdowskiClientSpec extends AnyFlatSpec with Matchers {

  private val client  = new UjazdowskiClient(new FakeHttpFetch("ujazdowski"))
  private val results = client.fetch()
  private val byTitle = results.map(cm => cm.movie.title -> cm).toMap

  "UjazdowskiClient.fetch" should "return 6 films and 20 showtimes from the day AJAX pages" in {
    results.size shouldBe 6
    results.flatMap(_.showtimes).size shouldBe 20
  }

  it should "assign Kino U-jazdowski to every entry" in {
    results.map(_.cinema).toSet shouldBe Set(Ujazdowski)
  }

  it should "parse runtime/year/countries from the meta line and date from the ut timestamp" in {
    val m = byTitle("Erupcja")
    m.movie.runtimeMinutes shouldBe Some(71)
    m.movie.releaseYear    shouldBe Some(2025)
    m.movie.countries      shouldBe Seq("Polska", "USA")
    m.showtimes.size       shouldBe 5
    m.showtimes.head shouldBe
      Showtime(LocalDateTime.of(2026, 5, 25, 20, 30), Some("https://u-jazdowski.pl/kino/repertuar/erupcja"), None, Nil)
  }

  it should "read the bracketed original title off a foreign film, and none off a Polish one" in {
    val foreignDetail = client.fetchFilmDetail(byTitle("Zawieście czerwone latarnie").filmUrl.getOrElse(fail("no filmUrl for Zawieście czerwone latarnie")))
      .getOrElse(fail("no detail for Zawieście czerwone latarnie"))
    foreignDetail.originalTitle shouldBe Some("Da hong deng long gao gao gua")

    val polishDetail = client.fetchFilmDetail(byTitle("Erupcja").filmUrl.getOrElse(fail("no filmUrl for Erupcja")))
      .getOrElse(fail("no detail for Erupcja"))
    polishDetail.originalTitle shouldBe None
  }
}
