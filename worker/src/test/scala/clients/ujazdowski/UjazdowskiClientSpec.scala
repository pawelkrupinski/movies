package clients.ujazdowski

import clients.tools.FakeHttpFetch
import models.{Showtime, Ujazdowski}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.UjazdowskiClient

import java.time.{LocalDate, LocalDateTime}

class UjazdowskiClientSpec extends AnyFlatSpec with Matchers {

  // Pinned to the fixture capture date so the computed today..today+6 window
  // resolves to the recorded `week.ajax?ut=N` files.
  private val today   = LocalDate.of(2026, 6, 13)
  private val client  = new UjazdowskiClient(new FakeHttpFetch("ujazdowski"), today)
  private val results = client.fetch()
  private val byTitle = results.map(cm => cm.movie.title -> cm).toMap

  "UjazdowskiClient.fetch" should "return 12 films and 31 showtimes from the day AJAX pages" in {
    results.size shouldBe 12
    results.flatMap(_.showtimes).size shouldBe 31
  }

  it should "assign Kino U-jazdowski to every entry" in {
    results.map(_.cinema).toSet shouldBe Set(Ujazdowski)
  }

  // Regression for the nav-window-too-short bug: the listing nav stopped at
  // 06-15, but `week.ajax` already served 06-16+. The computed forward window
  // recovers those advance days — Kumotry's 06-16 screening and the film
  // "Sceny z życia małżeńskiego", which only plays 06-16, were FW-only before.
  it should "reach advance days the listing nav omits via the computed window" in {
    byTitle("Kumotry").showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 16, 17, 0))
    byTitle should contain key "Sceny z życia małżeńskiego"
    byTitle("Sceny z życia małżeńskiego").showtimes.map(_.dateTime) shouldBe Seq(LocalDateTime.of(2026, 6, 16, 18, 30))
  }

  it should "parse runtime/year/countries from the meta line and date from the ut timestamp" in {
    val m = byTitle("Erupcja")
    m.movie.runtimeMinutes shouldBe Some(71)
    m.movie.releaseYear    shouldBe Some(2025)
    m.movie.countries      shouldBe Seq("Polska", "USA")
    m.showtimes.size       shouldBe 2
    m.showtimes.head shouldBe
      Showtime(LocalDateTime.of(2026, 6, 11, 18, 30), Some("https://u-jazdowski.pl/kino/repertuar/erupcja"), None, Nil)
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
