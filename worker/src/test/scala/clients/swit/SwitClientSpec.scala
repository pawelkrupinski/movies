package clients.swit

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import clients.tools.FakeHttpFetch
import models.{KinoSwit, Showtime}
import services.cinemas.pl.SwitClient

import java.time.LocalDateTime

class SwitClientSpec extends AnyFlatSpec with Matchers {

  private val client  = new SwitClient(new FakeHttpFetch("kino-swit"))
  private val results = client.fetch()
  private val byTitle = results.map(cm => cm.movie.title -> cm).toMap

  "SwitClient.fetch" should "return 2 films and 10 showtimes from the single listing page" in {
    results.size shouldBe 2
    results.flatMap(_.showtimes).size shouldBe 10
  }

  it should "assign Kino Świt to every entry" in {
    results.map(_.cinema).toSet shouldBe Set(KinoSwit)
  }

  it should "read runtime/year off the listing and absolute showtime dates" in {
    val m = byTitle("Diabeł ubiera się u Prady 2")
    m.movie.runtimeMinutes shouldBe Some(119)
    m.movie.releaseYear    shouldBe Some(2026)
    m.showtimes.size       shouldBe 4
    byTitle("Zawodowcy").showtimes.size shouldBe 6
    m.showtimes.head shouldBe
      Showtime(
        LocalDateTime.of(2026, 6, 5, 16, 45),
        Some("https://biletyna.pl/film/Diabel-ubiera-sie-u-Prady-2/Warszawa?utm_source=dkswit.com.pl&utm_medium=APIv2&utm_campaign=general"),
        None, Nil
      )
  }

  it should "read the genre badge off the card (and leave it empty when absent)" in {
    byTitle("Diabeł ubiera się u Prady 2").movie.genres shouldBe Seq("Komediodramat")
    byTitle("Zawodowcy").movie.genres                   shouldBe Seq.empty
  }
}
