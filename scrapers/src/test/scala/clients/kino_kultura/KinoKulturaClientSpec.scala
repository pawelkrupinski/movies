package clients.kino_kultura

import models.{KinoKultura, Showtime}
import org.scalatest.matchers.should.Matchers
import clients.tools.FakeHttpFetch
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas.pl.KinoKulturaClient

import java.time.LocalDateTime

class KinoKulturaClientSpec extends AnyFlatSpec with Matchers {

  private val client  = new KinoKulturaClient(new FakeHttpFetch("kino-kultura"))
  private val results = client.fetch()
  private val byTitle = results.map(cm => cm.movie.title -> cm).toMap

  "KinoKulturaClient.fetch" should "read the nonce + date list off the main page and return 23 films / 62 showtimes" in {
    results.size shouldBe 23
    results.flatMap(_.showtimes).size shouldBe 62
  }

  it should "assign Kino Kultura to every entry" in {
    results.map(_.cinema).toSet shouldBe Set(KinoKultura)
  }

  it should "merge a film's screenings across the AJAX day responses" in {
    val counts = results.map(m => m.movie.title -> m.showtimes.size).toMap
    counts("Podziemny krąg") shouldBe 7
    counts("Zawodowcy")      shouldBe 7
  }

  it should "carry the auditorium and a date-scoped booking URL" in {
    byTitle("Podziemny krąg").showtimes.head shouldBe
      Showtime(
        LocalDateTime.of(2026, 6, 5, 19, 45),
        Some("https://rezerwacja.kinokultura.pl/MSI/mvc/pl?sort=Flow&date=2026-06-05&datestart=0"),
        Some("Kultura"), Nil
      )
  }

  it should "carry a poster URL" in {
    byTitle("Podziemny krąg").posterUrl.getOrElse("") should startWith ("https://www.kinokultura.pl/foto,")
  }
}
