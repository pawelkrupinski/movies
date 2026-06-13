package clients.amondo

import clients.tools.FakeHttpFetch
import models.{KinoAmondo, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.{AmondoClient, FilmDetail}

import java.time.LocalDateTime

class AmondoClientSpec extends AnyFlatSpec with Matchers {

  private val client  = new AmondoClient(new FakeHttpFetch("kino-amondo"))
  private val results = client.fetch()
  private val byTitle = results.map(cm => cm.movie.title -> cm).toMap

  private def detailFor(title: String): FilmDetail =
    client.fetchFilmDetail(
      byTitle.getOrElse(title, fail(s"no movie for '$title'")).filmUrl.getOrElse(fail(s"no filmUrl for '$title'"))
    ).getOrElse(fail(s"fetchFilmDetail returned None for '$title'"))

  "AmondoClient.fetch" should "return 29 films and 32 showtimes" in {
    results.size shouldBe 29
    results.flatMap(_.showtimes).size shouldBe 32
  }

  it should "assign Kino Amondo to every entry" in {
    results.map(_.cinema).toSet shouldBe Set(KinoAmondo)
  }

  it should "take genre from the listing and runtime/year/countries/director from the detail page" in {
    val m = byTitle("Aftersun")
    m.movie.genres         shouldBe Seq("Dramat")
    val d = detailFor("Aftersun")
    d.runtimeMinutes shouldBe Some(98)
    d.releaseYear    shouldBe Some(2022)
    d.countries      shouldBe Seq("Wielka Brytania", "USA")
    m.posterUrl.getOrElse("") should startWith ("https://kinoamondo.pl")
    d.synopsis.getOrElse("").length should be > 20
  }

  it should "carry the room and booking URL on each showtime" in {
    byTitle("Aftersun").showtimes.head shouldBe
      Showtime(
        LocalDateTime.of(2026, 6, 5, 18, 15),
        Some("https://biletomat.pl/embeddables/repertoire?organizerId=1772&showId=30561"),
        Some("Sala II"), Nil
      )
  }
}
