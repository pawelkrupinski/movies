package clients.muranow

import clients.tools.FakeHttpFetch
import models.{KinoMuranow, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.{FilmDetail, MuranowClient}

import java.time.LocalDateTime

class MuranowClientSpec extends AnyFlatSpec with Matchers {

  private val client  = new MuranowClient(new FakeHttpFetch("kino-muranow"))
  private val results = client.fetch()
  private val byTitle = results.map(cm => cm.movie.title -> cm).toMap

  private def detailFor(title: String): FilmDetail =
    client.fetchFilmDetail(
      byTitle(title).filmUrl.getOrElse(fail(s"no filmUrl for $title"))
    ).getOrElse(fail(s"no detail for $title"))

  "MuranowClient.fetch" should "return 92 films from the month calendar" in {
    results.size shouldBe 92
  }

  it should "return 194 showtimes in total" in {
    results.flatMap(_.showtimes).size shouldBe 194
  }

  it should "assign Kino Muranów to every entry" in {
    results.map(_.cinema).toSet shouldBe Set(KinoMuranow)
  }

  it should "return correct showtime counts per film" in {
    val counts = results.map(m => m.movie.title -> m.showtimes.size).toMap
    counts("Milcząca przyjaciółka") shouldBe 21
    counts("Bez wyjścia")           shouldBe 11
    counts("Tajny agent")           shouldBe 9
  }

  it should "carry the film page URL and listing poster on the bare fetch result" in {
    val m = byTitle("Milcząca przyjaciółka")
    m.filmUrl              shouldBe Some("https://kinomuranow.pl/film/milczaca-przyjaciolka")
    m.posterUrl.getOrElse("") should startWith ("https://kinomuranow.pl/sites/default/files/")
  }

  it should "enrich metadata from the /film/<slug> detail page" in {
    val d = detailFor("Milcząca przyjaciółka")
    d.runtimeMinutes shouldBe Some(147)
    d.releaseYear    shouldBe Some(2025)
    d.countries      shouldBe Seq("Niemcy", "Francja", "Węgry")
    d.genres         shouldBe Seq("Dramat")
    d.synopsis.getOrElse("").length should be > 50
  }

  it should "read cast, original title and the YouTube trailer off the detail page" in {
    val d = detailFor("Milcząca przyjaciółka")
    d.cast shouldBe Seq("Tony Leung", "Luna Wedler", "Enzo Brumm", "Sylvester Groth",
                        "Martin Wuttke", "Johannes Hegemann", "Rainer Bock", "Léa Seydoux")
    d.originalTitle shouldBe Some("Stille Freundin")
    d.trailerUrl    shouldBe Some("https://www.youtube.com/watch?v=H1jQ_5vNGYk")
  }

  it should "build /tickets/<id>/buy booking URLs" in {
    results.flatMap(_.showtimes).flatMap(_.bookingUrl).foreach { u =>
      u should startWith ("https://kinomuranow.pl/tickets/")
    }
  }

  it should "return the first Milcząca przyjaciółka showtime fully specified" in {
    byTitle("Milcząca przyjaciółka").showtimes.head shouldBe
      Showtime(LocalDateTime.of(2026, 6, 5, 15, 0), Some("https://kinomuranow.pl/tickets/26878/buy"), None, Nil)
  }
}
