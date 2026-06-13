package clients.kinoteka

import clients.tools.FakeHttpFetch
import models.{Kinoteka, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.{FilmDetail, KinotekaClient}

import java.time.LocalDateTime

class KinotekaClientSpec extends AnyFlatSpec with Matchers {

  private val client  = new KinotekaClient(new FakeHttpFetch("kinoteka"))
  private val results = client.fetch()
  private val byTitle = results.map(cm => cm.movie.title -> cm).toMap

  private def detailFor(title: String): FilmDetail =
    client.fetchFilmDetail(
      byTitle(title).filmUrl.getOrElse(fail(s"no filmUrl for $title"))
    ).getOrElse(fail(s"no detail for $title"))

  "KinotekaClient.fetch" should "walk the date nav and return 82 films / 268 showtimes" in {
    results.size shouldBe 82
    results.flatMap(_.showtimes).size shouldBe 268
  }

  it should "assign Kinoteka to every entry" in {
    results.map(_.cinema).toSet shouldBe Set(Kinoteka)
  }

  it should "merge a film across days and carry listing fields on the bare fetch result" in {
    val m = byTitle("Zawodowcy")
    m.showtimes.size       shouldBe 28
    m.movie.genres         shouldBe Seq("Akcja", "Dramat")
    m.filmUrl              shouldBe Some("https://kinoteka.pl/film/zawodowcy/")
    byTitle("Diabeł ubiera się u Prady 2").showtimes.size shouldBe 21
  }

  it should "enrich runtime / year / countries / original title from the detail page" in {
    val d = detailFor("Zawodowcy")
    d.runtimeMinutes shouldBe Some(100)
    d.releaseYear    shouldBe Some(2026)
    d.countries      shouldBe Seq("USA", "Wielka Brytania")
    d.originalTitle  shouldBe Some("In the Grey")
  }

  it should "read the cast list and the YouTube trailer off the detail page" in {
    val d = detailFor("Zawodowcy")
    d.cast       shouldBe Seq("Henry Cavill", "Rosamund Pike", "Jake Gyllenhaal")
    d.trailerUrl shouldBe Some("https://www.youtube.com/watch?v=AYq1ljpbNfA")
  }

  it should "carry the screening booking URL with absolute date" in {
    byTitle("Zawodowcy").showtimes.head shouldBe
      Showtime(
        LocalDateTime.of(2026, 6, 5, 14, 15),
        Some("https://bilety.kinoteka.pl/#/screen?screeningId=42d45fbe-db10-4ef1-b5e4-0eeb0c149927&cinemaId=9ef78349-db9c-4dfc-85aa-96d030082c0d"),
        None, Nil
      )
  }
}
