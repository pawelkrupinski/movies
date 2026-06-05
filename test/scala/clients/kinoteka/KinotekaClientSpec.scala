package clients.kinoteka

import clients.tools.FakeHttpFetch
import models.{Kinoteka, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.KinotekaClient

import java.time.LocalDateTime

class KinotekaClientSpec extends AnyFlatSpec with Matchers {

  private val client  = new KinotekaClient(new FakeHttpFetch("kinoteka"))
  private val results = client.fetch()
  private val byTitle = results.map(cm => cm.movie.title -> cm).toMap

  "KinotekaClient.fetch" should "walk the date nav and return 82 films / 268 showtimes" in {
    results.size shouldBe 82
    results.flatMap(_.showtimes).size shouldBe 268
  }

  it should "assign Kinoteka to every entry" in {
    results.map(_.cinema).toSet shouldBe Set(Kinoteka)
  }

  it should "merge a film across days and enrich from its detail page" in {
    val m = byTitle("Zawodowcy")
    m.showtimes.size       shouldBe 28
    m.movie.runtimeMinutes shouldBe Some(100)
    m.movie.releaseYear    shouldBe Some(2026)
    m.movie.countries      shouldBe Seq("USA", "Wielka Brytania")
    m.movie.genres         shouldBe Seq("Akcja", "Dramat")
    m.movie.originalTitle  shouldBe Some("In the Grey")
    m.filmUrl              shouldBe Some("https://kinoteka.pl/film/zawodowcy/")
    byTitle("Diabeł ubiera się u Prady 2").showtimes.size shouldBe 21
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
