package clients.cinemas

import clients.tools.{FailingHttpFetch, FakeHttpFetch}
import models.{KinoLukow, Showtime}
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.pl.KinoLukowClient
import tools.HttpStatusException

import java.time.LocalDateTime

/** Replays the 27-09-2026 capture of Kino Łuków's own `/repertuar/` page: a
 *  day-tab widget of 35 `div.kino-rep-panel[data-day]` panels (25-09 → 29-10),
 *  each holding an `article.kino-film` per film playing that day with its
 *  `li.kino-slot[data-at]` screenings.
 *
 *  Fixture directory: test/resources/fixtures/kino-lukow/ (recorded with
 *  RecordingHttpFetch over RealHttpFetch). */
class KinoLukowClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val movies = new KinoLukowClient(new FakeHttpFetch("kino-lukow"), KinoLukow).fetch()
  private def film(title: String) = movies.find(_.movie.title == title).value

  "KinoLukowClient" should "fold the per-day film cards into one film per title" in {
    movies.map(_.movie.title) should contain allOf (
      "Tedi i magiczna lampa", "Mistyczka", "Lalka", "Primetime", "Bez znieczulenia",
      "„Niech żyje Maastricht!”. Nowy koncert André Rieu")
    movies should have size 15
    movies.map(_.cinema).toSet shouldBe Set(KinoLukow)
    movies.flatMap(_.showtimes) should have size 121
  }

  it should "read each screening's data-at, its version, and its ekobilet link once sale has opened" in {
    film("Mistyczka").showtimes.head shouldBe Showtime(LocalDateTime.of(2026, 9, 25, 18, 0), None, format = List("2D"))
    film("Tedi i magiczna lampa").showtimes.head.format shouldBe List("2D", "DUB")
    film("Gwiazdozbiór Psa").showtimes.head.format shouldBe List("2D", "NAP")
    film("Dzień dziecka księdza Jana Kaczkowskiego").showtimes should contain(Showtime(
      LocalDateTime.of(2026, 9, 28, 16, 0),
      Some("https://ekobilet.pl/lukowski-osrodek-kultury/dzien-dziecka-ksiedza-jana-kaczkowskiego-63769?calendar=true"),
      format = List("2D")))
  }

  it should "emit the card's runtime, genres, age, poster and film page" in {
    val mistyczka = film("Mistyczka")
    mistyczka.movie.runtimeMinutes shouldBe Some(120)
    mistyczka.movie.genres shouldBe Seq("Biograficzny", "Dramat")
    mistyczka.ageRating shouldBe Some("5+")
    mistyczka.posterUrl shouldBe Some("https://kino.lukow.pl/wp-content/uploads/2026/08/mistyczka_plakat_a3-scaled.jpg")
    mistyczka.filmUrl shouldBe Some("https://kino.lukow.pl/movies/mistyczka/")
    film("Tedi i magiczna lampa").ageRating shouldBe None // "B.O." = no restriction
  }

  it should "keep a whole synopsis and drop one the card cut short" in {
    film("Gwiazdozbiór Psa").synopsis.value should startWith("W postapokaliptycznym świecie")
    film("Mistyczka").synopsis shouldBe None
  }

  it should "propagate a fetch failure instead of reporting an empty (white) scrape" in {
    a[HttpStatusException] should be thrownBy new KinoLukowClient(new FailingHttpFetch(503), KinoLukow).fetch()
  }
}
