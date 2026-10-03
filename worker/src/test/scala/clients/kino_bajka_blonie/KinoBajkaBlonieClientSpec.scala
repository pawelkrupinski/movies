package clients.kino_bajka_blonie

import clients.tools.{FailingHttpFetch, FakeHttpFetch}
import models.KinoBajkaBlonie
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.pl.KinoBajkaBlonieClient
import tools.HttpStatusException

import java.time.{LocalDate, LocalDateTime}

/** Replays `kino.blonie.pl` (Kino Bajka, Centrum Kultury w Błoniu) as captured on
 *  2026-09-27: the `/filmy/` and homepage listings, every linked `/film/<slug>/`
 *  page, and each screening film's `base_cinema_film_dates` POST (cursor
 *  2026-09-26, so the pinned `today` must stay 2026-09-27 for the bodies to match).
 *  Fixture directory: test/resources/fixtures/kino-centrum-kultury-blonie/ */
class KinoBajkaBlonieClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val today  = LocalDate.of(2026, 9, 27)
  private val movies = new KinoBajkaBlonieClient(new FakeHttpFetch("kino-bajka-blonie"), KinoBajkaBlonie, today).fetch()

  "KinoBajkaBlonieClient" should "return every film with scheduled screenings, and none of the dateless announcements" in {
    movies.map(_.movie.title) shouldBe Seq("Lalka", "Powiedz mi, co czujesz", "Tedi i magiczna lampa", "Zapomniana wyspa")
    movies.map(_.cinema).toSet shouldBe Set(KinoBajkaBlonie)
  }

  it should "read every day past the four-day carousel from the film-dates endpoint" in {
    val film = movies.find(_.movie.title == "Zapomniana wyspa").value
    film.showtimes.map(_.dateTime) shouldBe Seq(3, 4, 6, 7, 8).flatMap { day =>
      Seq(LocalDateTime.of(2026, 10, day, 15, 0), LocalDateTime.of(2026, 10, day, 17, 0))
    }
    all(film.showtimes.map(_.bookingUrl)) shouldBe None
    all(film.showtimes.map(_.format)) shouldBe List("DUB")
    // An unrated film's age slot reads "Bez ograniczeń" — the country after it still counts.
    film.movie.countries shouldBe Seq("USA")
    film.ageRating shouldBe None
    film.movie.genres shouldBe Seq("Animacja", "Familijny", "Przygodowy")
  }

  it should "carry the film page's identity signals" in {
    val film = movies.find(_.movie.title == "Lalka").value
    film.showtimes.map(_.dateTime) should contain allOf (LocalDateTime.of(2026, 10, 3, 19, 0), LocalDateTime.of(2026, 10, 8, 19, 0))
    film.movie.rawTitle.value shouldBe "LALKA"
    film.movie.runtimeMinutes.value shouldBe 162
    film.movie.genres shouldBe Seq("Dramat", "Obyczajowy")
    film.movie.countries shouldBe Seq("POLSKA")
    film.ageRating.value shouldBe "12+"
    film.synopsis.value should startWith("Warszawa końca XIX wieku.")
    film.posterUrl.value shouldBe "https://kino.blonie.pl/wp-content/uploads/2026/09/LALKA_PLAKAT-724x1024.jpg"
    film.trailerUrl.value shouldBe "https://www.youtube.com/watch?v=UsmL7HKwVWM"
    film.filmUrl.value shouldBe "https://kino.blonie.pl/film/lalka/"
  }

  it should "read a whole-hours runtime with no minutes part, keeping the facts around it" in {
    val page = org.jsoup.Jsoup.parse(
      """<h1>DWIE GODZINY</h1><ul class="film-meta-list">""" +
        Seq("2D napisy", "Dramat", "2 godz.", "Od lat: 12", "Francja", "Bilety: 20 zł")
          .map(f => s"""<li><span class="film-meta-list__text">$f</span></li>""").mkString + "</ul>",
      KinoBajkaBlonieClient.HomeUrl)
    val film = KinoBajkaBlonieClient.parseFilm(page, "https://kino.blonie.pl/film/dwie-godziny/",
      Seq(LocalDateTime.of(2026, 10, 3, 19, 0)), KinoBajkaBlonie).value
    film.movie.runtimeMinutes.value shouldBe 120
    film.movie.genres shouldBe Seq("Dramat")
    film.movie.countries shouldBe Seq("Francja")
    film.ageRating.value shouldBe "12+"
  }

  it should "leave countries empty when the page lists none" in {
    val film = movies.find(_.movie.title == "Tedi i magiczna lampa").value
    film.showtimes should have size 6
    film.movie.countries shouldBe empty
    film.movie.runtimeMinutes.value shouldBe 90
  }

  it should "propagate a listing failure instead of reporting an empty (white) scrape" in {
    val client = new KinoBajkaBlonieClient(new FailingHttpFetch(503), KinoBajkaBlonie, today)
    a[HttpStatusException] should be thrownBy client.fetch()
  }
}
