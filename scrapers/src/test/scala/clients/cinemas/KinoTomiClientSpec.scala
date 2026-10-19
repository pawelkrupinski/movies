package clients.cinemas

import clients.tools.{FailingHttpFetch, FakeHttpFetch}
import models.KinoTomi
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.common.DetailEnricher
import services.cinemas.pl.KinoTomiClient
import tools.HttpStatusException

import java.time.{LocalDate, LocalDateTime}

/** Replays the recorded Kino Tomi (Pabianice) `/repertuarr` page (2026-09-27
 *  capture) — one server-rendered page holding every scheduled day (27 Sep
 *  through 14 Nov) as `div.seat-plan-row[data-date]` sections — plus two
 *  `/film/<slug>` detail pages for the deferred-detail assertions.
 *
 *  Fixture directory: test/resources/fixtures/kino-tomi/ */
class KinoTomiClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val client = new KinoTomiClient(new FakeHttpFetch("kino-tomi"), KinoTomi)
  private lazy val movies = client.fetch()

  private def film(title: String) = movies.find(_.movie.title == title).value

  "KinoTomiClient" should "read every day section of the multi-week calendar" in {
    val dates = movies.flatMap(_.showtimes).map(_.dateTime.toLocalDate).toSet
    dates should contain allOf (LocalDate.of(2026, 9, 27), LocalDate.of(2026, 10, 8), LocalDate.of(2026, 11, 14))
    movies.map(_.cinema).toSet shouldBe Set(KinoTomi)
  }

  it should "pin a screening to its day, time, booking link, film page and poster" in {
    val asterix = film("Asterix i Obelix: Misja Kleopatra")
    val show    = asterix.showtimes.find(_.dateTime == LocalDateTime.of(2026, 9, 27, 10, 30)).value
    show.bookingUrl.value shouldBe "https://kinotomi.pl/zamowienie/sala?event=b85d7e6a-bb9b-4fed-bf4c-829e70d0cbed"
    show.format           shouldBe List("DUB")
    asterix.filmUrl.value shouldBe "https://kinotomi.pl/film/2026-asterix-i-obelix-misja-kleopatra"
    asterix.posterUrl.value shouldBe "https://kinotomi.pl/files/cinema_movies/6448/main_20260817_165114_1932036710.jpg"
    film("Tedi i magiczna lampa").showtimes.map(_.dateTime) should contain allOf (
      LocalDateTime.of(2026, 9, 27, 11, 30), LocalDateTime.of(2026, 9, 27, 15, 45))
  }

  it should "keep the version suffix out of the title — even one with its own ' - ' — and on the showtime instead" in {
    // "Avengers: Koniec gry - wersja rozszerzona" is billed both dubbed and
    // subtitled; the language sits in its own `<span>` ("- Dubbing" /
    // "- Napisy"), so both land on ONE film, each showing badged by its own.
    val avengers = film("Avengers: Koniec gry - wersja rozszerzona")
    avengers.showtimes.map(_.format).toSet shouldBe Set(List("DUB"), List("NAP"))
    movies.map(_.movie.title) should not contain "Avengers: Koniec gry"
    // "Polski" (a Polish-language film) is no badge; the title stays bare.
    all(film("Lalka").showtimes.map(_.format)) shouldBe empty
    movies.map(_.movie.title).filter(_.contains("Dubbing")) shouldBe empty
    // A leading space in the source title (" Pucio kocha zwierzaki") is trimmed.
    film("Pucio kocha zwierzaki").showtimes should not be empty
  }

  it should "fetch the detail page's director, runtime, genres, cast, age rating, synopsis and trailer" in {
    val d = client.fetchFilmDetail("https://kinotomi.pl/film/2026-avengers-koniec-gry-wersja-rozszerzona").value
    d.director       shouldBe Seq("Anthony Russo", "Joe Russo")
    d.runtimeMinutes shouldBe Some(183)
    d.genres         shouldBe Seq("Akcja", "Sci-Fi")
    d.cast           should contain allOf ("Robert Downey Jr.", "Scarlett Johansson")
    d.ageRating      shouldBe Some("12+")
    d.synopsis.value should include("Thanosem")
    d.trailerUrl.value shouldBe "https://www.youtube.com/watch?v=dwZ1KNqT_z4"
    // The page's "Rok: 2026" is the Polish (re-)release year, not the
    // production year — this 2019 film and Asterix (2002) both say 2026 —
    // so it must not reach TMDB as a year hint.
    d.releaseYear    shouldBe None
  }

  it should "defer TMDB resolution to the detail page, which alone carries the director" in {
    client shouldBe a[DetailEnricher]
    client.defersTmdbResolution shouldBe true
    client.detailGroup shouldBe "kino-tomi"
  }

  it should "propagate a fetch failure instead of reporting an empty scrape" in {
    a[HttpStatusException] should be thrownBy new KinoTomiClient(new FailingHttpFetch(503), KinoTomi).fetch()
  }
}
