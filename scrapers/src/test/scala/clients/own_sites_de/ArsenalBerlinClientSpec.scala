package clients.own_sites_de

import clients.tools.FakeHttpFetch
import models.{Cinema, GermanRoster}
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.de.{ArsenalBerlinClient, GermanFeed, GermanSources}

import java.time.{LocalDate, LocalDateTime}

/** Replays Arsenal Berlin's calendar API as recorded 2026-10-14 (clients.tools.WriteArsenalBerlin): five
 *  pages, 15 to 31 October, the Ninjin Club retrospective among them — and the screening pages of
 *  Kaidan, Mozu, Ototo, Ogin-sama and Petra von Kant. Cineamo lists the retrospective's films bare
 *  (no year, director or runtime); the house's own calendar is what tells "Ogin-sama" (Tanaka, 1962)
 *  from its 1978 namesake. */
class ArsenalBerlinClientSpec extends AnyFlatSpec with Matchers with OptionValues {
  private val arsenal: Cinema = GermanRoster.theaterIdByCinema.collectFirst { case (c, "A0050") => c }.value

  private def client(today: LocalDate) =
    new ArsenalBerlinClient(new FakeHttpFetch("arsenal-berlin", strict = true), arsenal, today)

  private val movies = client(LocalDate.of(2026, 10, 14)).fetch()
  private def film(title: String) = movies.find(_.movie.title == title).value

  "ArsenalBerlinClient" should "give each retrospective film the director, year, country and runtime its calendar states" in {
    val facts = Seq("Kaidan", "Mozu", "Ototo", "Ogin-sama").map { title =>
      val m = film(title)
      (title, m.director, m.movie.releaseYear, m.movie.countries, m.movie.runtimeMinutes)
    }
    facts shouldBe Seq(
      ("Kaidan",    Seq("Masaki Kobayashi"), Some(1964), Seq("Japan"), Some(183)),
      ("Mozu",      Seq("Minoru Shibuya"),   Some(1961), Seq("Japan"), Some(95)),
      ("Ototo",     Seq("Kon Ichikawa"),     Some(1960), Seq("Japan"), Some(98)),
      ("Ogin-sama", Seq("Kinuyo Tanaka"),    Some(1962), Seq("Japan"), Some(102)))
    film("West Indies, les Nègres marrons de la liberté").movie.countries shouldBe Seq("Frankreich", "Mauretanien", "Algerien")
  }

  it should "place each yearless screening, whichever language its weekday is in, with its tickets and language version" in {
    val kaidan = film("Kaidan")
    kaidan.showtimes.map(_.dateTime) shouldBe Seq(LocalDateTime.of(2026, 10, 24, 20, 0), LocalDateTime.of(2026, 10, 31, 20, 0))
    kaidan.showtimes.map(_.format) shouldBe Seq(List("OmeU"), List("OmeU"))
    kaidan.showtimes.head.bookingUrl.value should startWith("https://kinotickets.express/arsenal-berlin/booking/")
    // "Fr. 23.10. 16:00" beside the other screenings' "Tue 27.10. 19:30"
    film("An American in Paris").showtimes.map(_.dateTime) shouldBe
      Seq(LocalDateTime.of(2026, 10, 23, 16, 0), LocalDateTime.of(2026, 10, 27, 19, 30))
    movies.flatMap(_.showtimes) should have size 33 // the 50 events less the foyer installation's 17
    movies.map(_.cinema).toSet shouldBe Set(arsenal)
  }

  it should "leave out the foyer's video installation, which is no screening" in {
    movies.map(_.movie.title) should not contain "Finding Norma"
  }

  it should "point each film at its first screening's page" in {
    film("Ogin-sama").filmUrl.value shouldBe "https://www.arsenal-berlin.de/kino/filmvorfuehrung/ogin-sama-5876/"
    film("Ototo").filmUrl.value shouldBe "https://www.arsenal-berlin.de/kino/filmvorfuehrung/ototo-5834/"
  }

  it should "drop the screenings already past" in {
    client(LocalDate.of(2026, 10, 25)).fetch().map(_.movie.title) should not contain "Ototo"
  }

  "Its screening page" should "add the cast and the house's synopsis beside the facts the calendar states" in {
    val ogin = client(LocalDate.of(2026, 10, 14)).fetchFilmDetail(film("Ogin-sama").filmUrl.value).value
    (ogin.director, ogin.cast, ogin.releaseYear, ogin.countries, ogin.runtimeMinutes) shouldBe
      (Seq("Kinuyo Tanaka"), Seq("Ineko Arima"), Some(1962), Seq("Japan"), Some(102))
    ogin.synopsis.value should startWith("Ein Film der Schlachtfelder")
    val petra = client(LocalDate.of(2026, 10, 14)).fetchFilmDetail(film("Die bitteren Tränen der Petra von Kant").filmUrl.value).value
    petra.cast shouldBe Seq("Margit Carstensen", "Irm Hermann", "Hanna Schygulla")
    // Kaidan's page credits no cast
    client(LocalDate.of(2026, 10, 14)).fetchFilmDetail(film("Kaidan").filmUrl.value).value.cast shouldBe empty
  }

  "Arsenal Berlin" should "be read from its own calendar first, Cineamo behind it" in {
    GermanSources.of("A0050", None).take(2).map(_.label) shouldBe Seq(GermanFeed.ArsenalBerlin.label, "Cineamo")
  }
}
