package clients.cinemas

import clients.tools.{FailingHttpFetch, FakeHttpFetch}
import models.{KinoGwiazdaKetrzyn, Showtime}
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.pl.KinoGwiazdaKetrzynClient
import tools.HttpStatusException

import java.time.LocalDateTime

/** Replays the 27-09-2026 capture of Kino Gwiazda Kętrzyn's own Statamic site:
 *  the `/repertuar` listing (ten film cards) plus each card's `/filmy/<slug>`
 *  page, which carries the `<time datetime>` showtime table, the eurobilet
 *  booking links, director, cast, runtime, genres, language version, trailer
 *  and synopsis.
 *
 *  Fixture directory: test/resources/fixtures/kino-gwiazda-ketrzyn/ (recorded
 *  with RecordingHttpFetch over RealHttpFetch). */
class KinoGwiazdaKetrzynClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val movies = new KinoGwiazdaKetrzynClient(new FakeHttpFetch("kino-gwiazda-ketrzyn"), KinoGwiazdaKetrzyn).fetch()
  private def film(title: String) = movies.find(_.movie.title == title).value

  "KinoGwiazdaKetrzynClient" should "read every film card on the repertoire, tagged with the venue" in {
    movies.map(_.movie.title) should contain allOf (
      "LALKA", "LUNA I ROZGADANA ŚWINKA", "GORZKIE ŚWIĘTA - KINO KONESERA", "100 DNI: MISJA ZEUS")
    movies should have size 10
    movies.map(_.cinema).toSet shouldBe Set(KinoGwiazdaKetrzyn)
    all(movies.map(_.showtimes)) should not be empty
  }

  it should "date each showtime from the film page's <time datetime> pair and keep its eurobilet booking link" in {
    film("LALKA").showtimes should contain(Showtime(
      LocalDateTime.of(2026, 9, 30, 20, 0),
      Some("https://kinoketrzyn.eurobilet.pl/Rezerwacja/Default.aspx?event_id=16462&typetran=0&returnlink=https%3A%2F%2Fkino.ketrzyn.pl")))
    film("LALKA").showtimes should have size 25
    film("LUNA I ROZGADANA ŚWINKA").showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 9, 27, 15, 30))
  }

  it should "emit the identity signals the film page carries" in {
    val lalka = film("LALKA")
    lalka.director shouldBe Seq("Maciej Kawalski")
    lalka.movie.runtimeMinutes shouldBe Some(160)
    lalka.movie.genres shouldBe Seq("Dramat", "Romans")
    lalka.cast should contain allOf ("Marcin Dorociński", "Kamila Urzędowska", "Jakub Wieczorek")
    all(lalka.cast) should not include "„"
    lalka.filmUrl shouldBe Some("https://kino.ketrzyn.pl/filmy/lalka")
    lalka.posterUrl.value should startWith("https://kino.ketrzyn.pl/img/asset/")
    lalka.trailerUrl shouldBe Some("https://www.youtube.com/watch?v=DmmBAXjbZWk")
    lalka.ageRating shouldBe Some("13+")
    lalka.synopsis.value should startWith("Dzieło Bolesława Prusa")
    film("GORZKIE ŚWIĘTA - KINO KONESERA").director shouldBe Seq("Pedro Almodóvar")
  }

  it should "carry the page's language version onto every showtime" in {
    all(film("LUNA I ROZGADANA ŚWINKA").showtimes.map(_.format)) should contain("DUB")
    all(film("GORZKIE ŚWIĘTA - KINO KONESERA").showtimes.map(_.format)) should contain("NAP")
    all(film("LALKA").showtimes.map(_.format)) shouldBe empty
  }

  it should "propagate a fetch failure instead of reporting an empty (white) scrape" in {
    a[HttpStatusException] should be thrownBy
      new KinoGwiazdaKetrzynClient(new FailingHttpFetch(503), KinoGwiazdaKetrzyn).fetch()
  }

  // One film page that failed to load threw the whole scrape, turning the venue red over one film.
  it should "drop only a film whose page fails to load, keeping the rest" in {
    val replay  = new FakeHttpFetch("kino-gwiazda-ketrzyn")
    val lalka   = film("LALKA").filmUrl.value
    val oneDown = new tools.HttpFetch {
      def get(url: String): String = if (url == lalka) throw new java.io.IOException("reset") else replay.get(url)
      def post(url: String, body: String, contentType: String): String = replay.post(url, body, contentType)
    }
    val partial = new KinoGwiazdaKetrzynClient(oneDown).fetch()
    partial.map(_.movie.title) should contain theSameElementsAs movies.map(_.movie.title).filterNot(_ == "LALKA")
  }
}
