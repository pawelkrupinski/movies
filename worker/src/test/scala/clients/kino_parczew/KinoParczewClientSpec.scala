package clients.kino_parczew

import org.scalatest.OptionValues
import clients.tools.FakeHttpFetch
import models.KinoParczew
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas.pl.KinoParczewClient

import java.time.LocalDateTime

/** Replays the recorded `kinoparczew.pl` homepage grid plus two of its
 *  `/filmy/<slug>/` detail pages (Elementor, captured 2026-09-23) through the
 *  client. The homepage links to more films than are fixed here — those
 *  fetches 404 against the fake and are silently dropped, same as any other
 *  ParallelDetailFetch-based client's partial fixture coverage. */
class ParczewClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val movies = new KinoParczewClient(new FakeHttpFetch("kino-parczew")).fetch()

  "KinoParczewClient" should "return a non-empty, single-cinema film list" in {
    movies should not be empty
    movies.map(_.cinema).toSet shouldBe Set(KinoParczew)
    all(movies.map(_.showtimes)) should not be empty
  }

  it should "expand every 'Codziennie od X do Y' run on a film's own page into daily showtimes, with its full detail" in {
    val film = movies.find(_.movie.title == "Lalka").value
    val dates = film.showtimes.map(_.dateTime)
    dates should contain allOf (
      LocalDateTime.of(2026, 10, 2, 16, 0), LocalDateTime.of(2026, 10, 2, 19, 0),
      LocalDateTime.of(2026, 10, 9, 17, 0), LocalDateTime.of(2026, 10, 29, 17, 0)
    )
    all(film.showtimes.map(_.format)) shouldBe List("2D")
    film.movie.runtimeMinutes.value shouldBe 160
    film.movie.genres shouldBe Seq("Dramat obyczajowy", "kostiumowy")
    film.movie.countries shouldBe Seq("Polska")
    film.director shouldBe Seq("Maciej Kowalski")
    film.synopsis.value should include("Wokulski")
    film.posterUrl.value should include("kinoparczew.pl/wp-content/uploads")
    film.filmUrl.value shouldBe "https://kinoparczew.pl/filmy/lalka/"
  }

  it should "sentence-case a shouted title and union a single run's two showtimes" in {
    val film = movies.find(_.movie.title == "100 dni:misja zeus").value
    val dates = film.showtimes.map(_.dateTime)
    dates should have size 14
    dates should contain allOf (LocalDateTime.of(2026, 9, 18, 19, 0), LocalDateTime.of(2026, 10, 1, 19, 0))
    film.movie.genres shouldBe Seq("Akcja", "Komedia")
    film.director shouldBe Seq("Mikołaj Piszczan")
  }
}
