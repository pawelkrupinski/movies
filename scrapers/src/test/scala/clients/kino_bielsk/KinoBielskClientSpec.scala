package clients.kino_bielsk

import org.scalatest.OptionValues
import clients.tools.FakeHttpFetch
import models.KinoBielsk
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas.pl.KinoBielskClient

import java.time.LocalDateTime

/** Replays the recorded `bdkbielsk.pl/kino/` calendar (a bespoke w3.css page,
 *  captured 2026-09-23) through the client. The venue mixes its own daily
 *  commercial repertoire with a free monthly "Klub Filmowy 'Kino Znicz'"
 *  classics slot — both are real, dated film screenings. */
class BielskDomKulturyClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val movies = new KinoBielskClient(new FakeHttpFetch("kino-bielsk")).fetch()

  "KinoBielskClient" should "return a non-empty, single-cinema film list" in {
    movies should not be empty
    movies.map(_.cinema).toSet shouldBe Set(KinoBielsk)
    all(movies.map(_.showtimes)) should not be empty
  }

  it should "convert the calendar link's UTC 'dates=' timestamp to Warsaw wall-clock time, with the year the day header lacks" in {
    val film = movies.find(_.movie.title == "Sekretne życie Waltera Mitty").value
    film.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 9, 28, 11, 0))
    film.movie.releaseYear.value shouldBe 2013
  }

  it should "read the venue's own commercial-repertoire row (title up to the first comma)" in {
    val film = movies.find(_.movie.title == "Lalka").value
    film.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 10, 7, 16, 30))
    film.movie.releaseYear.value shouldBe 2026
  }

  it should "drop a closed session reserved for a school group, not open to the public" in {
    movies.flatMap(_.showtimes.map(_.dateTime)) should not contain LocalDateTime.of(2026, 10, 7, 8, 0)
  }
}
