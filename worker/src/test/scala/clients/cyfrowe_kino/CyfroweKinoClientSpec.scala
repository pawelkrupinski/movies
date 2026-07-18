package clients.cyfrowe_kino

import models.KinoCyfroweKino
import org.scalatest.OptionValues
import clients.tools.FakeHttpFetch
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas.pl.CyfroweKinoClient

import java.time.LocalDateTime

/** Replays the recorded `dksrodaslaska.pl/aktualny-repertuar/` WordPress
 *  listing for Cyfrowe Kino (Środa Śląska) through the client.
 *
 *  Cyfrowe Kino was previously scraped from Filmweb, whose API had silently
 *  gone empty for it (every poll returned `[]`) though the cinema is open —
 *  this fixture is the proof its programme is real and reachable on its own
 *  site. */
class CyfroweKinoClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val movies = new CyfroweKinoClient(new FakeHttpFetch("cyfrowe-kino")).fetch()

  "CyfroweKinoClient" should "return a non-empty, single-cinema film list" in {
    movies should not be empty
    movies.map(_.cinema).toSet shouldBe Set(KinoCyfroweKino)
    all(movies.map(_.showtimes)) should not be empty
  }

  it should "pin a concrete screening with the date read off the label" in {
    // Fixture: "STRASZNY FILM / dubbing i napisy" screens 2026-06-12 at 19:00.
    val film = movies.find(_.movie.title.toLowerCase.contains("straszny")).value
    film.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 12, 19, 0))
  }

  it should "clean the dirty title (drop the leading date, DKF tag and version)" in {
    // Raw title in the fixture: "15.06 / DKF WERDYKT / napisy" → "Werdykt".
    val film = movies.find(_.movie.title.toLowerCase.contains("werdykt")).value
    film.movie.title should not include "/"
    film.movie.title should not include "15.06"
    film.movie.title.toLowerCase should not include "napisy"
    film.movie.title.toLowerCase should not include "dkf"
  }

  it should "carry a poster for every film" in {
    all(movies.map(_.posterUrl)) shouldBe defined
  }
}
