package clients.kino_jednosc

import clients.tools.FakeHttpFetch
import models.KinoJednosc
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.KinoJednoscClient

import java.time.LocalDateTime

/** Replays the recorded `kinosedziszow.pl` listing + per-film detail pages for
 *  Kino Jedność (Sędziszów Małopolski) through the client — proving the
 *  two-fetch path (listing → detail) recovers discrete dated showtimes that
 *  live only on the detail pages.
 *
 *  Previously scraped from Filmweb, which had silently gone empty for it. */
class KinoJednoscClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val movies = new KinoJednoscClient(new FakeHttpFetch("kino-jednosc")).fetch()

  "KinoJednoscClient" should "return a non-empty, single-cinema film list" in {
    movies should not be empty
    movies.map(_.cinema).toSet shouldBe Set(KinoJednosc)
    all(movies.map(_.showtimes)) should not be empty
  }

  it should "pin a concrete screening read off the film's detail page" in {
    val film = movies.find(_.movie.title.toLowerCase.contains("władcy wszechświata")).value
    film.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 12, 17, 15))
  }
}
