package clients.kino_nowa_fala

import org.scalatest.OptionValues
import clients.tools.FakeHttpFetch
import org.scalatest.matchers.should.Matchers
import models.KinoNowaFalaGizycko
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas.pl.KinoNowaFalaClient

import java.time.LocalDateTime

/** Replays the recorded `kino.gizycko.pl/repertuar/` WP Theatre listing for
 *  Kino Nowa Fala (Giżycko) through the client.
 *
 *  Newly onboarded (2026-09-23 sweep) — no prior source scraped this venue. */
class KinoNowaFalaClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val movies = new KinoNowaFalaClient(new FakeHttpFetch("kino-nowa-fala")).fetch()

  "KinoNowaFalaClient" should "return a non-empty, single-cinema film list" in {
    movies should not be empty
    movies.map(_.cinema).toSet shouldBe Set(KinoNowaFalaGizycko)
    all(movies.map(_.showtimes)) should not be empty
  }

  it should "parse a film's title, showtime and booking link" in {
    val film = movies.find(_.movie.title.toLowerCase.contains("totalna magia")).value
    film.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 9, 25, 17, 55))
    film.showtimes.flatMap(_.bookingUrl).head should include("bilety.gizycko.pl")
  }

  it should "carry the venue's own production page as the film URL and a poster" in {
    val film = movies.find(_.movie.title.toLowerCase.contains("lalka")).value
    film.filmUrl.value should include("kino.gizycko.pl/production/")
    film.posterUrl should not be empty
  }

  it should "merge repeated screenings of the same film across days into one row" in {
    // "TEDI I MAGICZNA LAMPA" screens 25/26/27 września at several times — all
    // must fold onto the same film's showtimes, not one row per day.
    val film = movies.find(_.movie.title.toLowerCase.contains("tedi")).value
    film.showtimes.map(_.dateTime) should contain allOf (
      LocalDateTime.of(2026, 9, 25, 16, 0),
      LocalDateTime.of(2026, 9, 26, 14, 0),
      LocalDateTime.of(2026, 9, 27, 16, 0)
    )
  }
}
