package clients.kino_sck_stargard

import org.scalatest.OptionValues
import clients.tools.FakeHttpFetch
import org.scalatest.matchers.should.Matchers
import models.KinoSCK
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas.pl.SckStargardClient

import java.time.LocalDateTime

/** Replays the recorded `sck.stargard.pl/repertuar-kina/` WordPress (Avada/
 *  Fusion) listing for Kino SCK (Stargardzkie Centrum Kultury, Stargard)
 *  through the client.
 *
 *  Previously scraped via biletyna.pl, which had stopped carrying the film
 *  programme — the schedule now lives only on the venue's own site. */
class SckStargardClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val movies = new SckStargardClient(new FakeHttpFetch("kino-sck-stargard")).fetch()

  "SckStargardClient" should "return a non-empty, single-cinema film list" in {
    movies should not be empty
    movies.map(_.cinema).toSet shouldBe Set(KinoSCK)
    all(movies.map(_.showtimes)) should not be empty
  }

  it should "carry real upcoming titles from the live repertoire" in {
    val titles = movies.map(_.movie.title)
    titles should contain("Toy Story 5")
    titles should contain("Władcy Wszechświata")
    titles should contain("Robin Hood : Koniec legendy")
  }

  it should "pin Toy Story 5's screenings off the full data-seance_link datetime" in {
    val film = movies.find(_.movie.title == "Toy Story 5").value
    film.showtimes.map(_.dateTime) should contain allOf (
      LocalDateTime.of(2026, 6, 23, 12, 0),
      LocalDateTime.of(2026, 6, 23, 14, 0),
      LocalDateTime.of(2026, 6, 23, 16, 0)
    )
  }

  it should "carry the rezerwacje.sck booking link off each showtime" in {
    val film  = movies.find(_.movie.title == "Toy Story 5").value
    val first = film.showtimes.find(_.dateTime == LocalDateTime.of(2026, 6, 23, 12, 0)).value
    first.bookingUrl.value should include("rezerwacje.sck.stargard.pl")
  }

  it should "surface the loose movie-tag format words as showtime tokens" in {
    val film = movies.find(_.movie.title == "Toy Story 5").value
    film.showtimes.head.format should contain allOf ("2D", "DUB")
  }

  it should "drop the coming-soon announcements that have no bookable screening" in {
    // "Minionki i Straszydła" is an "od 01.07.2026" announcement with no
    // data-seance_link anchors, so it must NOT surface as a screening.
    movies.map(_.movie.title) should not contain "Minionki i Straszydła"
  }

  it should "resolve the poster off data-orig-src, not the lazyload placeholder" in {
    val film = movies.find(_.movie.title == "Toy Story 5").value
    film.posterUrl.value should (startWith("https://sck.stargard.pl/wp-content/uploads") and endWith(".jpeg"))
  }
}
