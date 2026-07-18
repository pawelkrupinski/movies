package services.cinemas

import org.scalatest.matchers.should.Matchers
import models.{CinemaMovie, Movie, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas.pl.HeliosClient

import java.time.LocalDateTime

/** Regression for the recurring "(N new)" churn on Helios. A live scrape lists
 *  some films twice in one fetch under titles that the cache's normalisation
 *  treats as one row; each raw entry then overwrites the single slot and is
 *  flagged `isNew` on every pass. `mergeDuplicateFilms` collapses them so the
 *  cache sees one entry per film. Shapes below are taken verbatim from a real
 *  Helios Posnania scrape (HeliosChurnProbe). */
class HeliosClientMergeDuplicatesSpec extends AnyFlatSpec with Matchers {

  // `fetch()` is never called, so the default real HttpFetch never hits the
  // network — the constructor does no I/O.
  private val client = new HeliosClient()

  private def st(dt: String) = Showtime(LocalDateTime.parse(dt), bookingUrl = None)
  private def cm(title: String, year: Option[Int], filmUrl: Option[String], times: String*): CinemaMovie =
    CinemaMovie(
      movie     = Movie(title = title, releaseYear = year),
      cinema    = client.cinema,
      posterUrl = None,
      filmUrl   = filmUrl,
      synopsis  = None,
      cast      = Seq.empty,
      director  = Seq.empty,
      showtimes = times.map(st)
    )

  "mergeDuplicateFilms" should "collapse a /wydarzenie event page and the /filmy film page that differ only in case" in {
    val event = cm("Drzewo Magii", None,       Some("https://helios.pl/poznan/kino-helios/wydarzenie/drzewo-magii-knt-2582"), "2026-06-07T10:00")
    val film  = cm("Drzewo magii", Some(2026), Some("https://helios.pl/poznan/kino-helios/filmy/drzewo-magii-4413"),         "2026-06-07T16:00")

    val merged = client.mergeDuplicateFilms(Seq(event, film))

    merged should have size 1
    val m = merged.head
    m.movie.title       shouldBe "Drzewo magii"     // canonical /filmy spelling wins
    m.movie.releaseYear shouldBe Some(2026)         // year filled from the film page
    m.filmUrl.get         should include ("/filmy/")
    m.showtimes.map(_.dateTime.toString) should contain allOf ("2026-06-07T10:00", "2026-06-07T16:00")
  }

  it should "collapse a REST-only entry whose title carries a trailing space onto the trimmed film entry" in {
    val film     = cm("Straszny film",  None,       Some("https://helios.pl/poznan/kino-helios/filmy/straszny-film-4465"), "2026-06-07T18:00")
    val restOnly = cm("Straszny film ", Some(2026), None,                                                                  "2026-06-07T20:00")

    val merged = client.mergeDuplicateFilms(Seq(film, restOnly))

    merged should have size 1
    val m = merged.head
    m.movie.title       shouldBe "Straszny film"   // trailing space trimmed
    m.movie.releaseYear shouldBe Some(2026)        // year filled from the REST entry
    m.showtimes          should have size 2
  }

  it should "leave genuinely distinct films untouched" in {
    val a = cm("Drzewo magii",  Some(2026), Some("https://helios.pl/poznan/kino-helios/filmy/drzewo-magii-4413"),  "2026-06-07T16:00")
    val b = cm("Straszny film", Some(2026), Some("https://helios.pl/poznan/kino-helios/filmy/straszny-film-4465"), "2026-06-07T18:00")

    client.mergeDuplicateFilms(Seq(a, b)) should have size 2
  }
}
