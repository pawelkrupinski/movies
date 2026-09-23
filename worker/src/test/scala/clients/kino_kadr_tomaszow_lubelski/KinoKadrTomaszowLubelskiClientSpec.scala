package clients.kino_kadr_tomaszow_lubelski

import org.scalatest.OptionValues
import clients.tools.FakeHttpFetch
import models.KinoKadrTomaszowLubelski
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas.pl.KinoKadrTomaszowLubelskiClient

import java.time.LocalDate

/** Replays the recorded `kinokadr.pl/repertuar/` nonce page plus the
 *  `wp-admin/admin-ajax.php` per-day responses (captured 2026-09-23) through
 *  the client. Only the four LIVE dates (30 Sep, 1, 3, 4 Oct 2026) needed a
 *  real fixture — `planChunks`'s day-walk treats every unrecorded day as
 *  blank (see `FakeHttpFetch`'s missing-fixture-as-404 fallback), which
 *  matches this venue exactly: every other day in the window really is
 *  "brak seansów w tym dniu". */
class KinoKadrTomaszowLubelskiClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val today  = LocalDate.of(2026, 9, 23)
  private val movies = new KinoKadrTomaszowLubelskiClient(
    new FakeHttpFetch("kino-kadr-tomaszow-lubelski"), today = today
  ).fetch()

  "KinoKadrTomaszowLubelskiClient" should "return a non-empty, single-cinema film list" in {
    movies should not be empty
    movies.map(_.cinema).toSet shouldBe Set(KinoKadrTomaszowLubelski)
    all(movies.map(_.showtimes)) should not be empty
  }

  it should "list the venue's fixed weekly rota of three films" in {
    movies.map(_.movie.title) should contain allOf ("Mistyczka", "100 dni: Misja Zeus", "Lalka")
  }

  it should "merge the same film's showtimes across every live day into one row" in {
    val film = movies.find(_.movie.title == "Mistyczka").value
    film.showtimes.map(_.dateTime.toLocalDate) should contain allOf (
      LocalDate.of(2026, 9, 30), LocalDate.of(2026, 10, 1), LocalDate.of(2026, 10, 3), LocalDate.of(2026, 10, 4)
    )
    all(film.showtimes.map(_.dateTime.toLocalTime.toString)) shouldBe "14:50"
  }

  it should "read the runtime and genres off the movie-info/movie-extra spans" in {
    val film = movies.find(_.movie.title == "Mistyczka").value
    film.movie.runtimeMinutes.value shouldBe 90
    film.movie.genres should contain allOf ("Religijny", "Biograficzny", "Dramat")
  }

  it should "read the dub badge off the movie-audio span" in {
    val film = movies.find(_.movie.title == "Mistyczka").value
    all(film.showtimes.map(_.format)) shouldBe List("DUB")
  }

  it should "carry the trailer link, despite the ticket-container class name holding no ticket link" in {
    val film = movies.find(_.movie.title == "Mistyczka").value
    film.trailerUrl.value should include("youtube.com/watch?v=RG_hCZBwcGU")
    all(film.showtimes.map(_.bookingUrl)) shouldBe None
  }
}
