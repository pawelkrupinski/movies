package clients.kino_promien

import models.KinoPromien
import org.scalatest.OptionValues
import clients.tools.FakeHttpFetch
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas.pl.KinoPromienClient

import java.time.{LocalDate, LocalDateTime}

/** Replays the recorded `kinotuchow.pl/repertuar/` listing + per-film
 *  `/movie/<slug>/` detail pages for Kino Promień (Dom Kultury, Tuchów)
 *  through the client. The schedule is Polish free text under a `Seanse:`
 *  heading with no year and day-ranges (`19 – 23 czerwca godz. 10:00, 15:00`),
 *  so the client takes a fixed `today` — pinned here to 2026-06-16, the day the
 *  fixtures were recorded — to infer the year and drop already-played dates. */
class KinoPromienClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val today  = LocalDate.of(2026, 6, 16)
  private val movies = new KinoPromienClient(new FakeHttpFetch("kino-promien"), today = today).fetch()

  "KinoPromienClient" should "return a non-empty, single-cinema film list with showtimes" in {
    movies should not be empty
    movies.map(_.cinema).toSet shouldBe Set(KinoPromien)
    all(movies.map(_.showtimes)) should not be empty
  }

  it should "pin a concrete film title and an expanded day-range showtime" in {
    // "19 – 23 czerwca godz. 10:00*, 15:00, 17:00" expands to a screening on
    // each day of the range × each time, with the year inferred from `today`.
    val film = movies.find(_.movie.title == "Toy Story 5").value
    film.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 19, 15, 0))
  }

  it should "drop screenings that already played before `today`" in {
    // Michael's only listed dates are 12 + 14–15 czerwca, all before 2026-06-16,
    // so it carries no upcoming screening and falls out of the list.
    movies.exists(_.movie.title == "Michael") shouldBe false
  }
}
