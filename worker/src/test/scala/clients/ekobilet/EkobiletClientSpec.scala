package clients.ekobilet

import clients.tools.FakeHttpFetch
import models.{KinoJaworzyna, KinoMeduza}
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.EkobiletClient

import java.time.{LocalDate, LocalDateTime}

/** Replays the recorded ekobilet.pl landing + per-film detail pages for Kino
 *  Meduza (Opole) through the client, proving the two-fetch path recovers dated
 *  showtimes that live only on the detail pages. `today` is pinned to the
 *  fixture capture date so the year-inference resolves into June 2026.
 *
 *  Kino Meduza was previously scraped from Filmweb. */
class EkobiletClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val movies =
    new EkobiletClient(new FakeHttpFetch("kino-meduza"), "opolskielamy", KinoMeduza,
      today = LocalDate.of(2026, 6, 8)).fetch()

  "EkobiletClient" should "return a non-empty, single-cinema film list" in {
    movies should not be empty
    movies.map(_.cinema).toSet shouldBe Set(KinoMeduza)
    all(movies.map(_.showtimes)) should not be empty
  }

  it should "pin a concrete screening read off the film detail page" in {
    val film = movies.find(_.movie.title == "Młode matki").value
    film.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 10, 18, 0))
    film.showtimes.flatMap(_.bookingUrl).head should startWith("https://ekobilet.pl/")
  }

  it should "strip format tags from titles (no '2D napisy' suffix)" in {
    movies.map(_.movie.title).foreach(_.toLowerCase should not include "2d napisy")
  }

  /** Kino Jaworzyna captured on a day the venue was dark (11.06.2026): the bare
   *  landing renders "Brak wydarzeń na dzisiaj" with zero `event-card`s, so the
   *  films live only behind the date strip's `?date=` pages. Before the per-day
   *  sweep this returned an empty list; now it recovers the full repertoire. */
  private val jaworzyna =
    new EkobiletClient(new FakeHttpFetch("ekobilet-jaworzyna"), "kino-jaworzyna", KinoJaworzyna,
      today = LocalDate.of(2026, 6, 11)).fetch()

  it should "sweep the date strip when today's landing is empty" in {
    // The bare landing fixture is the live "Brak wydarzeń na dzisiaj" page with
    // zero event-cards; before the date-strip sweep this returned an empty list.
    jaworzyna should not be empty
    jaworzyna.map(_.cinema).toSet shouldBe Set(KinoJaworzyna)
    all(jaworzyna.map(_.showtimes)) should not be empty
  }

  it should "pin a future-day screening discovered only via the date strip" in {
    val film = jaworzyna.find(_.movie.title == "Milczenie owiec").value
    film.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 12, 18, 20))
    film.showtimes.flatMap(_.bookingUrl).head should startWith("https://ekobilet.pl/")
  }
}
