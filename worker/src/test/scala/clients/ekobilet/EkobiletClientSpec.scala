package clients.ekobilet

import org.scalatest.OptionValues
import clients.tools.FakeHttpFetch
import models.{KinoJaworzyna, KinoMeduza}
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas.pl.EkobiletClient

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

  // ── Per-film detail page (deferred enrichment) ─────────────────────────────

  private val jaworzynaClient =
    new EkobiletClient(new FakeHttpFetch("ekobilet-jaworzyna"), "kino-jaworzyna", KinoJaworzyna,
      today = LocalDate.of(2026, 6, 11))

  it should "expose each film's detail page as filmUrl for deferred enrichment" in {
    jaworzyna.flatMap(_.filmUrl) should not be empty
    all(jaworzyna.flatMap(_.filmUrl).map(_.startsWith("https://ekobilet.pl/kino-jaworzyna/"))) shouldBe true
  }

  it should "harvest the synopsis off the detail page reached via filmUrl" in {
    // Take the film's own filmUrl (the ref the listing scrape leaves) and run the
    // deferred fetchFilmDetail against it, exactly as the EnrichDetails task does.
    val ref    = jaworzyna.find(_.movie.title == "Milczenie owiec").value.filmUrl.value
    val detail = jaworzynaClient.fetchFilmDetail(ref).value
    detail.synopsis.value shouldBe
      "Seryjny morderca i inteligentna agentka łączą siły, by znaleźć przestępcę obdzierającego ze skóry swoje ofiary."
    // ekobilet pages carry no other film-level metadata — assert the gap so a
    // future page shape that *does* add it shows up as a failing expectation.
    detail.releaseYear shouldBe None
    detail.director    shouldBe empty
    detail.cast        shouldBe empty
    detail.countries   shouldBe empty
  }

  it should "read the film synopsis only, not the venue's own about-the-cinema blurb" in {
    val ref    = jaworzyna.find(_.movie.title == "Romeria").value.filmUrl.value
    val detail = jaworzynaClient.fetchFilmDetail(ref).value
    detail.synopsis.value should startWith("Marina wraca do rodzinnej Galicji")
    detail.synopsis.value should not include "Małopolskiej Sieci Kin Cyfrowych" // the venue blurb
  }

  it should "return None when the detail-page fetch fails (no fixture)" in {
    jaworzynaClient.fetchFilmDetail("https://ekobilet.pl/kino-jaworzyna/nie-ma-takiego-filmu-99999") shouldBe None
  }
}
