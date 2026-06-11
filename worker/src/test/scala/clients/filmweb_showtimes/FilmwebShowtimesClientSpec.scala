package clients.filmweb_showtimes

import clients.tools.FakeHttpFetch
import models.{KinoTatry, Multikino}
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.FilmwebShowtimesClient

import java.time.{LocalDate, LocalDateTime}

/** Replays a recorded Filmweb JSON-API capture (Poznań Multikino, cinemaId 633,
 *  date 2026-06-07) through the client. The capture is the single seances page
 *  for that date plus a `/title/{id}/info` per film on it; with `daysAhead = 0`
 *  and `today` pinned to the capture date the client requests exactly that one
 *  page, so the fixture set is bounded.
 *
 *  Asserts the JSON-API path the old `FilmwebClient.fetch()` used still
 *  resolves: films assembled per id, every row tagged with the injected cinema,
 *  showtimes carrying the right `LocalDateTime`s, the per-hour `orderLinks`
 *  becoming `bookingUrl`s, and the `dubbing`/`subtitles` flag surfacing as a
 *  DUB/NAP format token. Fixtures recorded via the seances + title/info
 *  endpoints under www.filmweb.pl/api/v1. */
class FilmwebShowtimesClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val captureDate = LocalDate.of(2026, 6, 7)
  private val http        = new FakeHttpFetch("filmweb-showtimes")
  private val client      = new FilmwebShowtimesClient(http, 633, Multikino, daysAhead = 0, today = captureDate)

  "FilmwebShowtimesClient" should "assemble films from the seances + title/info JSON API" in {
    val movies = client.fetch()

    movies should not be empty
    movies.map(_.cinema).toSet shouldBe Set(Multikino)

    // Every film carries at least one showtime, all on the capture date, and a
    // canonical Filmweb film URL + externalId.
    all(movies.map(_.showtimes)) should not be empty
    movies.flatMap(_.showtimes).map(_.dateTime.toLocalDate).toSet shouldBe Set(captureDate)
    all(movies.map(_.filmUrl.value)) should startWith("https://www.filmweb.pl/film/")
    all(movies.map(_.externalIds.keySet)) should contain("filmweb")
  }

  it should "carry booking links from orderLinks and format tokens from the version flag" in {
    val movies    = client.fetch()
    val showtimes = movies.flatMap(_.showtimes)

    showtimes.flatMap(_.bookingUrl)
      .exists(_.startsWith("https://www.multikino.pl/rezerwacja-biletow/")) shouldBe true
    showtimes.flatMap(_.format).toSet should contain allOf ("DUB", "NAP")
  }

  it should "pin a concrete (title, dateTime) from the capture" in {
    val movies = client.fetch()

    // Filmweb id 582 → "Kosmiczny mecz" (Space Jam), screened 13:00, dubbed (DUB),
    // with a Multikino booking deep-link.
    val spaceJam = movies.find(_.movie.title == "Kosmiczny mecz").value
    spaceJam.cinema shouldBe Multikino
    spaceJam.externalIds("filmweb") shouldBe "582"

    val thirteen = spaceJam.showtimes.find(_.dateTime == LocalDateTime.of(2026, 6, 7, 13, 0)).value
    thirteen.format should contain("DUB")
    thirteen.bookingUrl.value should startWith("https://www.multikino.pl/rezerwacja-biletow/")
  }

  // ── /uptime source link ──────────────────────────────────────────────────
  // The cinema name on /uptime links to Filmweb's canonical, browser-renderable
  // showtimes page. Only the numeric id is in our model, so the city + name are
  // resolved from `/cinema/<id>/info` at boot and slugged the way Filmweb does.

  "canonicalSourceUrl" should "build Filmweb's exact canonical URL (encoded city/name + id)" in {
    // Matches the live page byte-for-byte (ń→%C5%84, space→+).
    FilmwebShowtimesClient.canonicalSourceUrl("Gdyńskie Centrum Filmowe", "Gdynia", 1904) shouldBe
      "https://www.filmweb.pl/showtimes/Gdynia/Gdy%C5%84skie+Centrum+Filmowe-1904"
  }

  "resolveSourceUrl" should "resolve the canonical link from the venue's /info endpoint" in {
    // Replays the recorded /api/v1/cinema/633/info ({name:"Multikino Stary Browar", city:"Poznań"}).
    client.resolveSourceUrl().value shouldBe
      "https://www.filmweb.pl/showtimes/Pozna%C5%84/Multikino+Stary+Browar-633"
  }

  it should "yield None (keeping the /cinema/-<id> fallback) when /info fails" in {
    // cinemaId 4242 has no recorded /info fixture → the fetch throws → tolerant None.
    val noInfo = new FilmwebShowtimesClient(http, 4242, KinoTatry, daysAhead = 0, today = captureDate)
    noInfo.resolveSourceUrl() shouldBe None
  }

  "resolveAll" should "map each resolvable venue to its canonical link, dropping failures" in {
    val ok   = new FilmwebShowtimesClient(http, 633, Multikino, daysAhead = 0, today = captureDate)
    val fail = new FilmwebShowtimesClient(http, 4242, KinoTatry, daysAhead = 0, today = captureDate)
    val urls = FilmwebShowtimesClient.resolveAll(Seq(ok, fail))
    urls("Multikino Stary Browar") shouldBe "https://www.filmweb.pl/showtimes/Pozna%C5%84/Multikino+Stary+Browar-633"
    urls.keySet should not contain "Kino Tatry"
  }
}
