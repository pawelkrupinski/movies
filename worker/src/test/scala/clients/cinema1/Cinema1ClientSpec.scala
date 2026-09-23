package clients.cinema1

import models.Cinema1Gdansk
import clients.tools.{FailingHttpFetch, FakeHttpFetch}
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas.pl.Cinema1Client
import tools.HttpStatusException

import java.time.LocalDate

/**
 * Cinema1 migrated onto the "POSitive Cinema" Angular SPA platform in 2026,
 * whose server returns a client-rendered shell (see `Cinema1Client`'s
 * scaladoc) — nothing for an HTML scraper to read. This spec exercises the
 * replacement JSON-API client against the real `restapi.cinemaone.pl`
 * responses recorded 2026-09-13 (`WriteCinema1`), pinning `today` to that same
 * date so the fixture's query-fingerprinted screening file still matches.
 */
class Cinema1ClientSpec extends AnyFlatSpec with Matchers {

  private val CinemaId = "8d3b10d9-f892-4f57-bf74-9f86905ce3ea"
  private val client   = new Cinema1Client(new FakeHttpFetch("cinema1-gdansk"), Cinema1Gdansk,
    cinemaId = CinemaId, today = LocalDate.of(2026, 9, 13))
  private val results  = client.fetch()
  private val byTitle  = results.map(cm => cm.movie.title -> cm).toMap

  "Cinema1Client.fetch" should "return 19 films and 185 showtimes" in {
    results.size shouldBe 19
    results.flatMap(_.showtimes).size shouldBe 185
  }

  it should "assign Cinema1Gdansk to every entry" in {
    results.map(_.cinema).toSet shouldBe Set(Cinema1Gdansk)
  }

  it should "read title, runtime, year, countries, genres, poster, cast and director off /movie/{id}" in {
    val m = byTitle("MARSUPILAMI")
    m.movie.runtimeMinutes shouldBe Some(99)
    m.movie.releaseYear    shouldBe Some(2026)
    m.movie.countries      shouldBe Seq("France")
    m.movie.genres         shouldBe Seq("Familijny", "Komedia")
    m.posterUrl            shouldBe Some("https://medstore.cinemaone.pl/MARSUPILAMIplakatB1net.jpg")
    m.cast                 shouldBe Seq("Gérard Jugnot", "Didier Bourdon", "Paco Boisson")
  }

  it should "split a multi-country co-production into one Movie.countries entry per country" in {
    byTitle("NIEBO NAD NORMANDIĄ").movie.countries shouldBe Seq("UK", "France", "The United States of America")
  }

  it should "carry a showtime's room (off /screenhead), booking link and dateTime" in {
    val show = byTitle("MARSUPILAMI").showtimes.head
    show.dateTime   shouldBe java.time.LocalDateTime.of(2026, 9, 13, 11, 30)
    show.bookingUrl shouldBe Some(
      s"https://bilety.cinemaone.pl/pl/screen?screeningId=c543fede-e245-4a0f-a155-bb9514b61bf4&cinemaId=$CinemaId")
    show.room shouldBe Some("Sala 3")
  }

  it should "map speakingType DUB/NAPISY to the DUB/NAP format badge, and omit the unremarkable 2D printType" in {
    byTitle("MARSUPILAMI").showtimes.head.format shouldBe List("DUB")
    byTitle("ODYSEJA").showtimes.head.format     shouldBe List("NAP")
  }

  it should "drop the Polish 'no restriction' rating code (BO) to None, and keep a real one verbatim" in {
    byTitle("BASIA. MAM SWÓJ ŚWIAT").ageRating shouldBe None
    byTitle("ODYSEJA").ageRating               shouldBe Some("15")
  }

  it should "leave director empty rather than guessing when the source field is blank" in {
    byTitle("MARSUPILAMI").director shouldBe Seq.empty
  }

  // The screenings listing is the whole scrape: a failed fetch of it must surface
  // red on /uptime with its cause, not be swallowed into an empty list that reads
  // as a successful "0 showtimes" scrape (white, indistinguishable from a dormant
  // venue — and the exact symptom the SPA migration itself produced).
  it should "propagate a failed screenings fetch instead of swallowing it into an empty (white) scrape" in {
    val failing = new Cinema1Client(new FailingHttpFetch(503), Cinema1Gdansk, cinemaId = CinemaId, today = LocalDate.of(2026, 9, 13))
    a[HttpStatusException] should be thrownBy failing.fetch()
  }
}
