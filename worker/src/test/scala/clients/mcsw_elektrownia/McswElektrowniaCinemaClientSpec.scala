package clients.mcsw_elektrownia

import clients.tools.FakeHttpFetch
import models.McswElektrowniaCinema
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.McswElektrowniaCinemaClient

import java.time.{LocalDate, LocalDateTime}

/** Replays the recorded day page (07-06-2026 capture) through the client.
 *  The client fetches seven day pages in parallel; the fixture only covers
 *  June 7 — the other six days' fetches throw in FakeHttpFetch and are
 *  tolerantly dropped (Try → None), leaving the June 7 screenings.
 *  `today` is pinned to 2026-06-07 so the June 7 URL is the first day fetched,
 *  which is the only one the fixture covers. */
class McswElektrowniaCinemaClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val http   = new FakeHttpFetch("mcsw-elektrownia")
  private val client = new McswElektrowniaCinemaClient(http, McswElektrowniaCinema, LocalDate.of(2026, 6, 7))

  "McswElektrowniaCinemaClient" should "return a non-empty film list" in {
    val movies = client.fetch()
    movies should not be empty
  }

  it should "tag every film with McswElektrowniaCinema" in {
    val movies = client.fetch()
    movies.map(_.cinema).toSet shouldBe Set(McswElektrowniaCinema)
  }

  it should "give every film at least one showtime" in {
    val movies = client.fetch()
    all(movies.map(_.showtimes)) should not be empty
  }

  it should "pin a concrete screening: DRZEWO MAGII on 2026-06-07 at 14:15" in {
    // On the 07-06-2026 fixture page, DRZEWO MAGII screens at 14:15.
    val movies    = client.fetch()
    val drzewo    = movies.find(_.movie.title == "DRZEWO MAGII").value
    drzewo.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 7, 14, 15))
  }

  it should "include booking URLs on showtimes" in {
    val movies = client.fetch()
    // Every showtime has a booking URL pointing to the MSI Default.aspx page.
    all(movies.flatMap(_.showtimes).map(_.bookingUrl)) should not be empty
    movies.flatMap(_.showtimes).flatMap(_.bookingUrl).foreach { url =>
      url should include("mcswelektrownia.pl")
      url should include("event_id=")
    }
  }

  it should "strip metadata from MSI composite titles" in {
    val movies = client.fetch()
    // The raw h2 is 'DRZEWO MAGII, Wlk. Brytania , dubbing, familijny, od 8 lat KS N…'
    // The client must return just 'DRZEWO MAGII'.
    movies.map(_.movie.title) should contain("DRZEWO MAGII")
    movies.map(_.movie.title).foreach { t =>
      // No raw metadata segment (country, genre) should appear after the title
      t should not include "dubbing"
      t should not include "dramat"
    }
  }
}
