package clients.mcsw_elektrownia

import org.scalatest.OptionValues
import models.McswElektrowniaCinema
import clients.tools.FakeHttpFetch
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas.pl.McswElektrowniaCinemaClient
import services.cinemas.common.ScrapeHorizon

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
    // The raw title is 'DRZEWO MAGII, Wlk. Brytania , dubbing, familijny, od 8 lat KS N…'
    // The client must return just 'DRZEWO MAGII'.
    movies.map(_.movie.title) should contain("DRZEWO MAGII")
    movies.map(_.movie.title).foreach { t =>
      // No raw metadata segment (country, genre) should appear after the title
      t should not include "dubbing"
      t should not include "dramat"
    }
  }

  // ── The programme past the first week ────────────────────────────────────
  //
  // The day route answers for any date, and on 2026-08-05 it had four films on
  // both the 13th and the 16th day ahead — past the today+6 window this client
  // used to ask for, so a fortnight of the schedule was invisible. The month
  // route [[services.cinemas.pl.MsiClient]] would use is no help here: it listed
  // only the 5th–11th. So the per-day walk stays, and now follows the programme
  // (`ScrapeHorizon.liveDays`).
  //
  // A stub rather than the recorded corpus: what is under test is which DAYS get
  // asked for, not how a day's HTML parses (the fixtures above cover that).

  private val start = LocalDate.of(2026, 8, 5)

  private def dayHtml(day: LocalDate): String =
    if (day.isAfter(start.plusDays(16))) "<html><body>Brak seansów</body></html>"
    else s"""<div class="js-event-details-filter movies-movie__single">
       |<h3 class="movies-movie__single__title">Jakiś film, Polska, dramat</h3>
       |<li event-filter="1"><a href="/MSI/Default.aspx?event_id=1">18:00</a></li></div>""".stripMargin

  it should "keep walking past the old one-week window" in {
    val asked = scala.collection.mutable.ArrayBuffer.empty[String]
    val stub = new tools.GetOnlyHttpFetch {
      def get(url: String): String = {
        asked += url
        dayHtml(LocalDate.parse("""date=(\d{4}-\d{2}-\d{2})""".r.findFirstMatchIn(url).map(_.group(1)).get))
      }
    }
    val days = new McswElektrowniaCinemaClient(stub, McswElektrowniaCinema, start)
      .fetch().flatMap(_.showtimes).map(_.dateTime.toLocalDate).distinct

    days should contain (start.plusDays(16))
    days.size shouldBe 17
    // The walk ends on the stop rule, not on a guess at the programme's length.
    asked.size shouldBe 17 + ScrapeHorizon.MaxEmptyDays
  }
}
