package clients.kino_kijow

import org.scalatest.OptionValues
import clients.tools.FakeHttpFetch
import models.KinoKijow
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas.pl.KinoKijowClient

import java.time.{LocalDate, LocalDateTime}
import services.movies.SingleCountryNormalizer.titleNormalizer

/** Replays the recorded `kupbilet.kijow.pl/MSI/mvc/pl?sort=Date&date=2026-06`
 *  page (07-06-2026 capture) through the client. The page covers June 2026;
 *  `today` is pinned to the capture date, and the months after it have no
 *  recording — which reads as a portal with nothing more to show, so the walk
 *  ends there. */
class KinoKijowClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val http   = new FakeHttpFetch("kino-kijow")
  private val client = new KinoKijowClient(http, KinoKijow, LocalDate.of(2026, 6, 7), titles = titleNormalizer)

  "KinoKijowClient" should "return a non-empty film list" in {
    client.fetch() should not be empty
  }

  it should "tag every film with KinoKijow" in {
    client.fetch().map(_.cinema).toSet shouldBe Set(KinoKijow)
  }

  it should "give every film at least one showtime" in {
    all(client.fetch().map(_.showtimes)) should not be empty
  }

  it should "pin a concrete screening: Niesamowite przygody skarpetek 3 on 2026-06-07 at 10:30" in {
    val movies   = client.fetch()
    val skarpety = movies.find(_.movie.title.contains("skarpetek")).value
    skarpety.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 7, 10, 30))
    skarpety.showtimes.flatMap(_.bookingUrl).head should include("/MSI/Default.aspx?event_id=")
  }

  // The portal bakes "2D DUBBING" / "2D NAPISY" into the h2 title. Strip it off
  // the title and surface it as a per-showtime format badge instead.
  it should "strip the trailing '2D DUBBING' tag into the showtime format badge" in {
    val movies   = client.fetch()
    val straszny = movies.find(_.movie.title == "Straszny film").value
    straszny.movie.title shouldBe "Straszny film" // not "Straszny film 2D DUBBING"
    straszny.showtimes.flatMap(_.format).toSet should contain allOf ("2D", "DUB")
  }

  // ── The programme past the current month ─────────────────────────────────
  //
  // This portal publishes a long way ahead: measured 2026-08-05, Kijów had
  // screenings in every month through March 2027 — 20 in September, 15 in
  // October, 13 in November — while the scrape asked only for August, since it
  // reached for next month solely inside a month's last fortnight. Roughly
  // fifty-five screenings were invisible. It now walks the months
  // (`ScrapeHorizon.liveMonths`), as the other MSI portals already did.

  private def monthHtml(month: java.time.YearMonth, day: Int): String =
    s"""<div class="cd-timeline-content eventlist">
       |<span class="cd-date">${"%02d".format(day)} ${PolishMonth(month.getMonthValue)} 18:00</span>
       |<h2>${"%02d".format(day)} ${PolishMonth(month.getMonthValue)} 18:00 - Jakiś film</h2>
       |<a class="btn-badge2" href="/MSI/Default.aspx?event_id=1">Kup bilet</a></div>""".stripMargin

  private val PolishMonth = Map(
    1 -> "sty", 2 -> "lut", 3 -> "mar", 4 -> "kwi", 5 -> "maj", 6 -> "cze",
    7 -> "lip", 8 -> "sie", 9 -> "wrz", 10 -> "paź", 11 -> "lis", 12 -> "gru")

  it should "keep walking the months while the portal still has a programme" in {
    val start   = LocalDate.of(2026, 8, 5)
    val through = java.time.YearMonth.of(2026, 11)
    val stub = new tools.GetOnlyHttpFetch {
      def get(url: String): String = {
        val month = java.time.YearMonth.parse("""date=(\d{4}-\d{2})""".r.findFirstMatchIn(url).map(_.group(1)).get)
        if (month.isAfter(through)) "<html></html>" else monthHtml(month, day = 10)
      }
    }
    val dates = new KinoKijowClient(stub, KinoKijow, start, titles = titleNormalizer)
      .fetch().flatMap(_.showtimes).map(_.dateTime.toLocalDate)

    dates should contain (LocalDate.of(2026, 11, 10))
    // October too: its abbreviation "paź" is the one with a diacritic, and the
    // date pattern used to be ASCII-only, so every October screening was dropped.
    dates should contain (LocalDate.of(2026, 10, 10))
    dates.size shouldBe 4   // August through November, one screening each
  }
}
