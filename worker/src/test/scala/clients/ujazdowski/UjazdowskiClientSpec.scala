package clients.ujazdowski

import org.scalatest.matchers.should.Matchers
import clients.tools.FakeHttpFetch
import org.scalatest.flatspec.AnyFlatSpec
import models.{Showtime, Ujazdowski}
import services.cinemas.pl.UjazdowskiClient
import services.cinemas.common.ScrapeHorizon

import java.time.{LocalDate, LocalDateTime}

class UjazdowskiClientSpec extends AnyFlatSpec with Matchers {

  // Pinned to the fixture capture date so the walked days resolve to the
  // recorded `week.ajax?ut=N` files.
  private val today   = LocalDate.of(2026, 6, 13)
  private val client  = new UjazdowskiClient(new FakeHttpFetch("ujazdowski"), today)
  private val results = client.fetch()
  private val byTitle = results.map(cm => cm.movie.title -> cm).toMap

  "UjazdowskiClient.fetch" should "return 12 films and 31 showtimes from the day AJAX pages" in {
    results.size shouldBe 12
    results.flatMap(_.showtimes).size shouldBe 31
  }

  it should "assign Kino U-jazdowski to every entry" in {
    results.map(_.cinema).toSet shouldBe Set(Ujazdowski)
  }

  // Regression for the nav-window-too-short bug: the listing nav stopped at
  // 06-15, but `week.ajax` already served 06-16+. The computed forward window
  // recovers those advance days — Kumotry's 06-16 screening and the film
  // "Sceny z życia małżeńskiego", which only plays 06-16, were FW-only before.
  it should "reach advance days the listing nav omits via the computed window" in {
    byTitle("Kumotry").showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 16, 17, 0))
    byTitle should contain key "Sceny z życia małżeńskiego"
    byTitle("Sceny z życia małżeńskiego").showtimes.map(_.dateTime) shouldBe Seq(LocalDateTime.of(2026, 6, 16, 18, 30))
  }

  it should "parse runtime/year/countries from the meta line and date from the ut timestamp" in {
    val m = byTitle("Erupcja")
    m.movie.runtimeMinutes shouldBe Some(71)
    m.movie.releaseYear    shouldBe Some(2025)
    m.movie.countries      shouldBe Seq("Polska", "USA")
    m.showtimes.size       shouldBe 2
    m.showtimes.head shouldBe
      Showtime(LocalDateTime.of(2026, 6, 11, 18, 30), Some("https://u-jazdowski.pl/kino/repertuar/erupcja"), None, Nil)
  }

  it should "read the bracketed original title off a foreign film, and none off a Polish one" in {
    val foreignDetail = client.fetchFilmDetail(byTitle("Zawieście czerwone latarnie").filmUrl.getOrElse(fail("no filmUrl for Zawieście czerwone latarnie")))
      .getOrElse(fail("no detail for Zawieście czerwone latarnie"))
    foreignDetail.originalTitle shouldBe Some("Da hong deng long gao gao gua")

    val polishDetail = client.fetchFilmDetail(byTitle("Erupcja").filmUrl.getOrElse(fail("no filmUrl for Erupcja")))
      .getOrElse(fail("no detail for Erupcja"))
    polishDetail.originalTitle shouldBe None
  }

  // Regression: some descriptions embed a source/related link as a plain-text
  // URL; it must be stripped from the synopsis. (The exact prod page isn't in
  // the corpus; the structure is reproduced via a synthetic detail page.)
  it should "strip an embedded source URL from the synopsis" in {
    val withUrl = new FakeHttpFetch("ujazdowski") {
      override def get(url: String): String =
        if (url.endsWith("/synthetic"))
          "<html><body><div class='body max-w'>Nowy film dokumentalny o mieście. Źródło: https://osw.org.pl/raport tutaj.</div></body></html>"
        else super.get(url)
    }
    val s = new UjazdowskiClient(withUrl, today).fetchFilmDetail("https://u-jazdowski.pl/synthetic")
      .flatMap(_.synopsis).getOrElse(fail("no synopsis"))
    s should include ("Nowy film dokumentalny o mieście")
    s should not include "http"
    s should not include "osw.org.pl"
  }

  // ── The programme past the first week ────────────────────────────────────
  //
  // `week.ajax` answers for any day, and on 2026-08-05 it was still returning
  // screenings 25 days out while the scrape asked for a computed week — so
  // three weeks of the programme were invisible. The window now follows the
  // programme (`ScrapeHorizon.liveDays`), as it does for Nowe Horyzonty and
  // Kino Mikro.
  //
  // A stub rather than the recorded corpus: what is under test is which DAYS
  // get asked for, not how a day's HTML parses (the fixtures above cover that).

  private val start = LocalDate.of(2026, 8, 5)

  private def dayHtml(hour: Int): String =
    s"""<a class="event-list-day-box" href="/kino/repertuar/jakis-film">
       |<span class="title"><em>Jakiś film</em></span><span class="hours">$hour:00</span></a>""".stripMargin

  /** Serves a programme running `days` days out from `start`, and nothing after. */
  private def stubRunning(days: Int, asked: scala.collection.mutable.ArrayBuffer[String]) =
    new tools.GetOnlyHttpFetch {
      def get(url: String): String = {
        asked += url
        // The nav lists only today, which is the shortfall the walk exists to cover.
        if (!url.contains("week.ajax")) s"""<a href="?ut=${start.atStartOfDay(java.time.ZoneId.of("Europe/Warsaw")).toEpochSecond}">dziś</a>"""
        else {
          val ut  = """ut=(\d+)""".r.findFirstMatchIn(url).map(_.group(1).toLong).getOrElse(0L)
          val day = java.time.Instant.ofEpochSecond(ut).atZone(java.time.ZoneId.of("Europe/Warsaw")).toLocalDate
          if (day.isAfter(start.plusDays(days.toLong))) "<html></html>" else dayHtml(18)
        }
      }
    }

  it should "keep walking past the old one-week window" in {
    val asked = scala.collection.mutable.ArrayBuffer.empty[String]
    val showtimes = new UjazdowskiClient(stubRunning(days = 25, asked), start).fetch().flatMap(_.showtimes)

    showtimes.map(_.dateTime.toLocalDate) should contain (start.plusDays(25))
    showtimes.size shouldBe 26
  }

  it should "stop once the programme runs out, so a dormant venue stays cheap" in {
    val asked = scala.collection.mutable.ArrayBuffer.empty[String]
    new UjazdowskiClient(stubRunning(days = -1, asked), start).fetch() shouldBe empty

    // The listing, then exactly the blank-day probes that end the walk — the
    // nav's one day is shared with the walk's first, so it is fetched once.
    asked.count(_.contains("week.ajax")) shouldBe ScrapeHorizon.MaxEmptyDays
  }
}
