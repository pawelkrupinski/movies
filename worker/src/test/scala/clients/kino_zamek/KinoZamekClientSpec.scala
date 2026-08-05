package clients.kino_zamek

import org.scalatest.OptionValues
import clients.tools.FakeHttpFetch
import models.KinoZamekSzczecin
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas.pl.KinoZamekClient

import java.time.{LocalDate, LocalDateTime, YearMonth}

/** Replays the recorded `bilety.zamek.szczecin.pl/MSI/mvc/pl?sort=Name&date=2026-06`
 *  (showtime data) and `zamek.szczecin.pl/wydarzenia/kino/` (film allow-list)
 *  fixtures through the client.  `today` is pinned to 2026-06-07. */
class KinoZamekClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val http   = new FakeHttpFetch("kino-zamek")
  private val client = new KinoZamekClient(http, KinoZamekSzczecin, today = LocalDate.of(2026, 6, 7))

  "KinoZamekClient" should "return a non-empty film list" in {
    val movies = client.fetch()
    movies should not be empty
  }

  it should "tag every film with the supplied cinema" in {
    val movies = client.fetch()
    movies.map(_.cinema).toSet shouldBe Set(KinoZamekSzczecin)
  }

  it should "give every film at least one showtime" in {
    val movies = client.fetch()
    all(movies.map(_.showtimes)) should not be empty
  }

  it should "exclude non-film events (concerts, workshops)" in {
    val movies = client.fetch()
    val titles = movies.map(_.movie.title.toLowerCase)
    // These are concerts / workshops in the MSI but absent from the kino listing:
    titles.filter(_.contains("koncert")) shouldBe empty
    titles.filter(_.contains("disco"))   shouldBe empty
    titles.filter(_.contains("joga"))    shouldBe empty
  }

  it should "pin a concrete film screening: Viridiana on 2026-06-07 at 19:00" in {
    // "VIRIDIANA – BUÑUEL. NIECH ŻYJĄ KAJDANY" is in the kino listing;
    // slug: "viridiana".  Cleaned title: "Viridiana – buñuel. Niech żyją kajdany"
    // (sentence-cased).
    val movies = client.fetch()
    val film = movies.find(_.movie.title.toLowerCase.startsWith("viridiana")).value
    film.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 7, 19, 0))
  }

  it should "attach the director mined from the MSI RepertoireEvents Description" in {
    // The rendered HTML table carries no director; it lives in the page's
    // `RepertoireEvents` JS array ("REŻYSERIA Luis Buñuel."), joined back to the
    // film by its name. Confirms the JS-array mining + name join end to end.
    val film = client.fetch().find(_.movie.title.toLowerCase.startsWith("viridiana")).value
    film.director shouldBe Seq("Luis Buñuel")
  }

  it should "build the date-scoped /MSI/mvc/pl month URL, not the parameter-dropping /MSI redirect" in {
    // `/MSI?…&date=…` 301→302 redirects to a bare `/MSI/mvc/pl`, dropping the
    // `date` parameter, so the month was never actually selected. Hit `/MSI/mvc/pl`
    // directly — the shape every other MSI cinema uses — to scope the month.
    KinoZamekClient.monthUrl(YearMonth.of(2026, 6)) shouldBe
      "https://bilety.zamek.szczecin.pl/MSI/mvc/pl?sort=Name&date=2026-06"
  }

  it should "propagate an MSI fetch failure instead of swallowing it into an empty list" in {
    // A timeout / 5xx on the MSI host must surface — so RetryingCinemaScraper
    // retries and the uptime bar goes red — rather than being swallowed into a
    // silent "0 showtimes" success (white on the bar, empty errors array).
    val msiDown = new FakeHttpFetch("kino-zamek") {
      override def get(url: String): String =
        if (url.contains("bilety.zamek.szczecin.pl"))
          throw new RuntimeException(s"HTTP 503 for GET $url")
        else super.get(url)
    }
    val failing = new KinoZamekClient(msiDown, KinoZamekSzczecin, today = LocalDate.of(2026, 6, 7))
    a[RuntimeException] should be thrownBy failing.fetch()
  }

  // ── The programme past the second month ──────────────────────────────────
  //
  // The client fetched this month and the next, full stop. That covered
  // everything the portal held when measured on 2026-08-05 (nothing past
  // September), but two months is still a horizon cap: the castle programmes
  // retrospectives, and one reaching into the autumn would simply not be seen.
  // It walks the months now, like every other MSI portal.
  //
  // The month pages here are synthetic so the walk's shape is what's under test;
  // the recorded fixtures above cover how a real month page parses. "Viridiana"
  // is a film on the recorded allow-list, so these slots survive the film filter.

  private def msiMonth(dateText: String): String =
    s"""<div class="movies-movie__single">
       |<div class="movies-movie__single__title" title="Viridiana"></div>
       |<div class="movies-movie__single__options d-none"><ul class="movies-movie__single__options__hours">
       |<a href="/MSI/Default.aspx?event_id=1">$dateText</a></ul></div></div>""".stripMargin

  /** Serves the recorded allow-list page, and the given month pages for MSI.
   *  A month named in `throwing` fails the way a 5xx does. */
  private def portal(months: Map[String, String], throwing: Set[String] = Set.empty) =
    new FakeHttpFetch("kino-zamek") {
      override def get(url: String): String =
        if (!url.contains("bilety.zamek.szczecin.pl")) super.get(url)
        else {
          val month = """date=(\d{4}-\d{2})""".r.findFirstMatchIn(url).map(_.group(1)).getOrElse("")
          if (throwing.contains(month)) throw new RuntimeException(s"HTTP 503 for GET $url")
          else months.getOrElse(month, "")
        }
    }

  it should "keep walking the months while the portal still has a programme" in {
    val running = Map(
      "2026-06" -> msiMonth("07 cze 19:00"), "2026-07" -> msiMonth("07 lip 19:00"),
      "2026-08" -> msiMonth("07 sie 19:00"), "2026-09" -> msiMonth("07 wrz 19:00"))
    val movies = new KinoZamekClient(portal(running), KinoZamekSzczecin,
      today = LocalDate.of(2026, 6, 7)).fetch()

    val dates = movies.flatMap(_.showtimes).map(_.dateTime.toLocalDate)
    // September is the third month out — two beyond the window this used to ask for.
    dates should contain (LocalDate.of(2026, 9, 7))
    dates.size shouldBe 4
  }

  it should "not fail the scrape when a month PAST the programme is the one that blips" in {
    // Those months are only the probes that end the walk. Failing the whole
    // scrape over one would throw away a perfectly good June — and a scrape that
    // errors is a scrape not recorded.
    val movies = new KinoZamekClient(
      portal(Map("2026-06" -> msiMonth("07 cze 19:00")), throwing = Set("2026-08")),
      KinoZamekSzczecin, today = LocalDate.of(2026, 6, 7)).fetch()

    movies.flatMap(_.showtimes).map(_.dateTime.toLocalDate) shouldBe Seq(LocalDate.of(2026, 6, 7))
  }

  it should "still propagate a failure on a month INSIDE the programme" in {
    // June carries the programme and July fails: that is a failed read, not an
    // empty month, and recording it as a smaller listing would let scrape-prune
    // read the missing films as no longer screening.
    val partlyDown = portal(
      Map("2026-06" -> msiMonth("07 cze 19:00"), "2026-09" -> msiMonth("07 wrz 19:00")),
      throwing = Set("2026-07"))

    a[RuntimeException] should be thrownBy
      new KinoZamekClient(partlyDown, KinoZamekSzczecin, today = LocalDate.of(2026, 6, 7)).fetch()
  }

  // ── When the allow-list gives us nothing ─────────────────────────────────
  //
  // `isFilm` passes everything when the castle listing yields no slugs, so that a
  // broken listing page can't empty the cinema. On 2026-08-05 that page carried
  // two events, neither a film — and prod was publishing the castle's yoga
  // classes and terrace concerts as cinema. The shared classifier is the floor
  // under that fail-open.
  it should "still keep non-films out when the castle listing yields no slugs" in {
    val noListing = new FakeHttpFetch("kino-zamek") {
      override def get(url: String): String =
        if (url.startsWith(KinoZamekClient.ListingUrl)) ""    // the listing, not the MSI host
        else super.get(url)
    }
    val titles = new KinoZamekClient(noListing, KinoZamekSzczecin, today = LocalDate.of(2026, 6, 7))
      .fetch().map(_.movie.title.toLowerCase)

    titles should not be empty                       // the fail-open still lets films through
    titles.filter(_.contains("joga"))         shouldBe empty
    titles.filter(_.contains("silent disco")) shouldBe empty
    titles.filter(_.contains("koncert"))      shouldBe empty
    titles.filter(_.contains("lato na tarasach")) shouldBe empty
    // …and the films on the same page are untouched.
    titles.exists(_.startsWith("viridiana")) shouldBe true
  }
}
