package clients.kino_zamek

import org.scalatest.OptionValues
import clients.tools.FakeHttpFetch
import models.KinoZamekSzczecin
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas.pl.KinoZamekClient

import java.time.{LocalDate, LocalDateTime}

/** Replays the castle's own `zamek.szczecin.pl/wydarzenia/kino/` category through
 *  the client. Two captures, because the site publishes its cinema in two shapes
 *  and the parser has to read both:
 *
 *   - `kino-zamek` (2026-08-08) — SUMMER. The category holds one CYCLE page,
 *     `zamkowe-noce-filmowe-2026`, carrying ten different films on a single page,
 *     plus one non-film event.
 *   - `kino-zamek-season` (listing from the 08-06-2026 corpus, event pages
 *     captured 2026-08-08) — IN SEASON. The category holds one page PER FILM,
 *     each naming its film in `<h1>`, plus a festival umbrella page that lists
 *     the same films again under week headings.
 *
 *  `today` is pinned to each capture's anchor so the year-less prose dates
 *  resolve deterministically.
 *
 *  Previously scraped from `bilety.zamek.szczecin.pl`, an MSI portal that stopped
 *  accepting TCP connections entirely. */
class KinoZamekClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val summer = new KinoZamekClient(
    new FakeHttpFetch("kino-zamek"), KinoZamekSzczecin, today = LocalDate.of(2026, 8, 8)).fetch()

  private val season = new KinoZamekClient(
    new FakeHttpFetch("kino-zamek-season"), KinoZamekSzczecin, today = LocalDate.of(2026, 6, 8)).fetch()

  "KinoZamekClient" should "read the summer cycle page as many films, not one event" in {
    // The whole open-air season lives on ONE event page; reading it as a single
    // film would publish "Zamkowe Noce Filmowe 2026" as though it were a movie.
    summer should not be empty
    summer.map(_.cinema).toSet shouldBe Set(KinoZamekSzczecin)
    all(summer.map(_.showtimes)) should not be empty
    summer.map(_.movie.title) should contain allOf (
      "Ostatni wiking (den sidste viking)", "Father mother sister brother")
    summer.map(_.movie.title).exists(_.toLowerCase.contains("zamkowe noce filmowe 2026")) shouldBe false
  }

  it should "pin a concrete cycle screening with the director off the same block" in {
    val film = summer.find(_.movie.title.startsWith("Ostatni wiking")).value
    film.showtimes.map(_.dateTime) shouldBe Seq(LocalDateTime.of(2026, 8, 11, 21, 30))
    film.director shouldBe Seq("Anders Thomas Jensen")
    film.filmUrl.value shouldBe "https://zamek.szczecin.pl/wydarzenie/kino/zamkowe-noce-filmowe-2026/"
  }

  it should "drop a cycle's past nights instead of rolling them a year forward" in {
    // The cycle page still lists the whole season, opening night included. The
    // dates carry no year, so a next-occurrence rule would read "30 czerwca" seen
    // in August as 2027 and publish a screening eleven months out that nobody is
    // going to. Only the four remaining nights survive.
    summer.flatMap(_.showtimes).map(_.dateTime) should have size 4
    all(summer.flatMap(_.showtimes).map(_.dateTime.toLocalDate)) should be >= LocalDate.of(2026, 8, 8)
    summer.map(_.movie.title).exists(_.toLowerCase.startsWith("amadeusz")) shouldBe false  // 4 August, played
  }

  it should "ignore a kino-category event that advertises no screenings" in {
    // "44.-45. Pomorskie Spotkania z Diaporamą" is filed under kino but is a
    // photography competition — it carries no `godzina HH:MM` line at all, so it
    // contributes nothing rather than becoming a film with no showtimes.
    summer.map(_.movie.title).exists(_.toLowerCase.contains("diaporam")) shouldBe false
  }

  // ── In season: one page per film ─────────────────────────────────────────

  it should "title a single-film page from its <h1>, not from the programme strand" in {
    // Every page in the classics festival is headed "SZCZECIŃSKIE ŚWIĘTO KLASYKI
    // FILMOWEJ W KINIE ZAMEK" — the bold line nearest each date. Reading that as
    // the title would publish thirty identically-named films.
    season.map(_.movie.title) should contain allOf ("Casablanca", "Milczenie owiec", "Faraon")
    season.map(_.movie.title).exists(_.toLowerCase.contains("święto klasyki")) shouldBe false
  }

  it should "not mistake the umbrella page's WEEK headings for films" in {
    // The festival's own page groups the month under "II TYDZIEŃ POKAZÓW – …"
    // headings, which are 14pt-bold exactly like a cycle page's film titles.
    // Keying the title on 14pt produced three "films" named after weeks.
    season.map(_.movie.title).filter(_.toLowerCase.contains("tydzień pokazów")) shouldBe empty
  }

  it should "merge a film listed on both its own page and the umbrella page" in {
    // "Pociągi" is on the umbrella page as "POCIĄGI – …" and has its own DKF page
    // titled "Pociągi – Dyskusyjny Klub Filmowy Zamek". One 18:00 showing, so it
    // must be ONE card — and the director exists only on the film's own page, so
    // the merge has to keep it rather than take whichever entry came first.
    val pociagi = season.filter(_.movie.title.toLowerCase.startsWith("pociągi"))
    pociagi should have size 1
    pociagi.head.showtimes.map(_.dateTime) shouldBe Seq(LocalDateTime.of(2026, 6, 18, 18, 0))
    pociagi.head.director shouldBe Seq("Maciej J. Drygas")
    season.find(_.movie.title == "Casablanca").value.director shouldBe Seq("Michael Curtiz")
  }

  it should "keep every date a multi-date film page lists" in {
    val orly = season.find(_.movie.title == "Orły republiki").value
    orly.showtimes.map(_.dateTime) shouldBe Seq(
      LocalDateTime.of(2026, 6, 12, 16, 30), LocalDateTime.of(2026, 6, 13, 16, 30),
      LocalDateTime.of(2026, 6, 14, 16, 30), LocalDateTime.of(2026, 6, 17, 16, 30))
  }

  it should "take the year from the page's own dated list, not from today" in {
    // The prose reads "21 czerwca (niedziela), godz. 19:00" with no year; only
    // `<p class="event-details">21-06-2026</p>` carries one.
    season.find(_.movie.title == "Casablanca").value
      .showtimes.map(_.dateTime) shouldBe Seq(LocalDateTime.of(2026, 6, 21, 19, 0))
  }

  it should "read the whole category, not just the film pages" in {
    season should have size 33
    season.flatMap(_.showtimes) should have size 45
  }

  // ── Reading failures ─────────────────────────────────────────────────────

  it should "throw when the listing renders without its events container" in {
    // A page we cannot read must not be reported as a venue with no films: a
    // silent zero is indistinguishable from a dormant venue on the uptime bar,
    // which is how a CMS migration hides for weeks. Same guard as KinoSfinks.
    val drifted = listingServing("<html><body><h1>Kino</h1></body></html>")
    an[IllegalStateException] should be thrownBy
      new KinoZamekClient(drifted, KinoZamekSzczecin, today = LocalDate.of(2026, 8, 8)).fetch()
  }

  it should "return empty — not throw — when the category rendered but holds no films" in {
    // A genuinely unprogrammed category is a white bar, not a red one.
    val unprogrammed = listingServing("""<html><body><div id="events-list"></div></body></html>""")
    new KinoZamekClient(unprogrammed, KinoZamekSzczecin, today = LocalDate.of(2026, 8, 8))
      .fetch() shouldBe empty
  }

  it should "propagate when every event page fails, rather than record an empty venue" in {
    // A source that is wholly down must surface as red so RetryingCinemaScraper
    // retries — never as a successful "0 showtimes" scrape, which scrape-prune
    // would read as the films having stopped.
    val allDown = new FakeHttpFetch("kino-zamek") {
      override def get(url: String): String =
        if (url.contains("/wydarzenie/")) throw new RuntimeException(s"HTTP 503 for GET $url")
        else super.get(url)
    }
    a[RuntimeException] should be thrownBy
      new KinoZamekClient(allDown, KinoZamekSzczecin, today = LocalDate.of(2026, 8, 8)).fetch()
  }

  it should "keep the rest of the programme when one event page blips" in {
    // The non-film Diaporama page fails here; the cycle still has to come back.
    val oneDown = new FakeHttpFetch("kino-zamek") {
      override def get(url: String): String =
        if (url.contains("diaporam")) throw new RuntimeException(s"HTTP 503 for GET $url")
        else super.get(url)
    }
    new KinoZamekClient(oneDown, KinoZamekSzczecin, today = LocalDate.of(2026, 8, 8))
      .fetch() should have size 4
  }

  it should "no longer reach for the MSI portal that went dark" in {
    // bilety.zamek.szczecin.pl stopped accepting TCP on :443 and :80 between
    // 2026-08-04 and 2026-08-08. Pinned because a scraper still naming that host
    // spends its whole time budget waiting for a connection that never opens.
    val client = new KinoZamekClient(new FakeHttpFetch("kino-zamek"), KinoZamekSzczecin)
    client.scrapeHosts shouldBe Set("zamek.szczecin.pl")
    client.sourceUrl.value shouldBe "https://zamek.szczecin.pl/wydarzenia/kino/"
  }

  /** The recorded event pages, but the category listing replaced by `html`. */
  private def listingServing(html: String) = new FakeHttpFetch("kino-zamek") {
    override def get(url: String): String =
      if (url == KinoZamekClient.ListingUrl) html else super.get(url)
  }
}
