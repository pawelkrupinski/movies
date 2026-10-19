package clients.falenica

import models.{Showtime, StacjaFalenica}
import clients.tools.FakeHttpFetch
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import tools.GetOnlyHttpFetch
import services.cinemas.pl.FalenicaClient

import java.time.LocalDateTime

class FalenicaClientSpec extends AnyFlatSpec with Matchers {

  private val client  = new FalenicaClient(new FakeHttpFetch("kino-falenica"))
  private val results = client.fetch()
  private val byTitle = results.map(cm => cm.movie.title -> cm).toMap

  "FalenicaClient.fetch" should "return 25 films and 51 showtimes" in {
    results.size shouldBe 25
    results.flatMap(_.showtimes).size shouldBe 51
  }

  it should "assign Stacja Falenica to every entry" in {
    results.map(_.cinema).toSet shouldBe Set(StacjaFalenica)
  }

  it should "read runtime, director and showtimes off the falenica3-theme listing/detail pages" in {
    val m = byTitle("500 mil")
    m.movie.runtimeMinutes shouldBe Some(102)
    m.director shouldBe Seq("Morgan Matthews")
    m.showtimes.head shouldBe
      Showtime(LocalDateTime.of(2026, 9, 13, 14, 30), Some("https://ksf.systembiletowy.pl/index.php/repertoire.html?id=34540"), None, Nil)
  }

  // The listing bakes the version into the title ("Ścieżki życia – LEKTOR"); it's
  // peeled off and carried as a per-showtime format badge.
  it should "strip a trailing 'LEKTOR' tag off the listing title into the showtime format badge" in {
    byTitle("Ścieżki życia").showtimes.flatMap(_.format).toSet shouldBe Set("LEK")
  }

  // Regression for the 2026-09 `falenica3` theme redesign: the listing's
  // `<article class="filmy">` wrapper (a WP custom-post-type class) is gone,
  // replaced by `div.repe-box` as the outermost per-film element, with a
  // crafted minimal page standing in for the live markup.
  it should "parse a listing item whose outer wrapper is div.repe-box, not article.filmy" in {
    val listing =
      """<div class="repe-box"><h2 class="repe_title">
        |<a href="/filmy/test-film/">Test Film</a></h2>
        |<div class="repe_czas">100 min | reż. Jane Doe</div></div>""".stripMargin
    val detail =
      """<div class="entry-terms__row">
        |<div class="entry-terms__date">20.09.2026</div>
        |<div class="entry-terms__time">18:00</div>
        |<div class="entry-terms__cta"><a href="https://ksf.systembiletowy.pl/x">Kup bilet</a></div>
        |</div>""".stripMargin
    val stub = new GetOnlyHttpFetch {
      def get(url: String): String = if (url.contains("/filmy/")) detail else listing
    }
    val movies = new FalenicaClient(stub).fetch()
    val film   = movies.find(_.movie.title == "Test Film").getOrElse(fail("no Test Film"))
    film.director shouldBe Seq("Jane Doe")
    film.showtimes shouldBe Seq(
      Showtime(LocalDateTime.of(2026, 9, 20, 18, 0), Some("https://ksf.systembiletowy.pl/x"), None, Nil))
  }

  // Regression for the `__trashed` over-filter: the venue trashes the
  // WordPress post but keeps live showtimes. A `/filmy/__trashed-N/` slug must
  // not be excluded — only the `showtimes.isEmpty` drop (a dead page with no
  // "Dostępne terminy") should remove a film.
  it should "include a film whose WordPress slug is __trashed but still has showtimes" in {
    val listing =
      """<div class="repe-box"><h2 class="repe_title">
        |<a href="/filmy/__trashed-9/">Trashed Film</a></h2></div>""".stripMargin
    val detail =
      """<div class="entry-terms__row">
        |<div class="entry-terms__date">21.09.2026</div>
        |<div class="entry-terms__time">19:00</div>
        |<div class="entry-terms__cta"><a href="https://ksf.systembiletowy.pl/y">Kup bilet</a></div>
        |</div>""".stripMargin
    val stub = new GetOnlyHttpFetch {
      def get(url: String): String = if (url.contains("/filmy/")) detail else listing
    }
    val movies = new FalenicaClient(stub).fetch()
    movies.find(_.movie.title == "Trashed Film") should not be empty
  }

  it should "read the YouTube trailer off the detail page's data-youtube-id button" in {
    val detail = client.fetchFilmDetail(byTitle("Tony").filmUrl.getOrElse(fail("no filmUrl for Tony")))
      .getOrElse(fail("no detail for Tony"))
    detail.trailerUrl shouldBe Some("https://www.youtube.com/watch?v=uI88YvjFdgE")
  }

  // Regression: `article.entry-description` opens with an "O filmie" <h2>
  // heading (dropped, else it leaks into the first line) and the showtimes
  // list + trailer button now live in SIBLING elements, not nested inside it.
  it should "extract the synopsis prose without the 'O filmie' heading, showtimes, or trailer" in {
    val detail = client.fetchFilmDetail(byTitle("Tony").filmUrl.getOrElse(fail("no filmUrl for Tony")))
      .getOrElse(fail("no detail for Tony"))
    val synopsis = detail.synopsis.getOrElse(fail("no synopsis for Tony"))
    synopsis should include("Anthony Bourdain")
    synopsis should not startWith "O filmie"
    synopsis should not include "Dostępne terminy"
    synopsis should not include "youtube.com"
  }

  // The detail page wraps the synopsis in several `<p>` blocks; flattening them
  // with jsoup `.text` fuses the whole thing into one wall of prose.
  // `cleanSynopsis`/`blockText` must preserve the paragraph breaks as blank
  // lines so the markdown view renders them as separate paragraphs.
  it should "preserve paragraph breaks between synopsis blocks" in {
    val detail = client.fetchFilmDetail(byTitle("Tony").filmUrl.getOrElse(fail("no filmUrl for Tony")))
      .getOrElse(fail("no detail for Tony"))
    val synopsis = detail.synopsis.getOrElse(fail("no synopsis for Tony"))
    withClue(s"synopsis = ${synopsis.replace("\n", "\\n")}\n") {
      synopsis should include("\n\n")
    }
  }
}
