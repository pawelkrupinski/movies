package clients.kino_zamek

import clients.tools.FakeHttpFetch
import models.KinoZamekSzczecin
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.KinoZamekClient

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
}
