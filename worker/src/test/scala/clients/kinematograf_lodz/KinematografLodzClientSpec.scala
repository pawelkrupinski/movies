package clients.kinematograf_lodz

import models.KinoCharlie
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.KinematografLodzClient
import tools.GetOnlyHttpFetch

import java.time.{LocalDate, LocalDateTime}

/** Drives the parser directly with a self-contained minimal HTML string.
 *  No fixture files are needed because `KinematografLodzClient` exposes
 *  `parseHtml(html, today, cinema)` in the `services.cinemas` package; the
 *  inline HTML below is the canonical structure of one `article.cwb-movie-item`
 *  as fetched from `muzeumkinematografii.pl/repertuar/` on 07-06-2026.
 *
 *  The cinema parameter is passed explicitly (as `KinoCharlie` standing in for
 *  the real `KinematografLodz` object that will be added to Cinema.scala on
 *  integration). The `cinema` field tests propagation, not the specific object.
 *
 *  Recorder line (for a full fixture-based test later):
 *    curl -sSL -m 25 -A "Mozilla/5.0 (Macintosh; ...) Chrome/120 Safari/537.36"
 *      "https://muzeumkinematografii.pl/repertuar/"
 *      > test/resources/fixtures/kinematograf-lodz/muzeumkinematografii.pl/repertuar.html
 */
class KinematografLodzClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  // Minimal two-item HTML matching the live page structure.
  // "Znaki Pana Śliwki" is a future event; the second is a past event that
  // must be filtered out.
  private val sampleHtml = """<!DOCTYPE html><html><body>
    <article class="cwb-movie-item h-100">
      <a href="https://muzeumkinematografii.pl/repertuar/znaki-pana-sliwki/"
         class="cwb-movie-card-link"
         title="Przejdź do seansu: Znaki Pana Śliwki (2025), reż. Urszula Morga, Bartosz Mikołajczyk"></a>
      <div class="thumbnail-container">
        <div class="cwb-movie-thumb d-block mb-3">
          <img class="wp-post-image"
               data-src="https://muzeumkinematografii.pl/wp-content/uploads/2026/05/znaki.jpg">
        </div>
      </div>
      <div class="cwb-movie-card-info">
        <div class="date-time"> 07.06.2026 14:00</div>
      </div>
    </article>
    <article class="cwb-movie-item h-100">
      <a href="https://muzeumkinematografii.pl/repertuar/rozmowa/"
         class="cwb-movie-card-link"
         title="Przejdź do seansu: Klasyk w kinie: Rozmowa (1973)"></a>
      <div class="cwb-movie-card-info">
        <div class="date-time"> 08.06.2026 19:00</div>
      </div>
    </article>
    <article class="cwb-movie-item h-100">
      <a href="https://muzeumkinematografii.pl/repertuar/stary-film/"
         class="cwb-movie-card-link"
         title="Przejdź do seansu: Stary Film (2025), reż. Jan Kowalski"></a>
      <div class="cwb-movie-card-info">
        <div class="date-time"> 01.01.2026 18:00</div>
      </div>
    </article>
  </body></html>"""

  // Stub HTTP that returns the sample HTML for any URL.
  private val http = new GetOnlyHttpFetch {
    def get(url: String): String = sampleHtml
  }

  private val testCinema = KinoCharlie  // stand-in; real integration uses KinematografLodz
  private val today      = LocalDate.of(2026, 6, 7)
  private val client     = new KinematografLodzClient(http, testCinema, today)

  "KinematografLodzClient" should "return a non-empty film list" in {
    client.fetch() should not be empty
  }

  it should "tag every film with the cinema passed in" in {
    client.fetch().map(_.cinema).toSet shouldBe Set(testCinema)
  }

  it should "give every film at least one showtime" in {
    all(client.fetch().map(_.showtimes)) should not be empty
  }

  it should "pin a concrete screening: Znaki Pana Śliwki on 2026-06-07 at 14:00" in {
    val movies = client.fetch()
    val znaki  = movies.find(_.movie.title == "Znaki Pana Śliwki").value
    znaki.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 7, 14, 0))
  }

  it should "strip the 'reż.' director suffix from the title" in {
    val movies = client.fetch()
    movies.map(_.movie.title).exists(_.contains("reż.")) shouldBe false
  }

  it should "strip the trailing '(YYYY)' release-year suffix from the title" in {
    val titles = client.fetch().map(_.movie.title)
    // Director + year stripped down to the bare title.
    titles should contain("Znaki Pana Śliwki")
    // Programme prefix kept, only the year stripped.
    titles should contain("Klasyk w kinie: Rozmowa")
    // No title retains a trailing "(YYYY)".
    titles.exists(_.matches(""".*\(\d{4}\)\s*$""")) shouldBe false
  }

  it should "drop past screenings (01-01-2026 is before today)" in {
    val movies = client.fetch()
    movies.exists(_.movie.title.contains("Stary Film")) shouldBe false
  }

  // ── Year + director extracted off the raw title before the strip ───────────

  it should "surface the production year and director(s) on the film" in {
    val znaki = client.fetch().find(_.movie.title == "Znaki Pana Śliwki").value
    znaki.movie.releaseYear shouldBe Some(2025)
    znaki.director          shouldBe Seq("Urszula Morga", "Bartosz Mikołajczyk")
  }

  it should "set the year but no director for a title with no 'reż.' suffix" in {
    val rozmowa = client.fetch().find(_.movie.title == "Klasyk w kinie: Rozmowa").value
    rozmowa.movie.releaseYear shouldBe Some(1973)
    rozmowa.director          shouldBe empty
  }

  it should "bound the director list at a trailing year or event suffix" in {
    // Real-fixture shapes: director then ", (2026)"; director then ". • <event>".
    KinematografLodzClient.parseDirectors(
      "Mały Kinematograf: Baczne oczka reż. Katarzyna Agopsowicz, (2026)"
    ) shouldBe Seq("Katarzyna Agopsowicz")
    KinematografLodzClient.parseDirectors(
      "DKF Człowiek w Zagrożeniu: Pociągi (2024), reż. Maciej Drygas. • Spotkanie z autorem"
    ) shouldBe Seq("Maciej Drygas")
  }

  it should "not mistake the prose 'reżyserką' mention for a director marker" in {
    // No `reż.` token → no director (the word "reżyserką" must not match).
    KinematografLodzClient.parseDirectors(
      "Mały Kinematograf: premiera animacji i spotkanie z reżyserką Aleksandrą Chrapowicką"
    ) shouldBe empty
  }
}
