package clients.kinematograf_lodz

import models.KinoCharlie
import org.scalatest.OptionValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import tools.GetOnlyHttpFetch
import clients.tools.FakeHttpFetch
import services.cinemas.pl.KinematografLodzClient

import java.time.{LocalDate, LocalDateTime}
import services.movies.SingleCountryNormalizer.titleNormalizer

/** The title/date/director cases drive the parser directly with a minimal HTML
 *  string — `KinematografLodzClient` exposes `parseHtml(html, today, cinema)` in
 *  the `services.cinemas` package, and the inline HTML is the canonical
 *  structure of one `article.cwb-movie-item`.
 *
 *  The cinema parameter is passed explicitly (as `KinoCharlie` standing in for
 *  the real `KinematografLodz` object that will be added to Cinema.scala on
 *  integration). The `cinema` field tests propagation, not the specific object.
 *
 *  Four recorded captures back the rest, all replayed through `FakeHttpFetch`:
 *    - `kinematograf-lodz`               populated page, 19 cards (07-06-2026)
 *    - `kinematograf-lodz-dormant`       live 12-08-2026, "0 wydarzeń"
 *    - `kinematograf-lodz-shape-drift`   live 12-08-2026 site ROOT
 *    - `kinematograf-lodz-cards-restyled` the populated capture with only its
 *      card class renamed, standing in for a CMS restyle
 *
 *  Recorder line:
 *    curl -sSL -m 25 -A "Mozilla/5.0 (Macintosh; ...) Chrome/126 Safari/537.36"
 *      "https://muzeumkinematografii.pl/kino/repertuar-kina/"
 *      > test/resources/fixtures/kinematograf-lodz/muzeumkinematografii.pl/kino/repertuar-kina
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
  private val client     = new KinematografLodzClient(http, testCinema, today, titles = titleNormalizer)

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

  // ── The URL the museum publishes today ─────────────────────────────────────
  //
  // The site restructured in mid-2026: `/repertuar/` now 301s to
  // `/kino/repertuar-kina/`. The redirect is followed, so nothing breaks while
  // it lasts — but Helios showed what happens when a site stops honouring an
  // old slug, so address the live page and pin it here.

  it should "scrape the repertoire URL the site publishes today" in {
    KinematografLodzClient.RepertoireUrl shouldBe "https://muzeumkinematografii.pl/kino/repertuar-kina/"
  }

  it should "link /uptime at the page it actually scraped" in {
    new KinematografLodzClient(http, testCinema, today, titles = titleNormalizer)
      .sourceUrl shouldBe Some(KinematografLodzClient.RepertoireUrl)
  }

  // ── Real recorded captures ─────────────────────────────────────────────────
  //
  // Everything above drives `parseHtml` with inline HTML. These replay the real
  // page, which is what catches the museum restyling around us.

  it should "parse the real recorded repertoire page" in {
    // Capture of the populated page (19 cards, day strip advertising 16
    // screenings across 07-14.06.2026).
    val movies = new KinematografLodzClient(
      new FakeHttpFetch("kinematograf-lodz"), testCinema, LocalDate.of(2026, 6, 7), titles = titleNormalizer
    ).fetch()

    movies should not be empty
    movies.flatMap(_.showtimes) should not be empty
    movies.map(_.movie.title) should contain("Milcząca przyjaciółka")
  }

  // ── A zero-screening parse must be ACCOUNTED for ───────────────────────────
  //
  // The venue went white on /uptime on 2026-08-11 with no way to tell whether
  // it was dormant or unreadable — the same blind spot as Kino Sfinks. The
  // widget states its own size, so use it: zero cards is only "empty" when the
  // widget agrees it is empty.

  it should "report zero screenings, not a failure, when the widget says it has none" in {
    // Live capture 2026-08-12: the museum's repertoire module renders
    // `span.items-counte` → "0 wydarzeń" and all eight day tabs read
    // "brak seansów". Genuinely dormant, so this must stay white, not go red.
    new KinematografLodzClient(
      new FakeHttpFetch("kinematograf-lodz-dormant"), testCinema, today, titles = titleNormalizer
    ).fetch() shouldBe empty
  }

  it should "fail loudly when the page carries neither a screening card nor the widget's own counters" in {
    // Live capture 2026-08-12 of the site ROOT, standing in for the repertoire
    // URL serving something that isn't the repertoire — the Helios slug-rename
    // shape. Note the homepage DOES carry `div.movies-tickets-inner` and
    // `div.cwb-movie-empty-state` for its own carousel, so this also pins that
    // the guard is not keyed on those false friends.
    val thrown = the[RuntimeException] thrownBy new KinematografLodzClient(
      new FakeHttpFetch("kinematograf-lodz-shape-drift"), testCinema, today, titles = titleNormalizer
    ).fetch()
    thrown.getMessage should include("muzeumkinematografii.pl")
  }

  it should "fail loudly when the widget advertises screenings it no longer renders as cards" in {
    // The real populated capture with its screening-card class renamed
    // (`article.cwb-movie-item` → `article.cwb-screening-card`) and NOTHING else
    // touched: the widget still counts "19 wydarzeń" and the day strip still
    // advertises 4/2/2/2/3/3 seanse. That is what a CMS restyle looks like, and
    // it is the case a presence-only guard would still paint white — the venue
    // looks readable and merely empty.
    val thrown = the[RuntimeException] thrownBy new KinematografLodzClient(
      new FakeHttpFetch("kinematograf-lodz-cards-restyled"), testCinema, LocalDate.of(2026, 6, 7), titles = titleNormalizer
    ).fetch()
    thrown.getMessage should include("advertises 19")
  }

  // ── The widget's own accounting, read directly ─────────────────────────────

  it should "read the item counter in preference to the day strip" in {
    val doc = org.jsoup.Jsoup.parse(
      """<span class="items-counte">7 wydarzeń</span>
         <a class="cinema-day-item"><div class="day-count">3 seanse</div></a>"""
    )
    KinematografLodzClient.advertisedScreenings(doc) shouldBe Some(7)
  }

  it should "sum the day strip when the item counter is gone" in {
    val doc = org.jsoup.Jsoup.parse(
      """<a class="cinema-day-item"><div class="day-count">3 seanse</div></a>
         <a class="cinema-day-item"><div class="day-count">brak seansów</div></a>
         <a class="cinema-day-item"><div class="day-count">1 seans</div></a>"""
    )
    KinematografLodzClient.advertisedScreenings(doc) shouldBe Some(4)
  }

  it should "report no accounting at all for a page carrying neither marker" in {
    KinematografLodzClient.advertisedScreenings(
      org.jsoup.Jsoup.parse("""<div class="movies-tickets-inner"></div>""")
    ) shouldBe None
  }
}
