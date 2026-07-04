package clients.kino_fenomen

import clients.tools.FakeHttpFetch
import models.KinoFenomen
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.KinoFenomenClient

import java.time.LocalDateTime

/** Replays the recorded `iframe639.biletyna.pl/?display=events` page
 *  (04-07-2026 capture) through the client. The listing is server-rendered and
 *  covers the next few weeks on a single page. */
class KinoFenomenClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val http   = new FakeHttpFetch("kino-fenomen")
  private val client = new KinoFenomenClient(http, KinoFenomen)

  "KinoFenomenClient" should "return a non-empty film list" in {
    val movies = client.fetch()
    movies should not be empty
  }

  it should "tag every film with KinoFenomen" in {
    val movies = client.fetch()
    movies.map(_.cinema).toSet shouldBe Set(KinoFenomen)
  }

  it should "give every film at least one showtime" in {
    val movies = client.fetch()
    all(movies.map(_.showtimes)) should not be empty
  }

  it should "pin a concrete screening: Ojczyzna on 2026-07-05 at 16:00" in {
    // Fixture: 05.07.2026 16:00 — event id 681094, artist id 62176
    val movies   = client.fetch()
    val ojczyzna = movies.find(_.movie.title == "Ojczyzna").value
    ojczyzna.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 7, 5, 16, 0))
  }

  it should "attach a booking URL to each showtime" in {
    val movies = client.fetch()
    movies.flatMap(_.showtimes).flatMap(_.bookingUrl).foreach { u =>
      u should startWith("https://iframe639.biletyna.pl/event/view/id/")
    }
  }

  it should "strip the format tag from titles" in {
    // Raw: "Gwiazdy (2D/oryginalny)" — artist id 5885
    val movies = client.fetch()
    movies.map(_.movie.title) should contain("Gwiazdy")
    movies.map(_.movie.title).foreach(_ should not include "2D/")
  }

  // ── Director + production year from the artist-link metadata ───────────────

  it should "extract director(s) from the '| reżyseria: … |' segment" in {
    KinoFenomenClient.parseDirectors(
      "„Milcząca przyjaciółka” | reżyseria: Ildikó Enyedi | Francja, Niemcy, Węgry 2025 (2D/napisy)"
    ) shouldBe Seq("Ildikó Enyedi")
    KinoFenomenClient.parseDirectors("Film | reżyseria: A. Kowalski, B. Nowak | Polska 2024") shouldBe
      Seq("A. Kowalski", "B. Nowak")
    KinoFenomenClient.parseDirectors("Orły Republiki (2D/napisy)") shouldBe Seq.empty
  }

  it should "extract the production year from the metadata segment or a (YYYY) paren" in {
    // Trailing "Country YYYY" metadata, ignoring the "(2D/napisy)" format tag.
    KinoFenomenClient.parseYear(
      "„Milcząca przyjaciółka” | reżyseria: Ildikó Enyedi | Francja, Niemcy, Węgry 2025 (2D/napisy)"
    ) shouldBe Some(2025)
    // Year carried in a paren in the title itself.
    KinoFenomenClient.parseYear("Mikey i Nicky (1976) (2D/oryginalny)") shouldBe Some(1976)
    // A bare title with no pipe metadata and no paren-year yields None.
    KinoFenomenClient.parseYear("Orły Republiki (2D/napisy)") shouldBe None
  }

  it should "surface a (YYYY) paren year from the listing title, never a listing director" in {
    // The listing no longer carries "| reżyseria: … | Country YYYY" metadata, so a
    // film's year now comes only from a (YYYY) paren in the title, and the director
    // is filled from the detail page later — never the listing.
    val movies  = client.fetch()
    val general = movies.find(_.movie.title.contains("Generał")).value
    general.movie.releaseYear shouldBe Some(1926)
    general.director          shouldBe empty
    // A plain title (no paren, no metadata) surfaces neither.
    val ojczyzna = movies.find(_.movie.title == "Ojczyzna").value
    ojczyzna.movie.releaseYear shouldBe None
    ojczyzna.director          shouldBe empty
  }

  // Detail head as SEPARATE <p> lines ("<p>reżyseria: …</p><p>występują: …</p>
  // <p>/ … / … min.</p>") — artist id 57079.
  it should "fetch cast, director, synopsis, runtime, country/year, genre and poster from a separate-line detail page" in {
    val d = client.fetchFilmDetail("https://iframe639.biletyna.pl/artist/view/id/57079").value
    d.director        should contain("Joachim Trier")
    d.cast            should contain allOf ("Renate Reinsve", "Stellan Skarsgard")
    d.runtimeMinutes  shouldBe Some(133)
    d.releaseYear     shouldBe Some(2025)
    d.countries       should contain("Norwegia")
    d.genres          should contain("dramat")
    d.synopsis.value  should include("Joachima Triera")
    d.posterUrl.value should include("/file/get/")
  }

  // Detail head COLLAPSED into one <p> ("reżyseria: X występują: Y / country /
  // genre / year / N min.") — the newer biletyna layout; splitHead must recover
  // each field instead of stuffing the whole run into `director`. Artist id 23224.
  it should "fetch fields from a collapsed single-line detail head" in {
    val d = client.fetchFilmDetail("https://iframe639.biletyna.pl/artist/view/id/23224").value
    d.director       shouldBe Seq("Jim Jarmusch")
    d.cast           should contain allOf ("Johnny Depp", "Gary Farmer", "Crispin Glover")
    d.runtimeMinutes shouldBe Some(121)
    d.releaseYear    shouldBe Some(1995)
    d.countries      should contain("USA")
    d.genres         should contain("dramat")
    d.synopsis.value should include("Jarmusch")
  }

  // A detail page with ONLY synopsis prose (no reżyseria/występują/meta) — still a
  // useful fetch (synopsis + poster), so it must not come back None (→ red enrichment).
  it should "fetch synopsis-only detail pages without a metadata head" in {
    val d = client.fetchFilmDetail("https://iframe639.biletyna.pl/artist/view/id/62176").value
    d.synopsis.value should include("Ojczyzna")
    d.director       shouldBe empty
    d.cast           shouldBe empty
    d.releaseYear    shouldBe None
  }

  it should "expose itself as a deferred DetailEnricher resolving TMDB from the listing" in {
    client                       shouldBe a[services.cinemas.DetailEnricher]
    client.detailGroup           shouldBe "kino-fenomen"
    client.defersTmdbResolution  shouldBe false
  }
}
