package clients.kino_palacowe

import clients.tools.FakeHttpFetch
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.KinoPalacoweClient

import java.nio.file.{Files, Paths}

/**
 * Kino Pałacowe's "Kino bez barier" (accessible-screening) film pages tack a
 * genre onto the metadata blob, in two shapes the recorded fixtures cover:
 *   "… Kraje produkcji: Belgia, Francja. Gatunek: dramat.<br>"   (colon)
 *   "… Kraj produkcji: Francja. Gatunek animowany.<br>"          (adjective, no colon)
 * `parseGenres` is exercised directly against those real detail-page fixtures
 * because none of the genre-bearing films appear in the calendar listing the
 * `fetch()`-level spec replays.
 */
class KinoPalacoweGenreSpec extends AnyFlatSpec with Matchers {

  private val client = new KinoPalacoweClient(new FakeHttpFetch("kino-palacowe"))

  private def fixture(slug: String): String =
    new String(Files.readAllBytes(
      Paths.get(s"test/resources/fixtures/kino-palacowe/kinopalacowe.pl/filmy/$slug")))

  "KinoPalacoweClient.parseGenres" should "read a colon-form genre and title-case it" in {
    client.parseGenres(fixture("14336-kino-bez-barier-mode-matki-ad-cc-pjm")) shouldBe Seq("Dramat")
  }

  it should "read the adjective form written without a colon" in {
    client.parseGenres(fixture("14334-kino-bez-barier-arco-ad-cc-pjm")) shouldBe Seq("Animowany")
  }

  it should "read a genre carrying a Polish diacritic" in {
    client.parseGenres(fixture("14337-kino-bez-barier-zestaw-filmow-krotkometrazowych-z-")) shouldBe Seq("Krótkometrażowy")
  }

  it should "return empty for a detail page without a Gatunek marker" in {
    client.parseGenres(fixture("14402-osiem-i-po-federico-fellini-ciao-a-tutti")) shouldBe empty
  }

  it should "stop at the sentence-closing period, not swallow following prose" in {
    val html = "Kraje produkcji: USA. Gatunek: thriller. Bohater wyrusza w podróż."
    client.parseGenres(html) shouldBe Seq("Thriller")
  }

  it should "split a comma-separated genre list" in {
    val html = "Gatunek: komedia, dramat.<br>"
    client.parseGenres(html) shouldBe Seq("Komedia", "Dramat")
  }
}
