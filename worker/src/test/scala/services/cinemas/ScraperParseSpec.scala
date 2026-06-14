package services.cinemas

import org.jsoup.Jsoup
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.LocalTime

/** Direct coverage for the parsing snippets shared across the cinema scrapers.
  * Each `*Client` spec exercises these via a real fixture, but the shared helper
  * earns its own edge-case tests (out-of-range time, the `&quot;` CSS quoting,
  * a custom `<dt>` selector) so a future tweak can't silently break one scraper. */
class ScraperParseSpec extends AnyFlatSpec with Matchers {

  "parseHHmm" should "pull the first HH:mm out of surrounding text" in {
    ScraperParse.parseHHmm("seans o 19:30 dziś") shouldBe Some(LocalTime.of(19, 30))
  }

  it should "accept a single-digit hour" in {
    ScraperParse.parseHHmm("9:05") shouldBe Some(LocalTime.of(9, 5))
  }

  it should "return None when there is no time" in {
    ScraperParse.parseHHmm("brak godziny") shouldBe None
  }

  it should "return None for an out-of-range time instead of throwing" in {
    ScraperParse.parseHHmm("25:99") shouldBe None
  }

  "cssUrl" should "unwrap a plain url()" in {
    ScraperParse.cssUrl("background-image: url(/img/a.jpg)") shouldBe Some("/img/a.jpg")
  }

  it should "unwrap single, double and &quot; quoting" in {
    ScraperParse.cssUrl("""url('a.jpg')""")            shouldBe Some("a.jpg")
    ScraperParse.cssUrl("""url("b.jpg")""")            shouldBe Some("b.jpg")
    ScraperParse.cssUrl("""url(&quot;c.jpg&quot;)""")  shouldBe Some("c.jpg")
  }

  it should "return None when there is no url()" in {
    ScraperParse.cssUrl("color: red") shouldBe None
  }

  "ddField" should "return the <dd> after the matching <dt> (case-insensitive label)" in {
    val document = Jsoup.parse(
      "<dl><dt>Rok produkcji</dt><dd>2024</dd><dt>Reżyseria</dt><dd>Jan Kowalski</dd></dl>"
    )
    ScraperParse.ddField(document, "rok produkcji") shouldBe Some("2024")
    ScraperParse.ddField(document, "reżyseria")     shouldBe Some("Jan Kowalski")
  }

  it should "honour a custom dt selector and ignore dt outside it" in {
    val document = Jsoup.parse(
      "<dl class='x'><dt>Czas</dt><dd>90 min</dd></dl><dl><dt>Czas</dt><dd>WRONG</dd></dl>"
    )
    ScraperParse.ddField(document, "czas", "dl.x dt") shouldBe Some("90 min")
  }

  it should "return None for a missing label or an empty dd" in {
    val document = Jsoup.parse("<dl><dt>Rok</dt><dd>   </dd></dl>")
    ScraperParse.ddField(document, "rok")     shouldBe None // empty dd → None
    ScraperParse.ddField(document, "gatunek") shouldBe None // no such dt
  }

  "extractFormatTags" should "split a parenthesised format tag into clean title + display tokens" in {
    ScraperParse.extractFormatTags("Film (2D NAPISY)")  shouldBe (("Film", List("2D", "NAP")))
    ScraperParse.extractFormatTags("Film (2D DUBBING)") shouldBe (("Film", List("2D", "DUB")))
    ScraperParse.extractFormatTags("Film (3D)")         shouldBe (("Film", List("3D")))
  }

  it should "handle a separator-suffix tag (` / 2D dubbing`, ` - napisy`)" in {
    ScraperParse.extractFormatTags("Straszny film / 2D dubbing") shouldBe (("Straszny film", List("2D", "DUB")))
    ScraperParse.extractFormatTags("Straszny film - napisy")     shouldBe (("Straszny film", List("NAP")))
    ScraperParse.extractFormatTags("Film lektor")                shouldBe (("Film", List("LEK")))
  }

  it should "strip non-version words (dolby, atmos) without emitting a token for them" in {
    ScraperParse.extractFormatTags("Dzień objawienia (2D NAPISY DOLBY ATMOS)") shouldBe
      (("Dzień objawienia", List("2D", "NAP")))
  }

  it should "return (title, Nil) for a title with no format tag" in {
    ScraperParse.extractFormatTags("Mavka. Prawdziwy mit") shouldBe (("Mavka. Prawdziwy mit", Nil))
  }

  it should "not mangle a real dash/colon title" in {
    ScraperParse.extractFormatTags("Tom i Jerry: Przygoda w muzeum") shouldBe
      (("Tom i Jerry: Przygoda w muzeum", Nil))
  }

  it should "agree byte-for-byte with stripFormatTags on the cleaned title" in {
    val inputs = Seq(
      "Film (2D NAPISY)", "Straszny film / 2D dubbing", "Straszny film - napisy",
      "Dzień objawienia (2D NAPISY DOLBY ATMOS)", "Mavka. Prawdziwy mit",
      "Tom i Jerry: Przygoda w muzeum", "Toy story 5 (2D DUBBING DOLBY ATMOS)",
      "Film [2D napisy]", "DKF - Some film"
    )
    inputs.foreach(in => ScraperParse.extractFormatTags(in)._1 shouldBe ScraperParse.stripFormatTags(in))
  }

  "canonicalTrailer" should "canonicalise YouTube embed / watch / youtu.be to the watch form" in {
    ScraperParse.canonicalTrailer("https://www.youtube.com/embed/AYq1ljpbNfA?feature=oembed") shouldBe
      Some("https://www.youtube.com/watch?v=AYq1ljpbNfA")
    ScraperParse.canonicalTrailer("https://www.youtube.com/watch?v=C-h48bml6k0") shouldBe
      Some("https://www.youtube.com/watch?v=C-h48bml6k0")
    ScraperParse.canonicalTrailer("https://youtu.be/H1jQ_5vNGYk") shouldBe
      Some("https://www.youtube.com/watch?v=H1jQ_5vNGYk")
  }

  it should "pass a Vimeo URL through unchanged and drop anything else" in {
    ScraperParse.canonicalTrailer("https://player.vimeo.com/video/12345") shouldBe
      Some("https://player.vimeo.com/video/12345")
    ScraperParse.canonicalTrailer("https://example.com/not-a-trailer") shouldBe None
  }
}
