package services.cinemas

import org.jsoup.Jsoup
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.{LocalDate, LocalTime}

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
    val doc = Jsoup.parse(
      "<dl><dt>Rok produkcji</dt><dd>2024</dd><dt>Reżyseria</dt><dd>Jan Kowalski</dd></dl>"
    )
    ScraperParse.ddField(doc, "rok produkcji") shouldBe Some("2024")
    ScraperParse.ddField(doc, "reżyseria")     shouldBe Some("Jan Kowalski")
  }

  it should "honour a custom dt selector and ignore dt outside it" in {
    val doc = Jsoup.parse(
      "<dl class='x'><dt>Czas</dt><dd>90 min</dd></dl><dl><dt>Czas</dt><dd>WRONG</dd></dl>"
    )
    ScraperParse.ddField(doc, "czas", "dl.x dt") shouldBe Some("90 min")
  }

  it should "return None for a missing label or an empty dd" in {
    val doc = Jsoup.parse("<dl><dt>Rok</dt><dd>   </dd></dl>")
    ScraperParse.ddField(doc, "rok")     shouldBe None // empty dd → None
    ScraperParse.ddField(doc, "gatunek") shouldBe None // no such dt
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

  "parsePolishDate" should "parse a 'DD <month> YYYY' Polish date with an explicit year" in {
    ScraperParse.parsePolishDate("Premiera kinowa 5 maja 2023 r.")  shouldBe Some(LocalDate.of(2023, 5, 5))
    ScraperParse.parsePolishDate("31 sierpnia 2011 r.")             shouldBe Some(LocalDate.of(2011, 8, 31))
  }

  it should "return None when there is no full date or the month is unknown" in {
    ScraperParse.parsePolishDate("Premiera: 11 kwietnia") shouldBe None // year-less
    ScraperParse.parsePolishDate("5 blursday 2023")       shouldBe None // bad month
  }
}
