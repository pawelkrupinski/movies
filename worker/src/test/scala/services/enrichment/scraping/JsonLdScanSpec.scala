package services.enrichment.scraping

import org.jsoup.Jsoup
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.*

/** The JSON-LD scan reads a page's `<script type="application/ld+json">` blocks without building
 *  its DOM. It must find exactly what a real HTML parser does, so it is held to Jsoup on every
 *  recorded Metacritic and Rotten Tomatoes page — the only pages it reads. */
class JsonLdScanSpec extends AnyFlatSpec with Matchers {

  private val Fixtures = Paths.get("test/resources/fixtures")
  private val RatingSites = Seq("www.metacritic.com", "www.rottentomatoes.com")

  private def ratingPages: Seq[Path] =
    Files.walk(Fixtures).iterator.asScala
      .filter(path => Files.isRegularFile(path) && RatingSites.exists(site => path.toString.contains(site)))
      .toSeq

  private def byJsoup(html: String): Seq[String] =
    Jsoup.parse(html).select("script[type=application/ld+json]").asScala.toSeq.map(_.data())

  "the JSON-LD scan" should "find the same blocks as Jsoup on every recorded rating-site page" in {
    withClue(s"$Fixtures must exist (run from the repo root)")(Files.isDirectory(Fixtures) shouldBe true)
    val pages = ratingPages
    pages.size should be > 400
    val differing = pages.flatMap { page =>
      val html = new String(Files.readAllBytes(page), StandardCharsets.UTF_8)
      Option.when(JsonLdAggregateRating.scripts(html) != byJsoup(html))(page.toString)
    }
    withClue(s"pages whose blocks differ from Jsoup's:\n${differing.take(20).mkString("\n")}\n")(differing shouldBe empty)
  }

  it should "match the type however it is quoted or cased, and skip other scripts" in {
    val html =
      """<script>var a = 1;</script>
        |<script type="application/ld+json">{"a":1}</script>
        |<script TYPE='application/LD+JSON' id="x">{"b":2}</script>
        |<script type=application/ld+json>{"c":3}</script>
        |<script type="application/ld+jsonx">{"d":4}</script>
        |<script type="text/javascript">{"e":5}</script>""".stripMargin
    JsonLdAggregateRating.scripts(html) shouldBe byJsoup(html)
    JsonLdAggregateRating.scripts(html) shouldBe Seq("""{"a":1}""", """{"b":2}""", """{"c":3}""")
  }
}
