package services.enrichment.scraping

import tools.SpecTimeouts

import org.jsoup.Jsoup
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import scala.concurrent.{Await, Future}
import scala.jdk.CollectionConverters.*

/** The JSON-LD scan reads a page's `<script type="application/ld+json">` blocks without building
 *  its DOM, and Rotten Tomatoes' scorecard its `<script id="media-scorecard-json">` island. Each must
 *  find exactly what a real HTML parser does, so both are held to Jsoup on every recorded Metacritic
 *  and Rotten Tomatoes page — the only pages they read. */
class JsonLdScanSpec extends AnyFlatSpec with Matchers {

  private val Fixtures = Paths.get("test/resources/fixtures")
  private val RatingSites = Seq("www.metacritic.com", "www.rottentomatoes.com")

  /** The recorded pages: the regular files under the fixtures directory, sorted, for the two rating sites. Listed
   *  by walking the directory, as no unit spec starts a process (`git ls-files` named the same files). */
  private def ratingPages: Seq[Path] = {
    val walk = Files.walk(Fixtures)
    try walk.iterator.asScala.filter(Files.isRegularFile(_)).toSeq
      .filter(path => RatingSites.exists(path.toString.contains))
      .sortBy(_.toString)
    finally walk.close()
  }

  private def byJsoup(html: String): Seq[String] = JsonLdBlocks.scripts(Jsoup.parse(html))

  private def scorecardByJsoup(html: String): Seq[String] =
    Jsoup.parse(html).select("script#media-scorecard-json").asScala.toSeq.map(_.data())

  /** The pages whose scan disagrees with Jsoup. Each page is independent and Jsoup-parsing 400+
   *  full rating pages one after another was most of this suite's time, so they are compared on a
   *  small bounded pool. */
  private def differingFromJsoup(pages: Seq[Path]): Seq[String] = {
    val pool = tools.DaemonExecutors.boundedEC("json-ld-scan-spec", Runtime.getRuntime.availableProcessors.min(8))
    try {
      val checks = pages.map { page =>
        Future {
          val html = new String(Files.readAllBytes(page), StandardCharsets.UTF_8)
          Option.when(JsonLdBlocks.scripts(html) != byJsoup(html) ||
            RottenTomatoesScorecard.blocks(html) != scorecardByJsoup(html))(page.toString)
        }(using pool)
      }
      Await.result(Future.sequence(checks)(using implicitly, pool), SpecTimeouts.Run).flatten
    } finally pool.shutdown()
  }

  "the JSON-LD and scorecard scans" should "find the same blocks as Jsoup on every recorded rating-site page" in {
    withClue(s"$Fixtures must exist (run from the repo root)")(Files.isDirectory(Fixtures) shouldBe true)
    val pages = ratingPages
    pages.size should be > 400
    val differing = differingFromJsoup(pages)
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
    JsonLdBlocks.scripts(html) shouldBe byJsoup(html)
    JsonLdBlocks.scripts(html) shouldBe Seq("""{"a":1}""", """{"b":2}""", """{"c":3}""")
  }

  "JsonLdBlocks.parsed" should "give the blocks that are JSON, in page order, from a raw page and a parsed one alike" in {
    val html =
      """<script type="application/ld+json">{"a":1}</script>
        |<script type="application/ld+json">not json {</script>
        |<script type="application/ld+json"></script>
        |<script type="application/ld+json">[{"b":2}]</script>""".stripMargin
    val expected = Seq(play.api.libs.json.Json.obj("a" -> 1), play.api.libs.json.Json.arr(play.api.libs.json.Json.obj("b" -> 2)))
    JsonLdBlocks.parsed(html) shouldBe expected
    JsonLdBlocks.parsed(Jsoup.parse(html)) shouldBe expected
  }

  "the scorecard scan" should "match the id however its name is cased or its value quoted, and nothing else" in {
    val html =
      """<script id="media-scorecard-json" type="application/json">{"a":1}</script>
        |<script ID='media-scorecard-json'>{"b":2}</script>
        |<script type="application/json" id=media-scorecard-json>{"c":3}</script>
        |<script data-id="media-scorecard-json">{"d":4}</script>
        |<script id="media-scorecard-jsonx">{"e":5}</script>
        |<script id="Media-Scorecard-Json">{"f":6}</script>""".stripMargin
    RottenTomatoesScorecard.blocks(html) shouldBe scorecardByJsoup(html)
    RottenTomatoesScorecard.blocks(html) shouldBe Seq("""{"a":1}""", """{"b":2}""", """{"c":3}""")
  }
}
