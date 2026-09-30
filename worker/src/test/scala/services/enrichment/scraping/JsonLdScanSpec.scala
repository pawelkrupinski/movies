package services.enrichment.scraping

import org.jsoup.Jsoup
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import scala.concurrent.duration.*
import scala.concurrent.{Await, Future}
import scala.jdk.CollectionConverters.*

/** The JSON-LD scan reads a page's `<script type="application/ld+json">` blocks without building
 *  its DOM. It must find exactly what a real HTML parser does, so it is held to Jsoup on every
 *  recorded Metacritic and Rotten Tomatoes page — the only pages it reads. */
class JsonLdScanSpec extends AnyFlatSpec with Matchers {

  private val Fixtures = Paths.get("test/resources/fixtures")
  private val RatingSites = Seq("www.metacritic.com", "www.rottentomatoes.com")

  /** The RECORDED pages: the ones git tracks. Walking the tree read whatever another suite running
   *  beside this one was writing into it at that moment — a page mid-write, or gone before it was
   *  read — and failed intermittently on a page nobody recorded. */
  private def ratingPages: Seq[Path] =
    scala.sys.process.Process(Seq("git", "ls-files", "-z", "--", Fixtures.toString)).!!.split('\u0000').toSeq
      .filter(path => path.nonEmpty && RatingSites.exists(path.contains))
      .map(Paths.get(_))
      .filter(Files.isRegularFile(_))

  private def byJsoup(html: String): Seq[String] =
    Jsoup.parse(html).select("script[type=application/ld+json]").asScala.toSeq.map(_.data())

  /** The pages whose scan disagrees with Jsoup. Each page is independent and Jsoup-parsing 400+
   *  full rating pages one after another was most of this suite's time, so they are compared on a
   *  small bounded pool. */
  private def differingFromJsoup(pages: Seq[Path]): Seq[String] = {
    val pool = tools.DaemonExecutors.boundedEC("json-ld-scan-spec", Runtime.getRuntime.availableProcessors.min(8))
    try {
      val checks = pages.map { page =>
        Future {
          val html = new String(Files.readAllBytes(page), StandardCharsets.UTF_8)
          Option.when(JsonLdAggregateRating.scripts(html) != byJsoup(html))(page.toString)
        }(using pool)
      }
      Await.result(Future.sequence(checks)(using implicitly, pool), 2.minutes).flatten
    } finally pool.shutdown()
  }

  "the JSON-LD scan" should "find the same blocks as Jsoup on every recorded rating-site page" in {
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
    JsonLdAggregateRating.scripts(html) shouldBe byJsoup(html)
    JsonLdAggregateRating.scripts(html) shouldBe Seq("""{"a":1}""", """{"b":2}""", """{"c":3}""")
  }
}
