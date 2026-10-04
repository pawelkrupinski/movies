package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * A film's detail page is read through `DetailFetchOutcome.page` — one door that
 * swallows a transient failure into `None` and lets a durable 404/410 escape.
 * Thirty clients had spelled that out by hand as
 * `DetailFetchOutcome.transientToNone(HttpRead.page(http, ref))`, each with its
 * own copy of the same doc comment; the hand-rolled shape fails the build so the
 * thirty-first client reaches for the helper instead.
 */
class DetailPageReadLintSpec extends AnyFlatSpec with Matchers {

  import ScalaSourceScan._

  private val HandRolled = """transientToNone\(\s*HttpRead\.page\(""".r

  "the scanner" should "find the hand-rolled read and nothing else" in {
    HandRolled.findFirstIn("DetailFetchOutcome.transientToNone(HttpRead.page(http, ref)).map(parseDetail)") shouldBe defined
    HandRolled.findFirstIn("DetailFetchOutcome.page(http, ref).map(parseDetail)") shouldBe empty
    HandRolled.findFirstIn("DetailFetchOutcome.transientToNone(HttpRead.pageBytes(http, ref))") shouldBe empty
  }

  "worker sources" should "read a detail page only through DetailFetchOutcome.page" in {
    val sites = scalaFiles(Seq("worker/src/main/scala"))
      .filterNot(_.toString.endsWith("DetailFetchOutcome.scala"))
      .filter(path => HandRolled.findFirstIn(codeOf(path)).isDefined)
      .map(_.toString)
    withClue("Use DetailFetchOutcome.page(http, url): ")(sites shouldBe empty)
  }
}
