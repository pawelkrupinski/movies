package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import ScalaSourceScan.{code, read, scalaFiles}

import scala.util.matching.Regex

/**
 * A scraper never declares its listing complete.
 *
 * Completeness is decided from the outcome of every page the scrape read
 * (`services.cinemas.common.ListingReads`) and from the scraper's structure (a chunked
 * reduce missing chunks), in the shared wrappers alone. A client that overrode
 * `listingIsComplete`, built its own `Scraped`/`PreScrapedCinemaScraper`, or answered
 * `fetchWithSource` itself could claim "complete" for a listing whose page failed — and the
 * cache's prune would retire the films that page held. Only the files in [[Allowlist]] —
 * the wrappers that compose completeness, each with WHY — may touch those members.
 */
class ListingCompletenessSpec extends AnyFlatSpec with Matchers {

  private val Root = "worker/src/main/scala/services/cinemas"

  private val Declares: Regex =
    """\b(?:override\s+(?:def|val|lazy\s+val)\s+listingIsComplete|override\s+def\s+fetchWithSource|PreScrapedCinemaScraper\s*[.(]|CinemaScraper\s*\.\s*Scraped\s*\(|\bScraped\s*\()""".r

  private val Allowlist: Map[String, String] = Map(
    s"$Root/common/CinemaScraper.scala" ->
      "the contract: defines listingIsComplete, Scraped and the default fetchWithSource, which reads completeness off ListingReads",
    s"$Root/common/CinemaScrapeRunner.scala" ->
      "consumes the verdict: destructures Scraped and ANDs it with the scraper's structure; never builds one",
    s"$Root/common/DelegatingCinemaScraper.scala" ->
      "forwards the delegate's own answer unchanged — it can only pass a verdict through, never raise one",
    s"$Root/common/MultiListingScraper.scala" ->
      "complete only when every listing it composes is (forall) — it can only lower the verdict",
    s"$Root/common/PreScrapedCinemaScraper.scala" ->
      "how the chunk reduce hands its listing to the runner; its completeness is computed from missing chunks and incomplete-read markers (ScrapeChunkReduceHandler)",
    s"$Root/common/SourceFallbackScraper.scala" ->
      "reports the SERVING source's ListingReads verdict per tick; it reads both scopes, it does not invent one"
  )

  private[tools] def declarations(source: String): Int =
    source.linesIterator.map(code).map(line => Declares.findAllIn(line).size).sum

  private lazy val found: Map[String, Int] =
    scalaFiles(Seq(Root)).map(_.toString).map(f => f -> declarations(read(java.nio.file.Paths.get(f)))).filter(_._2 > 0).toMap

  "Cinema clients" should "never declare a listing complete — only the composing wrappers touch completeness" in {
    val offenders = found.keySet.filterNot(Allowlist.contains).toSeq.sorted
    withClue(s"these files declare completeness by hand; let ListingReads decide it from the page reads:\n  ${offenders.mkString("\n  ")}\n") {
      offenders shouldBe empty
    }
  }

  "The allowlist" should "name only files that still touch completeness" in {
    Allowlist.keySet.filterNot(found.contains) shouldBe empty
  }

  "The lint" should "see every way to declare completeness, and not a mention in a comment" in {
    val src =
      """class A extends CinemaScraper {
        |  override def listingIsComplete: Boolean = true
        |  override val listingIsComplete = true
        |  override def fetchWithSource() = CinemaScraper.Scraped(fetch(), viaFallback = false, complete = true)
        |  val b = PreScrapedCinemaScraper.of(this, () => Nil)
        |  // override def listingIsComplete: Boolean = true
        |  def listingIsCompleteOfOther = other.listingIsComplete
        |}""".stripMargin
    declarations(src) shouldBe 5
  }
}
