package tools

import testsupport.SourceTrees
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import ScalaSourceScan.{code, read, scalaFiles}

import scala.util.matching.Regex

/**
 * Production code never asks a scraper what CLASS it is.
 *
 * `case client: FilmwebShowtimesClient` over the catalogue (the boot's source-url resolve), `case paged:
 * PagedChunkScraper` (the chunk handler) and `case venue: MultiListingScraper` (the /uptime marker) each narrowed a
 * `CinemaScraper` by type: a decorator around the client fell through the match unnoticed, and a second client with the
 * same capability had to be added to every match by hand (CLAUDE.md, Liskov: callers never need `isInstanceOf`). A
 * capability the trait offers (`sourceUrlResolving`, `pagedChunks`, `clientName`, `chunkedScraper`, `listingFeed`) is
 * the typed question instead. This refuses a type pattern or `isInstanceOf` on any `…Client` / `…Scraper` type in
 * `src/main`; a nested type (`OMDbClient.BudgetSpentException`) is not one.
 */
class ScraperTypeTestLintSpec extends AnyFlatSpec with Matchers {

  private val Roots = SourceTrees("*/src/main")

  private val TypeTest: Regex =
    """(?:\bcase\s+[\w`]+\s*:\s*|\bisInstanceOf\s*\[\s*|\basInstanceOf\s*\[\s*)(?:[\w.]+\.)?([A-Z]\w*(?:Client|Scraper))\b(?!\s*\.)""".r

  private[tools] def typeTests(source: String): Seq[String] =
    source.linesIterator.map(code).flatMap(line => TypeTest.findAllMatchIn(line).map(_.group(1))).toSeq

  "Production code" should "never narrow a scraper by its class — ask the trait's capability instead" in {
    val offenders = scalaFiles(Roots).flatMap(path => typeTests(read(path)).map(found => s"$path: $found"))
    withClue("these type-test a scraper or client; give CinemaScraper a typed capability and ask it:\n  " +
      offenders.mkString("\n  ") + "\n") {
      offenders shouldBe empty
    }
  }

  "The lint" should "see every way to type-test a scraper, and not a nested type or a comment" in {
    val source =
      """scrapers.collect { case client: FilmwebShowtimesClient => client }
        |scraper match { case paged: PagedChunkScraper => paged; case _: services.cinemas.common.MultiListingScraper => () }
        |if (s.isInstanceOf[CinemaCityScraper]) s.asInstanceOf[CinemaCityScraper]
        |failures.collect { case e: OMDbClient.BudgetSpentException => e }
        |// case client: FilmwebShowtimesClient =>
        |val client: FilmwebShowtimesClient = make()""".stripMargin
    typeTests(source) shouldBe Seq("FilmwebShowtimesClient", "PagedChunkScraper", "MultiListingScraper",
      "CinemaCityScraper", "CinemaCityScraper")
  }
}
