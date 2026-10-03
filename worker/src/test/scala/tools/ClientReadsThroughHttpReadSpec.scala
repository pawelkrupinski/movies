package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import ScalaSourceScan.{code, read, scalaFiles}

import scala.util.matching.Regex

/**
 * Client code reads an upstream only through `tools.HttpRead` (or `ReadOutcome`).
 *
 * A raw `http.get(url)` hands back a bare `String`, and every way of turning that string
 * into data took a guess about what a failure looks like — the guess that recurred as a
 * bug in OMDb, IMDb, Filmweb, Rotten Tomatoes, biletyna, iKsoris, Helios and more: a
 * block, a 5xx, a challenge page or an error document read as "no data". `HttpRead`
 * answers with a `ReadOutcome` (answered / absent / failed) after checking that a 2xx body
 * is the content the endpoint serves, so the guess is made once.
 *
 * Scope: the enrichment clients, the cinema scrapers and `TmdbClient`. Files not yet
 * migrated are listed in [[NotYetMigrated]] (`TODO-HttpRead`, nothing new may join it);
 * a file whose raw read is right on its own terms goes in [[Allowlist]] with WHY.
 */
class ClientReadsThroughHttpReadSpec extends AnyFlatSpec with Matchers {

  private val Roots = Seq("worker/src/main/scala/services/enrichment", "worker/src/main/scala/services/cinemas")
  private val Files = Seq("worker/src/main/scala/services/TmdbClient.scala")

  /** A call on a receiver named like a fetch (`http`, `httpFetch`, `bnFetch`, `detailFetch`…). */
  private val RawRead: Regex = """\b\w*(?:[Hh]ttp|[Ff]etch)\w*\s*\.\s*(?:get|getBytes|post|getAsync)\s*\(""".r

  /** File → why its raw read is right. */
  private val Allowlist: Map[String, String] = Map(
    "worker/src/main/scala/services/cinemas/common/VueCinemasPlatformClient.scala" ->
      "the token POST only mints a session cookie; its answer is never read, and the films GET after it is read through HttpRead.page and fails the scrape on its own"
  )

  private def rawReads(source: String): Int =
    source.linesIterator.map(code).map(line => RawRead.findAllIn(line).size).sum

  private lazy val found: Map[String, Int] =
    (scalaFiles(Roots).map(_.toString) ++ Files.filter(f => java.nio.file.Files.exists(java.nio.file.Paths.get(f))))
      .map(f => f -> rawReads(read(java.nio.file.Paths.get(f)))).filter(_._2 > 0).toMap

  private val Excused: Map[String, String] = Allowlist ++ HttpReadBacklog.NotYetMigrated

  "Client code" should "read HTTP only through HttpRead, outside the allowlist and the migration backlog" in {
    val offenders = found.keySet.filterNot(Excused.contains).toSeq.sorted
    withClue(s"${offenders.size} file(s) read an upstream raw — read through tools.HttpRead (see its doc):\n  " +
      offenders.mkString("\n  ") + "\n") {
      offenders shouldBe empty
    }
  }

  "The allowlist and the backlog" should "name only files that still read raw" in {
    val stale = Excused.keySet.filterNot(found.contains).toSeq.sorted
    withClue(s"stale entries (the file moved onto HttpRead or went — drop them): ${stale.mkString(", ")} — ") {
      stale shouldBe empty
    }
  }

  "The lint" should "see a raw read on any fetch-named receiver, and not one in a comment or through HttpRead" in {
    val src =
      """class A(http: HttpFetch, bnFetch: HttpFetch) {
        |  val a = http.get(url)
        |  val b = bnFetch.post(url, body)
        |  val c = detailFetch.getBytes(url)
        |  // http.get(url)
        |  val d = HttpRead.text(http, url)(Answered(_))
        |  val e = cache.get(key)
        |}""".stripMargin
    rawReads(src) shouldBe 3
  }
}
