package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.Path

/**
 * Unit specs start no Mongo and no browser (the owner, 2026-10-27: "unit tests shouldn't have mongo or browser").
 *
 * A spec that needs a real Mongo (a query, an index, a change stream) is an integration spec (an `it` tree, the
 * `integration-test` rows, which provide the database); one that needs a real Chrome is a page spec (`web/src/page`).
 * The unit configs (each module's src/test tree, testkit's own specs) run on every push in the deploy-gating rows, which have neither.
 *
 * Rules, each naming file:line:
 *
 *  1. No unit-tree source names a helper that connects to a provided database (testkit's [[IntegrationMongo]] and the
 *     databases built on it) or drives a browser (the page tree's CDP driver, Playwright, Selenium), nor an embedded
 *     database or container (Testcontainers, flapdoodle).
 *  2. A Mongo client a unit spec builds names a DEAD port, `mongodb://127.0.0.1:1`: the driver connects lazily, so
 *     such a client only ever fails — which is what those specs assert (an unreachable store's outcome, a write that
 *     must not be attempted). A client to a live address would reach whatever mongod the machine runs.
 *
 * Runnable programs under the test trees (`ScalaSourceScan.isProgram`: the capture recorder, the startup bench) are
 * operator tools pointed at a database by their caller; they assert nothing and stay exempt, as in
 * [[NoSleepInTestsSpec]].
 */
class UnitSpecsNeedNoMongoOrBrowserSpec extends AnyFlatSpec with Matchers {

  import ScalaSourceScan.{codeOf, isProgram, read, scalaFiles}

  /** The unit configs' trees: what `testUnit` compiles and runs. (testkit's main code holds the integration helpers
   *  themselves, for the it/ trees.) */
  private val UnitRoots =
    Seq("common/src/test", "identity/src/test", "scrapers/src/test", "testkit/src/test", "web/src/test", "worker/src/test")

  /** The helpers that connect or launch. Spelled as strings so this file's own code (strings stripped) names none. */
  private val Connecting = Seq(
    "IntegrationMongo", "IsolatedMongoDatabase", "IntegrationCorpusDatabase", "CdpDriver",
    "com.microsoft.playwright", "org.openqa.selenium", "org.testcontainers", "de.flapdoodle")
  private val ConnectingRef = Connecting.map(java.util.regex.Pattern.quote).mkString("""\b(?:""", "|", """)\b""").r

  /** Unit specs that name a helper without connecting, and why. */
  private val Allowlist: Map[String, String] = Map(
    "testkit/src/test/scala/tools/IntegrationMongoSpec.scala" ->
      "tests IntegrationMongo's pure URI classification and redaction; it opens no connection")

  private val StringLiteral = "\"\"\"(?s:.*?)\"\"\"|\"(?:[^\"\\\\\\n]|\\\\.)*\"".r
  private val ClientBuilt   = """\b(?:MongoClient|MongoClients\.create)\(\s*""".r
  private val DeadPort      = "\"mongodb://127.0.0.1:1(?:[/?\"])"

  /** `src` with every string literal's content blanked, offsets and line numbers preserved. */
  private[tools] def withoutStrings(src: String): String =
    StringLiteral.replaceAllIn(src, m => java.util.regex.Matcher.quoteReplacement(
      m.matched.zipWithIndex.map((c, i) => if (c == '\n' || c == '"' && (i < 3 || i >= m.matched.length - 3)) c else ' ').mkString))

  private def lineOf(src: String, at: Int) = src.substring(0, at).count(_ == '\n') + 1

  /** The 1-based lines of `src` (comment-free) naming a connecting helper outside a string. */
  private[tools] def connectingLines(src: String): Seq[Int] = {
    val code = withoutStrings(src)
    ConnectingRef.findAllMatchIn(code).map(m => lineOf(code, m.start)).toSeq.distinct
  }

  /** The 1-based lines of `src` (comment-free) building a Mongo client to anything but the dead port. */
  private[tools] def liveClientLines(src: String): Seq[Int] =
    ClientBuilt.findAllMatchIn(withoutStrings(src)).collect {
      case m if !src.startsWith("\"", m.end) || DeadPort.r.findPrefixOf(src.substring(m.end)).isEmpty => lineOf(src, m.start)
    }.toSeq.distinct

  /** This spec: its self-test feeds the matchers these shapes as string literals. */
  private val Self = "worker/src/test/scala/tools/UnitSpecsNeedNoMongoOrBrowserSpec.scala"

  private lazy val sources: Seq[(Path, String)] =
    scalaFiles(UnitRoots).filterNot(isProgram).filterNot(_.toString == Self).map(p => p -> codeOf(p))

  private def offenders(lines: String => Seq[Int], allow: Map[String, String]): Seq[String] =
    sources.filterNot { case (p, _) => allow.contains(p.toString) }.flatMap { case (p, src) =>
      val raw = read(p).linesIterator.toIndexedSeq
      lines(src).map(n => s"$p:$n: ${raw(n - 1).trim}")
    }

  "the matchers" should "catch a connecting helper or a live client, and pass names in strings and dead-port clients" in {
    connectingLines("val db = IsolatedMongoDatabase.create()") shouldBe Seq(1)
    connectingLines("\nimport com.microsoft.playwright.Page") shouldBe Seq(2)
    connectingLines("""val why = "the page tree's CdpDriver sleeps"""") shouldBe empty
    connectingLines("val IntegrationMongoUri = 1") shouldBe empty
    liveClientLines("""MongoClient("mongodb://localhost:27017")""") shouldBe Seq(1)
    liveClientLines("MongoClient(uri)") shouldBe Seq(1)
    liveClientLines("""MongoClients.create("mongodb://127.0.0.10:1")""") shouldBe Seq(1)
    liveClientLines("""org.mongodb.scala.MongoClient("mongodb://127.0.0.1:1") // never connected""") shouldBe empty
    liveClientLines("""MongoClient("mongodb://127.0.0.1:1/?serverSelectionTimeoutMS=200")""") shouldBe empty
  }

  "unit test sources" should "name no helper that connects to a database or drives a browser" in {
    sources.size should be > 100
    val found = offenders(connectingLines, Allowlist)
    withClue("A unit spec needing a real Mongo belongs in an it tree, one needing a real browser in web/src/page; " +
      "otherwise test the logic against an in-memory fake (testkit's InMemory* stores):\n" + found.mkString("\n") + "\n") {
      found shouldBe empty
    }
  }

  it should "keep every allowlist entry pointing at a file that still names one" in {
    val stale = Allowlist.keys.toSeq.sorted.filterNot(f => sources.exists { case (p, src) => p.toString == f && connectingLines(src).nonEmpty })
    withClue("Allowlisted but naming no connecting helper any more — drop the entry:\n" + stale.mkString("\n") + "\n")(stale shouldBe empty)
  }

  it should "build Mongo clients only to the dead port" in {
    val found = offenders(liveClientLines, Map.empty)
    withClue("Point the client at \"mongodb://127.0.0.1:1\" (it fails, never connects), or move the spec to an it tree:\n" +
      found.mkString("\n") + "\n") {
      found shouldBe empty
    }
  }
}
