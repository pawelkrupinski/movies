package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.Path

/**
 * Unit specs reach no real dependency (the owner, 2026-10-27: "unit tests have all external deps mocked, so those with
 * real deps move to it or e2e"; "if real dep valuable, just move to it").
 *
 * The build holds most of it structurally: what reaches a real thing -- a Mongo cluster, a child JVM, git, keytool, the
 * repository's scripts -- lives in the `itkit` module, and project/UnitClasspathGuard.scala refuses a unit Test
 * classpath holding itkit or a library that starts a database or drives a browser. What a classpath cannot see is a
 * PRODUCTION class or a JDK API that talks to the world, on every classpath through main; this lint holds those, in
 * every unit tree (each module's src/test, testkit's own specs), each rule naming file:line:
 *
 *  1. CONNECTING HELPERS: no unit-tree source names one (itkit's, the page tree's CDP driver, Playwright, Selenium,
 *     Testcontainers, flapdoodle).
 *  2. MONGO: a client a unit spec builds names a DEAD port, `mongodb://127.0.0.1:1`. The driver connects lazily, so
 *     such a client only ever fails -- which is what those specs assert (an unreachable store's outcome).
 *  3. PROCESSES: nothing spawned (`ProcessBuilder`, `scala.sys.process`, `Runtime.exec`).
 *  4. NETWORK: no real HTTP client or socket built or opened (`RealHttpFetch`, the JDK's `HttpClient`, `URL`
 *     connections, sockets, name lookups) but by the allowlisted specs, each of which talks to an IN-PROCESS fake
 *     server or sends nothing.
 *  5. ENVIRONMENT: no read of the process's environment (`sys.env`, `System.getenv`, `ProcessConfiguration.resolve()`):
 *     a spec builds the configuration it needs (`Env.of`, `Env.layered`).
 *  6. MONOTONIC CLOCK: no `Stopwatch.System` handed to the code under test: a spec steps its own (testkit's
 *     SpecStopwatch).
 *
 * Wall clocks and sleeps have their own lints ([[NoWallClockInTestsSpec]], [[NoSleepInTestsSpec]],
 * [[NoLiteralWaitBoundSpec]]). Reading the repository's own checked-in files (fixtures, recorded responses, the
 * sources and workflows the lint and deploy specs read) is reading the test's inputs, versioned with it, not a
 * dependency; writes go to a temp directory. Runnable programs under the test trees (`ScalaSourceScan.isProgram`:
 * the recorders, probes, backfills) are operator tools that reach the world on purpose; they assert nothing and stay
 * exempt, as in [[NoSleepInTestsSpec]].
 */
class NoRealDependenciesInUnitSpecsSpec extends AnyFlatSpec with Matchers {

  import ScalaSourceScan.{codeOf, isProgram, read, scalaFiles}
  import NoRealDependenciesInUnitSpecsSpec._

  private lazy val sources: Seq[(Path, String)] =
    scalaFiles(UnitRoots).filterNot(isProgram).filterNot(_.toString == Self).map(p => p -> codeOf(p))

  private def offenders(lines: String => Seq[Int], allow: Map[String, String]): Seq[String] =
    sources.filterNot { case (p, _) => allow.contains(p.toString) }.flatMap { case (p, src) =>
      val raw = read(p).linesIterator.toIndexedSeq
      lines(src).map(n => s"$p:$n: ${raw(n - 1).trim}")
    }

  "the matchers" should "catch each real dependency, and pass names in strings and dead-port clients" in {
    connecting.lines("val db = IsolatedMongoDatabase.create()") shouldBe Seq(1)
    connecting.lines("\nimport com.microsoft.playwright.Page") shouldBe Seq(2)
    connecting.lines("""val why = "the page tree's CdpDriver sleeps"""") shouldBe empty
    connecting.lines("val IntegrationMongoUri = 1") shouldBe empty
    liveMongoLines("""MongoClient("mongodb://localhost:27017")""") shouldBe Seq(1)
    liveMongoLines("MongoClient(uri)") shouldBe Seq(1)
    liveMongoLines("""MongoClients.create("mongodb://127.0.0.10:1")""") shouldBe Seq(1)
    liveMongoLines("""org.mongodb.scala.MongoClient("mongodb://127.0.0.1:1") // never connected""") shouldBe empty
    liveMongoLines("""MongoClient("mongodb://127.0.0.1:1/?serverSelectionTimeoutMS=200")""") shouldBe empty
    processes.lines("""new ProcessBuilder("bash", "x.sh").start()""") shouldBe Seq(1)
    processes.lines("import scala.sys.process.*") shouldBe Seq(1)
    processes.lines("Runtime.getRuntime.exec(cmd)") shouldBe Seq(1)
    processes.lines("ProcessHandle.current().pid()") shouldBe empty
    network.lines("new TmdbClient(new RealHttpFetch, apiKey = None)") shouldBe Seq(1)
    network.lines("HttpClient.newHttpClient().send(request, handler)") shouldBe Seq(1)
    network.lines("URI.create(url).toURL.openConnection()") shouldBe Seq(1)
    network.lines("InetAddress.getByName(host)") shouldBe Seq(1)
    network.lines("val fetch = new FakeHttpFetch(Map.empty)") shouldBe empty
    environment.lines("sys." + "env.get(\"MONGODB_URI\")") shouldBe Seq(1)   // split: ProcessAccessLintSpec reads strings too
    environment.lines("val c = ProcessConfiguration.resolve()") shouldBe Seq(1)
    environment.lines("ProcessConfiguration.resolve(Env.of(\"A\" -> \"1\"))") shouldBe empty
    clock.lines("new LogThrottle(100, Stopwatch.System)") shouldBe Seq(1)
    clock.lines("new LogThrottle(100, SpecStopwatch.Frozen)") shouldBe empty
  }

  Rules.foreach { rule =>
    "unit test sources" should s"reach no real ${rule.name}" in {
      sources.size should be > 100
      val found = offenders(rule.lines, rule.allow)
      withClue(s"${rule.hint}\n${found.mkString("\n")}\n")(found shouldBe empty)
    }

    it should s"keep every ${rule.name} allowlist entry pointing at a file that still needs it" in {
      val stale = rule.allow.keys.toSeq.sorted.filterNot(f => sources.exists { case (p, src) => p.toString == f && rule.lines(src).nonEmpty })
      withClue("Allowlisted but no longer reaching it -- drop the entry:\n" + stale.mkString("\n") + "\n")(stale shouldBe empty)
    }
  }

  "unit test sources" should "build Mongo clients only to the dead port" in {
    val found = offenders(liveMongoLines, Map.empty)
    withClue("Point the client at \"mongodb://127.0.0.1:1\" (it fails, never connects), or move the spec to an it tree:\n" +
      found.mkString("\n") + "\n")(found shouldBe empty)
  }
}

object NoRealDependenciesInUnitSpecsSpec {

  /** The unit configs' trees: what `testUnit` compiles and runs. */
  val UnitRoots: Seq[String] =
    Seq("base/src/test", "http/src/test", "http/src/fixtures", "common/src/test", "identity/src/test", "scrapers/src/test", "scrapers/src/fixtures", "testkit/src/test", "web/src/test", "worker/src/test")

  /** This spec: its self-test feeds the matchers these shapes as string literals. */
  private val Self = "worker/src/test/scala/tools/NoRealDependenciesInUnitSpecsSpec.scala"

  private val StringLiteral = "\"\"\"(?s:.*?)\"\"\"|\"(?:[^\"\\\\\\n]|\\\\.)*\"".r

  /** `src` with every string literal's content blanked, offsets and line numbers preserved. */
  def withoutStrings(src: String): String =
    StringLiteral.replaceAllIn(src, m => java.util.regex.Matcher.quoteReplacement(
      m.matched.zipWithIndex.map((c, i) => if (c == '\n' || c == '"' && (i < 3 || i >= m.matched.length - 3)) c else ' ').mkString))

  private def lineOf(src: String, at: Int) = src.substring(0, at).count(_ == '\n') + 1

  /** One rule: what it finds in code (strings blanked), the files allowed it and why, and the fix it names. */
  final case class Rule(name: String, pattern: scala.util.matching.Regex, allow: Map[String, String], hint: String) {

    /** The 1-based lines of `src` (comment-free) the rule finds. */
    def lines(src: String): Seq[Int] = {
      val code = withoutStrings(src)
      pattern.findAllMatchIn(code).map(m => lineOf(code, m.start)).toSeq.distinct
    }
  }

  /** A helper only the runnable programs use (a recorder's live fetch, a tool's database), never a spec. */
  private val ToolOnly = "used only by the runnable programs beside it (operator tools, exempt), never by a spec"

  /** CaptureReplay, which specs and the refit/discovery tools share. */
  private val CaptureReplayLive =
    "asks TMDB live only in `all()`, the tools' entry, when the process holds a key; every spec builds its replay with " +
      "`live = None`, answered from the capture alone"

  private def words(names: String*) = names.map(java.util.regex.Pattern.quote).mkString("""\b(?:""", "|", """)\b""").r

  val connecting: Rule = Rule("connecting helper",
    words("IntegrationMongo", "IsolatedMongoDatabase", "IntegrationCorpusDatabase", "IntegrationMongoSuite", "OplogWrites",
      "QueryPlans", "ConcurrentInstances", "ChildJvm", "Keytool", "ScratchGitRepository", "ScriptTests", "TcpForwarder",
      "CdpDriver", "com.microsoft.playwright", "org.openqa.selenium", "org.testcontainers", "de.flapdoodle"),
    Map.empty,
    "A unit spec needing a real Mongo, process or browser belongs in an it tree (or web/src/page); otherwise test the " +
      "logic against an in-memory fake (testkit's InMemory* stores):")

  val processes: Rule = Rule("process",
    """\bProcessBuilder\b|\bsys\.process\b|Runtime\.getRuntime(?:\(\))?\.exec\b""".r,
    Map.empty,
    "A spec whose value is the real script, git or JVM belongs in an it tree; otherwise call the code in-process:")

  val network: Rule = Rule("network",
    """\bnew RealHttpFetch\b|\bRealHttpFetch\(|\bHttpClient\.new(?:HttpClient|Builder)\b|\bnew (?:java\.net\.)?Socket\(|\.openConnection\(|\.openStream\(|\bInetAddress\.getBy""".r,
    Map(
      "http/src/test/scala/tools/RealHttpFetchSpec.scala" ->
        "RealHttpFetch IS the code under test; every exchange is with a server the spec starts in-process",
      "common/src/test/scala/tools/HostPoliciesSpec.scala" -> "builds a request with RealHttpFetch and reads it; sends nothing",
      "scrapers/src/test/scala/clients/zyte/ZyteFetchSpec.scala" ->
        "hands the ZyteClient under test a JDK HttpClient its subclass never calls: every send is overridden",
      "web/src/test/scala/controllers/RequestHeaderSizeSpec.scala" ->
        "asks a Play server the spec starts in-process on 127.0.0.1:0 -- the header limit is the server's own",
      "worker/src/test/scala/modules/HealthEndpointSpec.scala"   -> "asks the worker's ops server, started in-process on a free port",
      "worker/src/test/scala/modules/HeapDumpEndpointSpec.scala" -> "asks the worker's ops server, started in-process on a free port",
      "worker/src/test/scala/modules/ProfileEndpointSpec.scala"  -> "asks the worker's ops server, started in-process on a free port",
      "scrapers/src/fixtures/scala/clients/tools/RecordMissingFetch.scala" -> ToolOnly,
      "worker/src/test/scala/scripts/CaptureReplay.scala" -> CaptureReplayLive),
    "Hand the code a testkit HttpFetch fake (FakeHttpFetch, FailingHttpFetch, ScriptedByUrlHttpFetch) fed recorded " +
      "responses, or serve it in-process (tools.AnsweringHttpServer) -- or move the spec to an it tree:")

  val environment: Rule = Rule("environment",
    """\bsys\.env\b|\bSystem\.getenv\b|\bProcessConfiguration\.resolve\(\)""".r,
    Map(
      "worker/src/test/scala/scripts/CaptureReplay.scala" -> CaptureReplayLive,
      "worker/src/test/scala/scripts/CountryDatabase.scala" -> ToolOnly),
    "Build the configuration the spec needs (settings.Env.of / Env.layered) instead of reading the process's:")

  val clock: Rule = Rule("monotonic clock",
    """\bStopwatch\.System\b""".r,
    Map(
      "common/src/test/scala/tools/ManagedResourcesSpec.scala" -> (
        "\"spend one grace across every stuck executor\": the bound under test is the JDK's own timed awaitTermination, " +
          "which no stepped clock drives; the spec asserts the real time it took stays within the grace"),
      "worker/src/test/scala/scripts/AmbientMovieRepository.scala" -> (
        "the read-and-patch scripts' store, metering their Mongo calls; its one spec opens it unconfigured (no MONGODB_URI), " +
          "where no call is made"),
      "worker/src/test/scala/tools/DrainablePoolSpec.scala" -> (
        "\"drain within its budget\": stop(200 ms) bounds a JDK timed wait on a task that never ends; a frozen stopwatch " +
          "would never spend the budget and the spec would hang")),
    "Hand the code testkit's SpecStopwatch (Frozen, or stepped from a MutableClock) instead of the JVM's nanoTime:")

  val Rules: Seq[Rule] = Seq(connecting, processes, network, environment, clock)

  private val ClientBuilt = """\b(?:MongoClient|MongoClients\.create)\(\s*""".r
  private val DeadPort    = "\"mongodb://127.0.0.1:1(?:[/?\"])".r

  /** The 1-based lines of `src` (comment-free) building a Mongo client to anything but the dead port. */
  def liveMongoLines(src: String): Seq[Int] =
    ClientBuilt.findAllMatchIn(withoutStrings(src)).collect {
      case m if !src.startsWith("\"", m.end) || DeadPort.findPrefixOf(src.substring(m.end)).isEmpty => lineOf(src, m.start)
    }.toSeq.distinct
}
