package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Paths}

/**
 * Test sources do not queue work on `ExecutionContext.global`.
 *
 * The class of failure: a spec queued work on the global pool and then waited a bounded time for
 * it (an `Await`, an `eventually`, a latch). Other suites in the same test JVM keep that pool busy
 * and a CI runner has only a few threads in it, so the work started after the wait had run out
 * (PosterMemoryCapSpec, MongoAuthExchangeCodeStoreIntegrationSpec, ShareCardPostersSpec,
 * RefreshingSnapshotSpec). The repair is [[DedicatedThreads]] — a thread per task — or an executor
 * the spec owns.
 *
 * Whether a bounded wait follows is not something a text scan can tell, so the rule is the
 * mention: any use of the global pool is listed here with the reason it cannot starve a wait.
 * The list only shrinks: an entry whose file stopped using `global` fails until it is dropped.
 */
class NoGlobalPoolInTestsSpec extends AnyFlatSpec with Matchers {

  import ScalaSourceScan.{TestRoots, codeOf, scalaFiles}

  private val SelfTest = "its self-test feeds the matcher these shapes as string literals"

  private val Allowlist: Map[String, String] = Map(
    "worker/src/test/scala/tools/NoGlobalPoolInTestsSpec.scala" -> SelfTest,
    "common/src/test/scala/tools/IndependentCases.scala" ->
      "CPU-bound cases awaited with no bound: a busy pool only makes it slower, never time out",
    "common/src/test/scala/services/attempts/FilmAttemptReportSpec.scala" ->
      "the one use is a film with no tmdbId, which builds its report without scheduling any work",
    "web/src/test/scala/controllers/TestAdminAction.scala" ->
      "the executor Play's action runs a handler on; the controller specs read already-completed futures",
    "web/src/test/scala/controllers/UnreadableSignedInUserSpec.scala" ->
      "an implicit for already-completed futures read through Play's synchronous test helpers",
    "worker/src/test/scala/clients/tools/MeasureStartup.scala" ->
      "a runnable program against the live world, not a spec: no test bound to run out")

  private val GlobalPool = """\bExecutionContext\.(?:global|Implicits\.global)\b""".r

  private[tools] def mentionsGlobalPool(src: String): Boolean = GlobalPool.findFirstIn(src).isDefined

  "the matcher" should "catch every way of naming the global pool" in {
    mentionsGlobalPool("import scala.concurrent.ExecutionContext.Implicits.global") shouldBe true
    mentionsGlobalPool("using ExecutionContext.global") shouldBe true
    mentionsGlobalPool("scala.concurrent.ExecutionContext.global.execute(task)") shouldBe true
    mentionsGlobalPool("ExecutionContext.fromExecutor(pool)") shouldBe false
    mentionsGlobalPool("given ExecutionContext = tools.DedicatedThreads") shouldBe false
  }

  "test sources" should "not use the global pool outside the allowlist" in {
    val found = scalaFiles(TestRoots).filterNot(p => Allowlist.contains(p.toString)).filter(p => mentionsGlobalPool(codeOf(p)))
    withClue("These files use ExecutionContext.global, which other suites keep busy. Use tools.DedicatedThreads " +
      "(a thread per task) or an executor the spec owns — or allowlist the file with a reason:\n" + found.mkString("\n") + "\n") {
      found shouldBe empty
    }
  }

  it should "keep every allowlist entry pointing at a file that still uses it" in {
    val stale = Allowlist.keys.toSeq.sorted.filterNot { file =>
      val path = Paths.get(file)
      Files.exists(path) && mentionsGlobalPool(codeOf(path))
    }
    withClue("Allowlisted but no longer using the global pool — drop the entry:\n" + stale.mkString("\n") + "\n") {
      stale shouldBe empty
    }
  }
}
