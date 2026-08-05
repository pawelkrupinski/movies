package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Guards the ordering that keeps the ~100s FixtureServerMain boot off the
 * critical path of the jobs that need it.
 *
 * That boot is almost entirely sbt: measured warm, ~98s is JVM start, build load
 * and `runMain` classpath assembly, and ~2.4s is the application — FixtureTestWiring
 * loads the checked-in `read-model-snapshot.json` instead of recomputing the
 * ~110s pipeline. So there is nothing to make faster inside it. The only saving
 * available is to stop WAITING for it, by launching it as early as a job can and
 * doing the unrelated setup — Node, the Playwright browser, apt's dependencies,
 * the Swift toolchain, the Android SDK — while sbt loads.
 *
 * It reads as a cosmetic reordering, which is exactly why it needs a guard: put
 * the launch back next to the wait and every one of the 13 page-test rows
 * silently grows ~25s again, with nothing failing.
 */
class FixtureServerBootOverlapSpec extends AnyFlatSpec with Matchers {
  private lazy val action = RepoFile.read(".github/actions/run-page-test/action.yml")
  private lazy val mobile = RepoFile.block(RepoFile.read(".github/workflows/ci.yml"), "mobile-local-server")

  private def indexOf(text: String, marker: String): Int = {
    val i = text.indexOf(marker)
    withClue(s"`$marker` is missing entirely: ")(i should be >= 0)
    i
  }

  private def assertOrder(text: String, first: String, second: String): Unit =
    withClue(s"`$first` must come before `$second`: ") {
      indexOf(text, first) should be < indexOf(text, second)
    }

  "a page-test row" should "launch the fixture server before installing the browser, and only wait for it after" in {
    assertOrder(action, "Launch FixtureServerMain", "Install npm deps")
    assertOrder(action, "Launch FixtureServerMain", "Install Playwright browsers + system deps")
    assertOrder(action, "Install Playwright browsers + system deps", "Wait for FixtureServerMain")
  }

  it should "wait for the server before running any spec against it" in {
    assertOrder(action, "Wait for FixtureServerMain", "Run Playwright behaviour specs")
  }

  "the mobile LocalServer job" should "compile the fixture server while the toolchains download" in {
    assertOrder(mobile, "Start compiling the fixture server", "swift-actions/setup-swift")
    assertOrder(mobile, "Start compiling the fixture server", "Set up Android SDK")
    assertOrder(mobile, "Set up Gradle", "Wait for the fixture-server compile")
  }

  /**
   * Backgrounding the compile must not swallow its result. The step that
   * collects it re-reads the recorded exit status, so a compile error still
   * fails the job at a step that NAMES the compile — which is the whole reason
   * compile and boot are separate steps (a boot timeout that was really a build
   * failure sent one investigation after the server for nothing).
   */
  it should "still fail the job on a compile error, at the step that names the compile" in {
    val collect = mobile.linesIterator.dropWhile(!_.contains("Wait for the fixture-server compile")).mkString("\n")
    collect should include("/tmp/compile.status")
    collect should include("""exit "$(cat /tmp/compile.status)"""")
  }
}
