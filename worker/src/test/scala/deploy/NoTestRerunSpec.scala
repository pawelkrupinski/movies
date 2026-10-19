package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * NO TEST IS EVER RERUN ON RED. A failing test is investigated and fixed, never retried until
 * it passes (user, 2026-10-19: "remove all test rerunning on red, always investigate and fix
 * flakes").
 *
 * Until then three mechanisms did rerun: Playwright's `retries: 1` (a retry that passed turned
 * the row green and hid the flake), and in every failed CI test job a rerun of the failed tests
 * three times (`scripts/ci/rerun-failed-sbt.sh`, Playwright `--last-failed`) whose FLAKY verdicts
 * fed a ledger issue (`scripts/ci/flake_verdict.py`, `flake-ledger.sh`). The job stayed red, but
 * the rerun was still the answer people read instead of the failure's own report and trace.
 *
 * This fails when any of them, or a framework's own retry knob, comes back.
 */
class NoTestRerunSpec extends AnyFlatSpec with Matchers {
  private lazy val playwrightConfig = RepoFile.read("page-tests-playwright/playwright.config.ts")

  "playwright.config.ts" should "retry nothing, anywhere it runs" in {
    val code = playwrightConfig.linesIterator.filterNot(_.trim.startsWith("//")).mkString("\n")
    """retries:\s*([^,\n]+)""".r.findAllMatchIn(code).map(_.group(1).trim).toSeq shouldBe Seq("0")
  }

  it should "keep a failed test's trace without a retry to record it on" in {
    // `on-first-retry` / `on-all-retries` record only on a retry, so with none they record nothing.
    playwrightConfig should not include "retry'"
    playwrightConfig should include("trace: 'retain-on-failure'")
  }

  "the Playwright specs" should "not set retries of their own" in {
    RepoFile.listed("page-tests-playwright/tests")(_.getName.endsWith(".ts")).foreach { spec =>
      withClue(s"${spec.getPath}: ")(RepoFile.read(spec.getPath) should not include "retries")
    }
  }

  "CI" should "never rerun a failed test" in {
    val ciScripts = RepoFile.listed("scripts/ci")(_.getName.endsWith(".sh")).map(_.getPath)
    (RepoFile.ciFiles() ++ ciScripts).foreach { path =>
      val code = RepoFile.withoutComments(RepoFile.read(path))
      withClue(s"$path: ") {
        code should not include "--last-failed"
        code should not include "--retries"
        code should not include "rerun-failed"
        code should not include "flake_verdict"
        code should not include "flake-ledger"
      }
    }
  }

  "the rerun tools" should "stay deleted" in {
    Seq("scripts/ci/rerun-failed-sbt.sh", "scripts/ci/flake_verdict.py", "scripts/ci/flake-ledger.sh")
      .foreach(path => withClue(path)(RepoFile.exists(path) shouldBe false))
  }

  "the mobile test runners" should "not retry a failed test either" in {
    val scheme = RepoFile.read("ios/Kinowo.xcodeproj/xcshareddata/xcschemes/Kinowo.xcscheme")
    scheme should not include "retryOnFailure"
    scheme should not include "testRepetitionMode"
    Seq("android/app/build.gradle.kts", "android/build.gradle.kts", "android/settings.gradle.kts")
      .filter(RepoFile.exists)
      .foreach(path => withClue(s"$path: ")(RepoFile.read(path) should not include "test-retry"))
  }
}
