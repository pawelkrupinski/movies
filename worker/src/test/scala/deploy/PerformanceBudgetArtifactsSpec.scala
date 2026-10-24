package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The budget figures CI measures reach `scripts/adopt-ci-budgets`: every ci.yml row whose sbt run holds a spec that
 *  checks a `tools.costs.PerformanceBudgets` figure uploads the `performance-budgets.tsv` it wrote, from the root
 *  `target/` (unforked specs) and each module's (forked ones), on a red run too. A laptop's figures drift ~3-5% from
 *  CI's and re-measured budgets turned Main red twice on 2026-10-24 (the PerformanceBudgets header). */
class PerformanceBudgetArtifactsSpec extends AnyFlatSpec with Matchers {
  private def uploadsBudgets(text: String, name: String): Unit = {
    text should include(s"name: performance-budgets-$name")
    val step = text.substring(text.indexOf(s"name: performance-budgets-$name"))
    val lines = step.linesIterator.map(_.trim).take(6).toSeq
    lines should contain("target/performance-budgets.tsv")
    lines should contain("*/target/performance-budgets.tsv")
    lines should contain("if-no-files-found: ignore")
  }

  "the unit, integration and e2e rows" should "upload their budget figures, red or green" in {
    val jobs = RepoFile.jobs(RepoFile.read(".github/workflows/ci.yml"))
    Seq("test" -> "unit-${{ matrix.shard }}", "integration-test" -> "it-${{ matrix.row }}", "e2e" -> "e2e-${{ matrix.shard }}")
      .foreach { case (job, name) =>
        withClue(s"$job: ") {
          uploadsBudgets(jobs(job), name)
          jobs(job) should include("if: ${{ !cancelled() }}\n              uses: actions/upload-artifact@v6\n")
        }
      }
  }

  "the chrome page row" should "upload the render and browser budget figures" in {
    val action = RepoFile.read(".github/actions/run-page-test/action.yml")
    uploadsBudgets(action, "page-chrome")
    action should include("if: ${{ !cancelled() && inputs.kind == 'chrome' }}")
  }

  "adopt-ci-budgets" should "be tested in CI and named in the budgets' header" in {
    ScriptTests.runInCi("scripts/adopt_ci_budgets_test.py") shouldBe true
    RepoFile.read("testkit/src/main/scala/tools/costs/PerformanceBudgets.scala") should include("scripts/adopt-ci-budgets <run id>")
  }
}
