package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * ci.yml's page, mobile and e2e rows run only for a push that can move them (`scope`, scripts/ci/ci-row-scope.sh;
 * its classification is ci-row-scope-test.sh's). What this holds is the wiring that keeps a skip SAFE:
 *
 *  - every gated row tests `!= 'false'` under `!cancelled()`, so a `scope` job that failed or said nothing runs the
 *    row rather than skipping it (an implicit `success()` would have skipped every row and left the run green);
 *  - the Scala page row (`chrome / stable`) is gated on `pages`, which identity's code moves, and the Playwright rows
 *    beside it only on `browsers`, step by step (a job `if:` cannot read the matrix);
 *  - the callers grant `actions: read`, which `scope` needs to find the newest green Main run (a called workflow
 *    can only narrow its caller's token, and github.com refuses the file outright otherwise);
 *  - the rows that ship nothing and test nothing else are the only ones gated: `test` and `integration-test` run on
 *    every push.
 */
class CiRowScopeSpec extends AnyFlatSpec with Matchers {

  private lazy val ci   = RepoFile.read(".github/workflows/ci.yml")
  private lazy val jobs = RepoFile.jobs(ci)

  private val Gated = Map(
    "mobile-local-server" -> "browsers",
    "e2e"                 -> "e2e",
    "page-tests-chrome"   -> "pages",
    "page-tests-safari"   -> "browsers")

  private def code(block: String) = RepoFile.withoutComments(block).linesIterator.map(_.trim).toList

  "ci.yml's slow rows" should "each wait for scope and run unless it said `false`" in {
    Gated.foreach { case (job, output) =>
      withClue(s"$job: ") {
        code(jobs(job)) should contain("needs: scope")
        code(jobs(job)) should contain(s"if: $${{ !cancelled() && needs.scope.outputs.$output != 'false' }}")
      }
    }
  }

  it should "run the Playwright rows beside the Scala page row only when the public pages can move" in {
    val steps = code(jobs("page-tests-chrome"))
    steps.count(_ == "if: matrix.kind == 'chrome' || needs.scope.outputs.browsers != 'false'") shouldBe 2
  }

  it should "leave the unit and integration rows ungated" in {
    Seq("test", "integration-test").foreach { job =>
      withClue(s"$job: ")(code(jobs(job)).exists(_.contains("needs.scope")) shouldBe false)
    }
  }

  "scope" should "answer in seconds, on the caller's lane, through the classifier" in {
    val scope = code(jobs("scope"))
    scope should contain("timeout-minutes: 5")
    scope should contain("runs-on: ${{ inputs.lane || 'ubuntu-26.04' }}")
    scope should contain("actions: read")
    scope should contain("run: scripts/ci/ci-row-scope.sh --outputs")
    Seq("e2e", "pages", "browsers").foreach(o => scope should contain(s"$o: $${{ steps.scope.outputs.$o }}"))
  }

  "every caller of ci.yml" should "grant the actions: read scope needs" in {
    Seq(".github/workflows/main.yml", ".github/workflows/branch-ci.yml").foreach { caller =>
      withClue(s"$caller: ")(code(RepoFile.jobs(RepoFile.read(caller))("ci")) should contain("actions: read"))
    }
  }
}
