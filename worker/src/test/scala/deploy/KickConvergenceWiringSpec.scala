package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * How main.yml starts the convergence suites: which workflows it dispatches, and
 * that it does so at the push rather than behind ci or anything else.
 *
 * Tests run with the repo root as CWD, so the workflow path resolves directly.
 */
class KickConvergenceWiringSpec extends AnyFlatSpec with Matchers {
  private lazy val mainYml = RepoFile.read(".github/workflows/main.yml")

  private def job(name: String): String = RepoFile.block(mainYml, name)

  /**
   * Through the pipeline-path gate (ConvergenceDispatchGateSpec), which names each suite: the
   * country suite and its identity-model twin (IdentityModelConvergenceWiringSpec). The United
   * States ran from a workflow of its own, dispatched beside them, until its legs went hermetic;
   * it is a row of the shared suites since 2026-09-30. A dispatch naming the retired build would
   * fail the job on a workflow that is not there.
   */
  "kick-convergence" should "dispatch the convergence builds that run every country, and not the retired US one" in {
    job("kick-convergence") should include("""kick-convergence.sh "$GITHUB_SHA" "$GITHUB_REF_NAME" "Identity model convergence"""")
    job("kick-convergence") should not include "US convergence"
  }

  /**
   * Convergence hangs off `gates` alone (seconds), so the suite starts at the push. Nothing
   * gates a deploy on convergence and nothing gates convergence on a machine having restarted
   * or on Main's tests (it builds the commit itself, against its own Mongo container and a
   * recorded corpus), so making it wait on anything else buys only latency — and behind `ci`
   * a cancelled or red Main run never dispatched at all (2026-10-15: 1 h 40 min between the
   * Arsenal client's push and its first convergence run). `gates`, not nothing: the runner
   * budget's t=0 set stays `gates` alone (CiRunnerBudgetSpec).
   */
  it should "start the convergence suite at the push, behind only the tier gates" in {
    job("kick-convergence") should include("needs: gates\n")
  }
}
