package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * How main.yml starts the convergence suites: which workflows it dispatches, and
 * that it does so the moment ci is green rather than behind anything else.
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
   * Convergence hangs off `ci` alone, so the suite starts the moment the build is
   * green. Nothing gates a deploy on convergence and nothing gates convergence on a
   * machine having restarted (it runs entirely against its own Mongo container and
   * a recorded corpus), so making it wait on anything else buys only latency — it
   * used to queue behind a flyctl deploy for exactly that nothing.
   */
  it should "start the convergence suite as soon as ci is green" in {
    job("kick-convergence") should include("needs: ci\n")
  }
}
