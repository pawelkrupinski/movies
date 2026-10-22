package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * How main.yml starts the convergence suites: which workflows it dispatches, and
 * that it does so only once ci is green.
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
   * Convergence is dispatched once Main's `ci` and `gates` are green (2026-10-22, the user's call), not at the push:
   * a red run stops taking runners instead of dispatching a suite for a commit Main has already rejected. It used to
   * hang off `gates` alone, because behind `ci` a cancelled or red Main run never dispatched at all (2026-10-15: 1 h
   * 40 min between the Arsenal client's push and its first convergence run); Main runs are no longer cancelled in
   * flight, and the next green run's dispatch, diffed from each suite's own newest run, covers a red run's commits.
   * No `if:`, so the implicit `success()` skips it after a red `ci` or `gates` (MainAfterRedCiSpec).
   */
  it should "start the convergence suite only once ci and the tier gates are green" in {
    job("kick-convergence") should include("needs: [ci, gates]\n")
    job("kick-convergence") should not include "if:"
  }
}
