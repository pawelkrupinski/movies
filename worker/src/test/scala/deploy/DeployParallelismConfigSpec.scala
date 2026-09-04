package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Guards against re-introducing the per-tier deploy concurrency cap.
 *
 * A `concurrency` group keyed on the deploy matrix's `bin` (web / worker) read
 * like it serialized a tier so apps didn't all restart at once. But GitHub keeps
 * only ONE running + ONE pending job per concurrency group, so once a tier had 3
 * apps (PL/UK/DE) the third leg was CANCELLED, not queued — one app silently
 * skipped every deploy. That is exactly what happened after commit 6915b3538
 * added `group: fly-deploy-${{ matrix.bin }}`: `kinowo-worker-de` (and a rotating
 * web leg) dropped from deploys for days, while the pre-group runs deployed all
 * six.
 *
 * The crash-loop that group was meant to prevent (simultaneous restart into an
 * overloaded Mongo) is handled at the root by MongoConnection's degrade-tolerant
 * boot (same commit). Fly deploys one app now (`FlyDeployScopeSpec`), so a group
 * could not drop anything today — but it would be inherited by whoever re-grows
 * the job, which is when it starts dropping apps again. If a tier ever needs
 * staggering, use `strategy.max-parallel` on a split per-tier job — never a
 * shared concurrency group, which drops all but one pending.
 *
 * Tests run with the repo root as CWD, so the workflow path resolves directly.
 */
class DeployParallelismConfigSpec extends AnyFlatSpec with Matchers {
  private lazy val mainYml = RepoFile.read(".github/workflows/main.yml")

  private def job(name: String): String = RepoFile.block(mainYml, name)

  "the deploy workflow" should "not bucket a whole tier into one concurrency group (GitHub cancels the 3rd app)" in {
    job("deploy") should not include "concurrency:"
  }

  it should "dispatch the country convergence suite" in {
    job("kick-convergence") should include("""gh workflow run "Country convergence"""")
  }

  /**
   * And the United States, which is a workflow of its own — one dispatch is not two.
   *
   * It was split out because its leg must not be superseded mid-run (five hours of live
   * enrichment, thrown away and re-fetched cold by whatever cancelled it); the shared
   * suite still yields to the next push. Two lanes need two `gh workflow run`s, and the
   * failure mode of forgetting the second one is silent — the US simply stops being
   * asked whether it converges, exactly as it was before it had a leg at all.
   */
  it should "dispatch the US convergence build alongside it" in {
    job("kick-convergence") should include("""gh workflow run "US convergence"""")
  }

  /**
   * Convergence hangs off `ci`, the same dependency the deploy matrix has — so the
   * suite starts the moment the build is green rather than queueing behind the
   * flyctl deploy. Nothing gates the deploy on convergence and nothing gates
   * convergence on a machine having restarted (it runs entirely against its own
   * Mongo container and a recorded corpus), so making it wait bought only latency.
   */
  it should "start the convergence suite in parallel with the deploy, not after it" in {
    val kick = job("kick-convergence")
    kick should include("needs: ci")
    kick should not include "needs: deploy"
  }

  /**
   * The install used to be gated on `matrix.enabled`, because five of the six
   * legs were disabled and their only work was to skip — which made the install
   * the ONLY step in them that could fail, and on 2026-08-30 it did: the `kinowo`
   * leg's install died in under a second with no output and no annotation (a
   * runner glitch), failing the whole workflow over a tool no leg was going to
   * use. There is one leg now and it always deploys, so the gate has nothing left
   * to hide — but the job still must not install flyctl in a shape where a
   * disabled path could pay for it, which is what having no such path at all
   * guarantees.
   */
  it should "install flyctl on the one leg that actually deploys" in {
    val deploy = job("deploy")
    deploy should include("- uses: superfly/flyctl-actions/setup-flyctl@")
    withClue("the deploy job gates its flyctl install on a condition it no longer has: ") {
      deploy should not include "matrix.enabled"
    }
  }
}
