package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Guards against re-introducing the per-tier deploy concurrency cap.
 *
 * A `concurrency` group keyed on the deploy matrix's `bin` (web / worker) reads
 * like it serializes a tier so apps don't all restart at once. But GitHub keeps
 * only ONE running + ONE pending job per concurrency group, so once a tier has 3
 * apps (PL/UK/DE) the third leg is CANCELLED, not queued — one app silently skips
 * every deploy. That is exactly what happened after commit 6915b3538 added
 * `group: fly-deploy-${{ matrix.bin }}`: `kinowo-worker-de` (and a rotating web
 * leg) dropped from deploys for days, while the pre-group runs deployed all six.
 *
 * The crash-loop that group was meant to prevent (simultaneous restart into an
 * overloaded Mongo) is handled at the root by MongoConnection's degrade-tolerant
 * boot (same commit). So deploys run fully parallel. If a tier ever needs
 * staggering, use `strategy.max-parallel` on a split per-tier job — never a
 * shared concurrency group, which drops all but one pending.
 *
 * Tests run with the repo root as CWD, so the workflow path resolves directly.
 */
class DeployParallelismConfigSpec extends AnyFlatSpec with Matchers {
  private lazy val mainYml = RepoFile.read(".github/workflows/main.yml")

  private def job(name: String): String = RepoFile.block(mainYml, name)

  "the deploy workflow" should "not bucket a whole tier into one concurrency group (GitHub cancels the 3rd app)" in {
    mainYml should not include "fly-deploy-${{ matrix.bin }}"
  }

  it should "dispatch the country convergence suite" in {
    job("kick-convergence") should include("""gh workflow run "Country convergence"""")
  }

  /**
   * Convergence hangs off `ci`, the same dependency the deploy matrix has — so the
   * suite starts the moment the build is green rather than queueing behind six
   * flyctl legs. Nothing gates the deploy on convergence and nothing gates
   * convergence on a machine having restarted (it runs entirely against its own
   * Mongo container and a recorded corpus), so making it wait bought only latency.
   */
  it should "start the convergence suite in parallel with the deploy matrix, not after it" in {
    val kick = job("kick-convergence")
    kick should include("needs: ci")
    kick should not include "needs: deploy"
  }

  /**
   * Five of the six legs are `enabled: false` since the web tier and the workers
   * moved to k3s (`FlyDeployScopeSpec` holds the roster), so the guard's
   * disabled-branch short-circuits before either flyctl probe — a disabled leg
   * never runs a flyctl command at all. Installing flyctl regardless put a tool
   * download on those five legs per push whose only work is to skip, and it made
   * that install the ONLY step in them that can fail: on 2026-08-30 the `kinowo`
   * leg's install died in under a second with no output and no annotation (a
   * runner glitch — the five sibling legs installed the same action fine),
   * failing the whole workflow over a tool no leg was going to use. Gate the
   * install on the same flag the guard reads.
   */
  it should "install flyctl only on a leg that actually deploys" in {
    val lines = job("deploy").linesIterator.toVector
    val at    = lines.indexWhere(_.trim.startsWith("- uses: superfly/flyctl-actions/setup-flyctl@"))
    at should be >= 0

    val neighbours = Seq(lines(at - 1), lines(at + 1)).map(_.trim)
    withClue(s"setup-flyctl in the deploy job is ungated; neighbours were $neighbours: ") {
      neighbours.exists(l => l.startsWith("if:") && l.contains("matrix.enabled")) shouldBe true
    }
  }
}
