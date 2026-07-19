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
  private lazy val deployYml = RepoFile.read(".github/workflows/deploy.yml")

  "the deploy workflow" should "not bucket a whole tier into one concurrency group (GitHub cancels the 3rd app)" in {
    deployYml should not include "fly-deploy-${{ matrix.bin }}"
  }
}
