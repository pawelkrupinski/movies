package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The weekly identity rule refit (`.github/workflows/identity-refit.yml`): the scheduled run keeps one lane, but a
 *  dispatch — a measurement of threads or a relearn — runs beside the others instead of queueing ~30 minutes behind
 *  each (2026-10-15: four thread-count runs, one after another, 15:19 to 17:30). */
class IdentityRefitWorkflowSpec extends AnyFlatSpec with Matchers {
  private lazy val concurrency = RepoFile.block(RepoFile.read(".github/workflows/identity-refit.yml"), "concurrency")

  "the refit" should "give each dispatch a lane of its own, and the schedule one shared lane" in {
    concurrency should include("group: identity-refit${{ github.event_name != 'schedule' && format('-{0}', github.run_id) || '' }}")
    concurrency should include("cancel-in-progress: false")
  }
}
