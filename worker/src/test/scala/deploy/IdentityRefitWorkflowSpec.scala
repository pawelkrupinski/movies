package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The weekly identity rule refit (`.github/workflows/identity-refit.yml`): the scheduled run keeps one lane, but a
 *  dispatch — a measurement of threads or a relearn — runs beside the others instead of queueing ~30 minutes behind
 *  each (2026-10-15: four thread-count runs, one after another, 15:19 to 17:30). */
class IdentityRefitWorkflowSpec extends AnyFlatSpec with Matchers {
  private lazy val concurrency = RepoFile.block(RepoFile.read(".github/workflows/identity-refit.yml"), "concurrency")

  // keyed by the branch, never the run
  "the refit" should "give each branch's dispatches a lane of their own, and the schedule one shared lane" in {
    concurrency should include("group: identity-refit${{ github.event_name != 'schedule' && format('-{0}', github.ref_name) || '' }}")
    concurrency should include("cancel-in-progress: false")
  }

  private lazy val workflow = RepoFile.read(".github/workflows/identity-refit.yml")

  it should "measure round 1 on as many shards as each shard divides it by, and merge them in the refit" in {
    val shards = """shard: \[([0-9, ]+)\]""".r.findFirstMatchIn(workflow).map(_.group(1).split(",").map(_.trim.toInt).toSeq)
    val of     = """--shard "\$\{\{ matrix\.shard \}\}/([0-9]+)"""".r.findFirstMatchIn(workflow).map(_.group(1).toInt)
    shards shouldBe of.map(n => 0 until n)
    workflow should include("needs: [ measure ]")
    workflow should include("${MERGE:+--merge \"$MERGE\"}")
    // the shards measured the triggering commit, the refit main's tip: merged only when what they measured is unmoved
    workflow should include("git diff --quiet ${{ github.sha }} HEAD -- common identity worker test/resources build.sbt project")
  }
}
