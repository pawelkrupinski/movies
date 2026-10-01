package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Locks the number of jobs a push to main starts AT ONCE to GitHub Free's
 * concurrency allowance.
 *
 * The allowance is 20 concurrent jobs across the whole account, and every job
 * past it queues — not in parallel-but-slower, but genuinely later, starting
 * only when something else finishes. A 21st job therefore does not cost a
 * fraction of a runner; it costs whatever the job it waits behind was still
 * going to take, added to the end of the build. Measured on this repo: runs that
 * overlapped a Country convergence run (3 extra jobs) had page-test rows start
 * up to 3m30s late and finished 12m44s against 8m49s for the same work
 * uncontended.
 *
 * So the budget is a fixed 20, and a new job has to take its slot from an
 * existing one rather than be added. Both files that contribute count: ci.yml's
 * jobs and main.yml's `preflight` start at t=0. (The Fly deploy does not — it
 * `needs: ci`, so ci's jobs have released their slots by then.)
 *
 * A NEEDS-LESS JOB IS THE ONLY KIND THAT COSTS ANYTHING HERE, and that is what
 * made folding `build-web-image.yaml` / `build-worker-image.yaml` into main.yml
 * a budget question rather than a formality. As separate workflows their build
 * jobs also started at t=0 — they just did it in files this spec never read, so
 * a push touching both tiers really started 22 jobs against an allowance of 20
 * and nothing said so. Folded in and hung off `needs: ci`, they take slots ci
 * has already given back, and the number this spec locks becomes true rather
 * than merely unchecked. That is why the last test below exists.
 */
class CiRunnerBudgetSpec extends AnyFlatSpec with Matchers {

  /** GitHub Free: 20 concurrent jobs per account. */
  private val Allowance = 20

  private lazy val ciYml     = RepoFile.read(".github/workflows/ci.yml")
  private lazy val mainYml = RepoFile.read(".github/workflows/main.yml")

  /** Job name → its YAML block, for every job in a workflow file. */
  /**
   * How many runners a job occupies: one per `matrix.include` entry, or one flat
   * if it has no matrix. Counts the `- ` items at the shallowest item indent
   * under `include:`, so a nested `- ` inside an entry (a multi-line list value)
   * isn't miscounted as another entry.
   */
  private def runners(jobBlock: String): Int =
    if (!jobBlock.linesIterator.exists(_.trim == "include:")) 1
    else {
      val body      = RepoFile.block(jobBlock, "include").linesIterator.drop(1).toVector
      val itemLines = body.filter(_.trim.startsWith("- "))
      if (itemLines.isEmpty) 1
      else {
        val itemIndent = itemLines.map(_.takeWhile(_ == ' ').length).min
        itemLines.count(_.takeWhile(_ == ' ').length == itemIndent)
      }
    }

  private lazy val ciRunners = RepoFile.jobs(ciYml).values.map(runners).sum

  // main.yml's own jobs that start immediately — i.e. no `needs:` at all. `ci`
  // is the reusable-workflow call itself and contributes no runner of its own;
  // its jobs are counted above.
  private lazy val deployRunnersAtStart =
    RepoFile.jobs(mainYml).view
      .filterKeys(_ != "ci")
      .collect { case (_, block) if !block.linesIterator.exists(_.trim.startsWith("needs:")) => runners(block) }
      .sum

  "a push to main" should "start no more jobs at once than GitHub Free allows to run at once" in {
    withClue(s"ci.yml=$ciRunners + main.yml(no-needs)=$deployRunnersAtStart: ") {
      ciRunners + deployRunnersAtStart should be <= Allowance
    }
  }

  /**
   * The slots ci.yml does NOT take are deliberate. One is main.yml's `preflight` (below).
   * The other `Headroom` are for the workflows that overlap a push — Country convergence
   * (which Main kicks off itself), dispatched identity runs, the mobile workflows — which
   * hold runners for 20-40 min: with ci filling 19 slots each of their jobs queued one of
   * these rows, and 12 Main runs to 2026-10-01 had ci rows starting a median 1.2-2.0 min
   * late. This was an EXACT-fill assertion while the 20th slot was `free-runners`
   * (retired 2026-09-08, `DeployImageReuseSpec`); it is an upper bound now, so a new row
   * has to take a slot from an existing one or consciously spend the headroom.
   */
  private val Headroom = 4

  it should "leave the preflight's slot and a headroom for overlapping workflows free" in {
    withClue(s"ci.yml=$ciRunners: ")(ciRunners should be <= (Allowance - 1 - Headroom))
  }

  /**
   * ONE main.yml job starts alongside ci: `preflight`, which checks the deploy's
   * secret in seconds so a missing one fails the run at t=0 rather than after
   * ci (`PreflightWiringSpec`). Everything else — the GHCR build jobs that ship
   * the k3s tiers, the Fly release — hangs off `needs: ci`, which is what keeps
   * the budget above honest: dropping a `needs:` to make a deploy land sooner
   * would silently push a push to main past the allowance, and the jobs that
   * queue would be whichever GitHub felt like. `free-runners` was the earlier
   * t=0 exception and is gone (`DeployImageReuseSpec`).
   */
  it should "start nothing alongside ci but the preflight" in {
    val atStart = RepoFile.jobs(mainYml).view
      .filterKeys(_ != "ci")
      .collect { case (name, block) if !block.linesIterator.exists(_.trim.startsWith("needs:")) => name }
      .toSet
    withClue("jobs starting alongside ci: ")(atStart shouldBe Set("preflight"))
  }

  /** ...and the preflight holds that slot for seconds, not for whatever it grows into. */
  it should "bound the preflight to a few minutes at most" in {
    val timeout = RepoFile.jobs(mainYml)("preflight").linesIterator.map(_.trim).collectFirst {
      case s"timeout-minutes: $n" => n.toInt
    }
    timeout.getOrElse(fail("main.yml's preflight has no timeout-minutes")) should be <= 5
  }
}
