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
 * jobs and main.yml's `free-runners` start at t=0. (The Fly deploy does not — it
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
  private def jobs(yml: String): Map[String, String] = {
    val jobsBlock = RepoFile.block(yml, "jobs")
    val Header    = """^(\s+)([A-Za-z][\w-]*):\s*$""".r
    val topIndent = jobsBlock.linesIterator
      .drop(1)
      .collectFirst { case Header(indent, _) => indent.length }
      .getOrElse(fail("`jobs:` has no job under it"))
    jobsBlock.linesIterator
      .collect { case line @ Header(indent, name) if indent.length == topIndent => name }
      .map(name => name -> RepoFile.block(jobsBlock, name))
      .toMap
  }

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

  private lazy val ciRunners = jobs(ciYml).values.map(runners).sum

  // main.yml's own jobs that start immediately — i.e. no `needs:` at all. `ci`
  // is the reusable-workflow call itself and contributes no runner of its own;
  // its jobs are counted above.
  private lazy val deployRunnersAtStart =
    jobs(mainYml).view
      .filterKeys(_ != "ci")
      .collect { case (_, block) if !block.linesIterator.exists(_.trim.startsWith("needs:")) => runners(block) }
      .sum

  "a push to main" should "start no more jobs at once than GitHub Free allows to run at once" in {
    withClue(s"ci.yml=$ciRunners + main.yml(no-needs)=$deployRunnersAtStart: ") {
      ciRunners + deployRunnersAtStart should be <= Allowance
    }
  }

  /**
   * Used to be an EXACT-fill assertion — under-filling the allowance was as real
   * a regression as over-filling, because the account's 20th slot was always
   * `free-runners`, a job with nothing else it could be spending it on. That job
   * was retired 2026-09-08 (`DeployImageReuseSpec`), and this budget now runs
   * one slot under the allowance deliberately: filling it means giving ci.yml's
   * own sharding another row, which is a call about THAT suite's shard sizes,
   * not a consequence of retiring a runner-preemption step. Left as headroom
   * until someone has a shard that wants it.
   */
  it should "leave the freed slot as headroom rather than force an unrelated shard to fill it" in {
    withClue(s"ci.yml=$ciRunners + main.yml(no-needs)=$deployRunnersAtStart: ") {
      ciRunners + deployRunnersAtStart shouldBe (Allowance - 1)
    }
  }

  /**
   * NO main.yml job should start alongside ci any more. `free-runners` was the
   * one exception — it ran with no `needs:` because the runners it freed were
   * only useful while ci's jobs were still queueing — and it is gone
   * (`DeployImageReuseSpec`). The GHCR build jobs that ship the k3s tiers all
   * hang off `needs: ci`, which is what keeps the budget above honest: dropping
   * a `needs:` to make a deploy land sooner would silently push a push to main
   * past the allowance, and the jobs that queue would be whichever GitHub felt
   * like.
   */
  it should "hang every main.yml job off ci rather than starting any at t=0" in {
    val atStart = jobs(mainYml).view
      .filterKeys(_ != "ci")
      .collect { case (name, block) if !block.linesIterator.exists(_.trim.startsWith("needs:")) => name }
      .toSet
    withClue("jobs starting alongside ci: ")(atStart shouldBe empty)
  }
}
