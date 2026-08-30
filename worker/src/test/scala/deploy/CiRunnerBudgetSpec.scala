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
 * jobs and main.yml's `build-image` all start at t=0. (The deploy matrix does
 * not — it `needs: ci`, so ci's jobs have released their slots by then.)
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
   * And it should USE the whole allowance. Under-filling it is as real a
   * regression as over-filling: the build's work is fixed, so a slot left idle
   * is a shard that had to be merged into another row, lengthening the long
   * pole. This is the assertion that makes someone deleting a job think about
   * where the freed runner should go.
   */
  it should "use the whole allowance, not leave a runner idle" in {
    withClue(s"ci.yml=$ciRunners + main.yml(no-needs)=$deployRunnersAtStart: ") {
      ciRunners + deployRunnersAtStart shouldBe Allowance
    }
  }

  /**
   * `build-image` is the one main.yml job that runs alongside ci rather than
   * after it, and it is deliberate: the container build needs the sources, not a
   * green test run, so building it concurrently takes the image build off the
   * post-CI tail. If it ever grew a `needs:`, it would slide back onto the
   * critical path and quietly undo that — while also freeing a slot ci.yml is
   * not expecting.
   */
  it should "build the deploy image alongside the tests, not after them" in {
    val buildImage = RepoFile.block(mainYml, "build-image")
    buildImage should not include "needs:"
    buildImage should include("--build-only")
  }

  /**
   * …and it must stay the ONLY one. The GHCR build jobs that ship the k3s tiers
   * arrived here from two workflows that started them at t=0, which is a slot
   * apiece that neither this budget nor ci.yml's 19 has room for. Hanging them
   * off `needs: ci` is what keeps the number above honest; dropping the `needs:`
   * to make a deploy land four minutes sooner would silently push a push to main
   * to 22 jobs, and the two that queue would be whichever GitHub felt like.
   */
  it should "hang every other main.yml job off ci rather than starting it at t=0" in {
    val atStart = jobs(mainYml).view
      .filterKeys(_ != "ci")
      .collect { case (name, block) if !block.linesIterator.exists(_.trim.startsWith("needs:")) => name }
      .toSet
    withClue("jobs starting alongside ci: ")(atStart shouldBe Set("build-image"))
  }
}
