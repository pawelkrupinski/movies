package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * NOTHING SHIPS AFTER A RED ci: once a Main test row or the tier gates fail, main.yml builds and publishes nothing
 * (2026-10-22). `image-worker` used to hang off `gates` alone and start beside ci, and could run after a red row
 * (run 1351: the worker image built minutes after `test (worker)` failed).
 *
 * Every job but `gates` (t=0), `timings` (`!cancelled()`: it reports red runs too) and `kick-convergence` needs `ci`
 * and `gates`, directly or through a job that does, and carries no status function in its `if:`, so the implicit
 * `success()` skips it. `kick-convergence` is the deliberate exception: it hangs off nothing so convergence
 * starts at the push, red ci or not (KickConvergenceWiringSpec); behind `ci` it started ~50 min later.
 */
class MainAfterRedCiSpec extends AnyFlatSpec with Matchers {
  private lazy val jobs = RepoFile.jobs(RepoFile.read(".github/workflows/main.yml"))

  private val Unconditional = Set("ci", "gates", "timings", "kick-convergence")

  private def needs(block: String): Set[String] =
    block.linesIterator.map(_.trim).collectFirst {
      case l if l.startsWith("needs:") =>
        l.stripPrefix("needs:").trim.stripPrefix("[").stripSuffix("]").split(",").map(_.trim).filter(_.nonEmpty).toSet
    }.getOrElse(Set.empty)

  private def jobIf(name: String): Seq[String] =
    RepoFile
      .withoutComments(jobs(name))
      .linesIterator
      .takeWhile(!_.trim.startsWith("steps:"))
      .map(_.trim)
      .filter(_.startsWith("if:"))
      .toSeq

  "every shipping job in main.yml" should "need both ci and gates" in {
    val shipping = jobs.keySet -- Unconditional
    shipping should contain allOf ("image-worker", "publish-web", "publish-worker")
    for (name <- shipping) withClue(s"$name: ") {
      needs(jobs(name)) should contain allOf ("ci", "gates")
    }
  }

  it should "run only when every job it needs succeeded" in {
    for (name <- jobs.keySet -- Unconditional) withClue(s"$name: ") {
      // The job's own `if:`, above its steps; a step's `if:` does not decide whether the job starts.
      for (cond <- jobIf(name)) {
        cond should not include "always()"
        cond should not include "cancelled()"
        cond should not include "failure()"
      }
    }
  }

  "kick-convergence" should "not be skipped by a red ci" in {
    needs(jobs("kick-convergence")) shouldBe Set.empty
    jobIf("kick-convergence") shouldBe Seq("if: ${{ !cancelled() }}")
  }
}
