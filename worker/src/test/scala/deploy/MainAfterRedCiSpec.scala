package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * NOTHING AFTER A RED ci: once a Main test row or the tier gates fail, main.yml builds, publishes and dispatches
 * nothing (2026-10-22). `image-worker` and `kick-convergence` used to hang off `gates` alone and start beside ci; on
 * Forgejo's one-runner `main-lane` they then queued behind the remaining ci rows and ran after a red row anyway (run
 * 1351: the worker image built and convergence dispatched minutes after `test (worker)` failed).
 *
 * Every job but `gates` (t=0) and `timings` (`!cancelled()`: it reports red runs too) needs `ci` and `gates`, directly
 * or through a job that does, and carries no status function in its `if:`, so the implicit `success()` skips it.
 */
class MainAfterRedCiSpec extends AnyFlatSpec with Matchers {
  private lazy val jobs = RepoFile.jobs(RepoFile.read(".github/workflows/main.yml"))

  private val Unconditional = Set("ci", "gates", "timings")

  private def needs(block: String): Set[String] =
    block.linesIterator.map(_.trim).collectFirst {
      case l if l.startsWith("needs:") =>
        l.stripPrefix("needs:").trim.stripPrefix("[").stripSuffix("]").split(",").map(_.trim).filter(_.nonEmpty).toSet
    }.getOrElse(Set.empty)

  "every shipping job in main.yml" should "need both ci and gates" in {
    val shipping = jobs.keySet -- Unconditional
    shipping should contain allOf ("image-worker", "publish-web", "publish-worker", "kick-convergence")
    for (name <- shipping) withClue(s"$name: ") {
      needs(jobs(name)) should contain allOf ("ci", "gates")
    }
  }

  it should "run only when every job it needs succeeded" in {
    for (name <- jobs.keySet -- Unconditional) withClue(s"$name: ") {
      // The job's own `if:`, above its steps; a step's `if:` does not decide whether the job starts.
      val header = RepoFile.withoutComments(jobs(name)).linesIterator.takeWhile(!_.trim.startsWith("steps:"))
      for (cond <- header.map(_.trim).filter(_.startsWith("if:"))) {
        cond should not include "always()"
        cond should not include "cancelled()"
        cond should not include "failure()"
      }
    }
  }
}
