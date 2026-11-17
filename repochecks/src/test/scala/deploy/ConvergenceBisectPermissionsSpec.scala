package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * The convergence bisect builds and runs the code of every commit it replays, so the job doing
 * that holds no write token: the commit comment on the first bad commit, the workflow's only
 * write, is a job of its own that checks out and runs nothing of the project.
 */
class ConvergenceBisectPermissionsSpec extends AnyFlatSpec with Matchers {
  private val jobs = RepoFile.jobs(RepoFile.read(".github/workflows/convergence-bisect.yml"))

  private def runsProjectCode(job: String) =
    job.contains("actions/checkout") || job.contains("uses: ./") || job.contains("sbt ")

  /** A job with no `permissions:` of its own takes the workflow's (`contents: read`). */
  private def writes(job: String) =
    job.linesIterator.exists(_.trim == "permissions:") &&
      RepoFile.block(job, "permissions").linesIterator.drop(1).exists(_.trim.matches("""[\w-]+:\s*write"""))

  "every convergence-bisect job that runs project code" should "hold no write permission" in {
    val writing = jobs.collect { case (name, body) if runsProjectCode(body) && writes(body) => name }
    writing shouldBe empty
  }

  "the bisect job" should "not leave the checkout's token in the working tree it runs" in {
    jobs("bisect") should include("persist-credentials: false")
  }

  "the commit comment" should "come from a job that runs nothing of the project" in {
    val commenting = jobs.collect { case (name, body) if body.contains("/comments") => name }
    commenting should not be empty
    commenting.foreach { name =>
      withClue(s"$name: ")(runsProjectCode(jobs(name)) shouldBe false)
    }
  }
}
