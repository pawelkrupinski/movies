package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Every recorder leg posts its own verdict the moment it ends.
 *
 * "Record scrape fixtures" reported when its SLOWEST leg did — the United States' full
 * convergence, 48 minutes on 2026-09-24 — so a leg that failed at minute 10 said nothing for
 * the rest of the hour. Cancelling the other legs would have thrown away their recordings
 * (each publishes its own fixture tree, pass or fail), so instead each leg runs
 * `.github/actions/leg-verdict` as its LAST step, `if: always()`: a commit status named after
 * the leg, a job-summary line, and an error annotation. The `report` job still aggregates.
 *
 * A reusable workflow's job can only use the token scopes its CALLER grants, so every caller
 * of the leg workflow must grant `statuses: write` too, or the run fails to start at all.
 */
class LegVerdictWiringSpec extends AnyFlatSpec with Matchers {
  private val Action  = "uses: ./.github/actions/leg-verdict"
  private val LegFile = ".github/workflows/country-convergence-leg.yml"

  private lazy val recorder = RepoFile.jobs(RepoFile.read(".github/workflows/record-scrape-fixtures.yml"))
  private lazy val leg      = RepoFile.jobs(RepoFile.read(LegFile))

  /** The jobs that run steps and must each report themselves: the recorder's scrape legs,
   *  and the leg workflow's sample and full jobs. */
  private lazy val legJobs: Seq[(String, String)] =
    Seq("record" -> recorder("record"), "sample" -> leg("sample"), "convergence" -> leg("convergence"))

  "every recorder leg" should "post its verdict as its LAST step, whatever happened before it" in {
    val problems = legJobs.flatMap { case (name, body) =>
      val steps = body.split("\n\\s+- (?=uses:|name:)").toSeq.drop(1)
      steps.lastOption match {
        case Some(last) if last.startsWith(Action) =>
          Option.when(!last.contains("if: always()"))(s"$name: the verdict step is skipped when the leg fails")
            .toSeq ++
            Option.when(!last.contains("status: ${{ job.status }}"))(s"$name: the verdict does not report job.status").toSeq
        case _ => Seq(s"$name: its last step is not $Action")
      }
    }
    problems shouldBe empty
  }

  it should "hold the statuses: write the verdict needs" in {
    legJobs.collect { case (name, body) if !body.contains("statuses: write") => name } shouldBe empty
  }

  "every caller of the leg workflow" should "grant statuses: write, or the run fails to start" in {
    val callers = for {
      file        <- RepoFile.workflows()
      (job, body) <- RepoFile.jobs(RepoFile.read(file.getPath)) if body.contains(s"uses: ./$LegFile")
    } yield s"${file.getName}:$job" -> body
    callers.map(_._1) should contain("record-scrape-fixtures.yml:enrichment")
    callers.collect { case (name, body) if !body.contains("statuses: write") => name } shouldBe empty
  }
}
