package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * A `Verify scrape recording` run says in its title which recording it judged, as `name=value` for every dispatch
 * input.
 *
 * GitHub serves no run's dispatch inputs. agentctl, verifying a landed fix for a red verify run, dispatches the
 * workflow again with the failing run's inputs; for a run agentctl did not start itself (Record scrape fixtures
 * dispatches the nightly one) it reads them from the run's title (agentctl `run_title_inputs`). Without them the
 * dispatch lacked the required `recording` and GitHub refused it (HTTP 422, Verify scrape recording #23, 2026-11-11):
 * no run could ever verify the red.
 */
class VerifyRecordingRunNameSpec extends AnyFlatSpec with Matchers {
  private val workflow = RepoFile.read(".github/workflows/verify-scrape-recording.yml")

  private val dispatchInputs: Seq[String] = {
    val lines   = workflow.linesIterator.toVector
    val start   = lines.indexWhere(_.trim == "inputs:") + 1
    val indent  = lines(start).takeWhile(_ == ' ').length
    lines.drop(start).takeWhile(l => l.trim.isEmpty || l.takeWhile(_ == ' ').length >= indent)
      .filter(l => l.takeWhile(_ == ' ').length == indent && l.trim.endsWith(":"))
      .map(_.trim.stripSuffix(":"))
  }

  "the verify workflow's run title" should "carry every dispatch input as name=value" in {
    dispatchInputs should contain allOf ("recording", "countries")
    val runName = workflow.linesIterator.find(_.startsWith("run-name:")).getOrElse(fail("no run-name"))
    dispatchInputs.foreach { input =>
      withClue(s"$input: ") { runName should include(s"$input=$${{ inputs.$input }}") }
    }
  }
}
