package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The closed-venue retirement (`.github/workflows/retire-venues.yml`): its later steps run only
 *  when the script reports a non-zero count, so that count must never survive the script failing. */
class RetireVenuesWorkflowSpec extends AnyFlatSpec with Matchers {
  private lazy val workflow = RepoFile.read(".github/workflows/retire-venues.yml")
  private lazy val retire   = RepoFile.stepScript(workflow, "Re-check each venue live and retire the confirmed ones")

  "the retirement step" should "fail when the script does, not pipe its crash into a count" in {
    // `bash -e` without pipefail grades a pipeline by its LAST command: `script | tail -1` turned a
    // crash half-way through the roster rewrite into an empty count, which is not '0', and the
    // half-applied rewrite went on through CountrySpec to a PR.
    retire should include("retire_venues.py")
    retire should not include "| tail"
  }
}
