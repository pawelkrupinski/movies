package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The YAML readers every workflow spec asserts through: a reader that hands a spec text from the
 *  wrong place makes that spec pass on wiring that is not there. */
class RepoFileSpec extends AnyFlatSpec with Matchers {

  private val workflow =
    """jobs:
      |    test:
      |        paths:
      |            - 'scripts/only-a-trigger_test.py'
      |        steps:
      |            - name: First
      |              run: echo first
      |            # The step below runs scripts/next-step.sh.
      |            - name: Second
      |              working-directory: data/es
      |              run: |
      |                  # scripts/a-shell-comment-test.sh
      |                  python3 test_generate_roster.py
      |            - run: bash scripts/inline-test.sh
      |""".stripMargin

  "step" should "end before the comment that introduces the next step" in {
    RepoFile.step(workflow, "First") should not include "next-step.sh"
    RepoFile.step(workflow, "First") should include("run: echo first")
  }

  "runSteps" should "read every run: script with its step's working directory" in {
    RepoFile.runSteps(workflow) shouldBe Seq(
      RepoFile.RunStep("echo first", None),
      RepoFile.RunStep("python3 test_generate_roster.py", Some("data/es")),
      RepoFile.RunStep("bash scripts/inline-test.sh", None))
  }

  it should "not count a trigger path, a comment or a shell comment as run" in {
    val scripts = RepoFile.runSteps(workflow).map(_.script).mkString("\n")
    Seq("only-a-trigger_test.py", "next-step.sh", "a-shell-comment-test.sh").foreach(scripts should not include _)
  }
}
