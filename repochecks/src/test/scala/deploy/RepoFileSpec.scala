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
      |                  python3 generate_roster_test.py
      |            - run: bash scripts/inline-test.sh
      |""".stripMargin

  "step" should "end before the comment that introduces the next step" in {
    RepoFile.step(workflow, "First") should not include "next-step.sh"
    RepoFile.step(workflow, "First") should include("run: echo first")
  }

  "runSteps" should "read every run: script with its step's working directory" in {
    RepoFile.runSteps(workflow) shouldBe Seq(
      RepoFile.RunStep("echo first", None),
      RepoFile.RunStep("python3 generate_roster_test.py", Some("data/es")),
      RepoFile.RunStep("bash scripts/inline-test.sh", None))
  }

  /** A GitOps manifest missing names where the JVM looked and what the GitOps root held there: the landing check's
   *  itrest row failed LauncherJvmFlagsSpec on missing manifests twice (Identity takes #349's fix, check runs
   *  44984605717 and 44998261508) although its own fetch-gitops had cloned them into the checkout, and "missing"
   *  alone could not say whether the specs looked from another directory or the tree was gone by then. */
  "read" should "name the working directory and the GitOps root's state when a manifest is missing" in {
    val missing = intercept[AssertionError](RepoFile.read("infra/kubernetes/no-such-manifest-for-this-spec.yaml"))
    missing.getMessage should include(s"looked from ${new java.io.File(".").getCanonicalPath}")
    missing.getMessage should include regex """infra/kubernetes (is absent|holds \d+ entr)"""
  }

  it should "not count a trigger path, a comment or a shell comment as run" in {
    val scripts = RepoFile.runSteps(workflow).map(_.script).mkString("\n")
    Seq("only-a-trigger_test.py", "next-step.sh", "a-shell-comment-test.sh").foreach(scripts should not include _)
  }
}
