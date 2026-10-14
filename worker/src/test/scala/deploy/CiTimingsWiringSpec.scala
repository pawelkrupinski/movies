package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * `ci-timings.yml` reports a build the moment it finishes only if its `workflow_run` list names
 * that workflow -- by `name:`, not file. Everything else waits for the 30-minute sweep, which reads
 * the same list (scripts/ci/ci_timings.py `workflow_run_names`) to know what it must cover. So a
 * workflow left off the list is charted late, not never -- unless it is one another workflow
 * dispatches with the GITHUB_TOKEN, whose runs raise no `workflow_run` and so must stay OFF it.
 *
 * Left out by construction: the collector itself, and the reusable `workflow_call`-only workflows
 * (`CI`, `Country convergence leg`), whose jobs run, and are charted, inside their callers' runs.
 */
class CiTimingsWiringSpec extends AnyFlatSpec with Matchers {
  private val Collector = ".github/workflows/ci-timings.yml"

  private def triggers(yml: String): Set[String] =
    RepoFile.block(yml, "on").linesIterator
      .collect { case line if line.matches("^ {4}[a-z_]+:.*") => line.trim.takeWhile(_ != ':') }
      .toSet

  private lazy val listed: Set[String] =
    RepoFile.block(RepoFile.read(Collector), "on").linesIterator
      .dropWhile(_.trim != "workflows:").drop(1)
      .takeWhile(_.trim.startsWith("- "))
      .map(_.trim.stripPrefix("- ").trim)
      .toSet

  private lazy val names: Map[String, String] =
    RepoFile.workflows().map(f => f.getPath -> RepoFile.read(f.getPath)).toMap

  "the CI timings collector" should "report every workflow that runs on its own as it finishes, bar those the GITHUB_TOKEN starts" in {
    val dispatched = RepoFile.tokenDispatchedWorkflowNames()
    val builds = names.collect { case (path, yml) if path != Collector && triggers(yml) != Set("workflow_call") => RepoFile.workflowName(yml) }.toSet
    builds should contain("Main") // the scan reaches the workflows
    withClue(s"$Collector's workflow_run list is missing these, so their runs are charted only by the sweep: ") {
      (builds -- dispatched -- listed) shouldBe empty
    }
  }

  it should "list nothing that is not a workflow, which GitHub ignores silently" in {
    listed should not be empty
    (listed -- names.values.map(RepoFile.workflowName)) shouldBe empty
  }

  it should "hand the sweep the list it reads" in {
    RepoFile.read(Collector) should include(s"--workflow-file $Collector")
  }
}
