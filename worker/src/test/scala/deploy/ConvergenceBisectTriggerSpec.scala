package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Every convergence workflow starts the auto-bisect itself when a leg on main goes red.
 *
 * The bisect used to wait for `on: workflow_run` of the convergence workflows. Main's
 * `kick-convergence` dispatches those with the GITHUB_TOKEN, so their runs belong to
 * github-actions[bot], and GitHub creates no new run for an event the GITHUB_TOKEN caused —
 * `workflow_dispatch` and `repository_dispatch` excepted. Their completions started nothing:
 * the only bisects that ever ran (2026-09-24) followed runs dispatched by hand with a personal
 * token, and the bot-dispatched reds of 16:25–19:51 that day were never bisected.
 *
 * A dispatch is the one event the GITHUB_TOKEN may raise, so the convergence run dispatches
 * the bisect as its last job. Not a reusable-workflow call: that would run the bisect INSIDE
 * the convergence run, holding the suite's one-run lane for up to four hours and queueing
 * the newer red run that is meant to supersede it.
 */
class ConvergenceBisectTriggerSpec extends AnyFlatSpec with Matchers {
  private val Bisect = RepoFile.read(".github/workflows/convergence-bisect.yml")

  /** The workflows that run a HERMETIC convergence leg — the only kind that requests a bisect —
   *  found by what they call, not listed. A recording leg (`mode: record`) has nothing to bisect. */
  private val convergenceWorkflows =
    RepoFile.workflows().map(_.getPath).filter { p =>
      val yml = RepoFile.read(p)
      yml.contains("uses: ./.github/workflows/country-convergence-leg.yml") && """mode:\s+record""".r.findFirstIn(yml).isEmpty
    }

  private def nameOf(yml: String) =
    yml.linesIterator.collectFirst { case s"name: $n" => n.trim }.getOrElse(fail("no name"))

  "the convergence workflows" should "be found" in {
    convergenceWorkflows.map(p => nameOf(RepoFile.read(p))).toSet shouldBe Set("Country convergence", "US convergence")
  }

  they should "each dispatch the bisect when a leg on main failed" in {
    convergenceWorkflows.foreach { path =>
      withClue(s"$path: ") {
        val dispatching = RepoFile.jobs(RepoFile.read(path)).values.filter(_.contains("gh workflow run convergence-bisect.yml"))
        dispatching should have size 1
        val job = dispatching.head
        job should include("needs: [leg]")
        job should include("if: ${{ !cancelled() && needs.leg.result == 'failure' && github.ref == 'refs/heads/main' }}")
        job should include("-f run-id=\"$RUN_ID\"")
        job should include("-f suite=\"$SUITE\"")
        job should include("SUITE: ${{ github.workflow }}")
        job should include("RUN_ID: ${{ github.run_id }}")
        RepoFile.block(job, "permissions").linesIterator.drop(1).map(_.trim).toList shouldBe List("actions: write")
      }
    }
  }

  "the bisect" should "be dispatched, never wait for a completion event the GITHUB_TOKEN never raises" in {
    Bisect should not include "workflow_run"
    val trigger = RepoFile.block(Bisect, "on")
    trigger should include("workflow_dispatch:")
    trigger should include("run-id:")
    trigger should include("suite:")
    convergenceWorkflows.foreach(p => trigger should include(s"- ${nameOf(RepoFile.read(p))}"))
  }

  // Main dispatches with the GITHUB_TOKEN; so does every convergence run. A `workflow_run` on
  // anything started that way waits for an event that never comes.
  "no workflow" should "wait for the completion of a workflow another workflow dispatches" in {
    val byFile = RepoFile.workflows().map(f => f.getName -> RepoFile.read(f.getPath)).toMap
    val all    = byFile.values.mkString("\n")
    val dispatched =
      ("""gh workflow run ([\w.-]+\.ya?ml)""".r.findAllMatchIn(all).map(m => nameOf(byFile(m.group(1)))) ++
        """kick-convergence\.sh "\$GITHUB_SHA" "\$GITHUB_REF_NAME" (.*)""".r.findAllMatchIn(all)
          .flatMap(m => "\"([^\"]+)\"".r.findAllMatchIn(m.group(1)).map(_.group(1)))).toSet
    dispatched should contain allOf ("Country convergence", "US convergence")
    byFile.foreach { case (file, yml) =>
      if (yml.contains("workflow_run:"))
        withClue(s"$file: ")(dispatched.filter(yml.contains) shouldBe empty)
    }
  }
}
