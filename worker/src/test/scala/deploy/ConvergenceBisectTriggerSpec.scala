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

  /** The workflows that run a HERMETIC pipeline convergence leg — the only kind that requests a
   *  bisect — found by what they call, not listed. A recording leg (`mode: record`) has nothing to
   *  bisect. Nor does `Verify scrape recording`: it judges main's head, which Identity model convergence has already
   *  judged commit by commit, against one night's recording -- a red there is code meeting new data, answered by a
   *  fix and a re-dispatch on the same recording, not by a range of commits. Nor Identity takes (below). */
  private val convergenceWorkflows =
    RepoFile.workflows().map(_.getPath).filter { p =>
      val yml = RepoFile.read(p)
      yml.contains("uses: ./.github/workflows/country-convergence-leg.yml") && """mode:\s+record""".r.findFirstIn(yml).isEmpty &&
      !p.endsWith("verify-scrape-recording.yml") && !p.endsWith("identity-takes.yml")
    }

  private def nameOf(yml: String) =
    yml.linesIterator.collectFirst { case s"name: $n" => n.trim }.getOrElse(fail("no name"))

  /** Identity takes (2026-11-08) runs only the takes rows, and a red takes row names its own range (base..head): no row of
   *  it leaves a bisect request (country-convergence-leg.yml: `!startsWith(matrix.phase, 'takes')`), so it dispatches none. */
  "Identity takes" should "dispatch no bisect: its rows name their own range" in {
    val takes = RepoFile.read(".github/workflows/identity-takes.yml")
    takes should not include "gh workflow run convergence-bisect.yml"
    RepoFile.read(".github/workflows/country-convergence-leg.yml") should include("!startsWith(matrix.phase, 'takes')")
  }

  "the convergence workflows" should "be found" in {
    convergenceWorkflows.map(p => nameOf(RepoFile.read(p))).toSet shouldBe Set("Identity model convergence")
  }

  they should "each dispatch the bisect when a leg on main failed" in {
    convergenceWorkflows.foreach { path =>
      withClue(s"$path: ") {
        val dispatching = RepoFile.jobs(RepoFile.read(path)).values.filter(_.contains("gh workflow run convergence-bisect.yml"))
        dispatching should have size 1
        val job = dispatching.head
        job should include("needs: [ preflight, leg ]")
        // "On main" is the plan's decision (WorkflowPlanSpec), read here as its output.
        job should include("if: ${{ !cancelled() && needs.leg.result == 'failure' && needs.preflight.outputs.on-main == 'true' }}")
        RepoFile.block(RepoFile.jobs(RepoFile.read(path))("preflight"), "outputs") should include(
          "on-main: ${{ github.ref == 'refs/heads/main' }}")
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
  // anything started that way waits for an event that never comes. A dispatch is code (a comment's
  // "by hand: gh workflow run ..." is a person's token); a wait is a name in `on.workflow_run.workflows`.
  "no workflow" should "wait for the completion of a workflow another workflow dispatches" in {
    val byFile = RepoFile.workflows().map(f => f.getName -> RepoFile.withoutComments(RepoFile.read(f.getPath))).toMap
    val all    = byFile.values.mkString("\n")
    val dispatched =
      ("""gh workflow run ([\w.-]+\.ya?ml)""".r.findAllMatchIn(all).map(m => nameOf(byFile(m.group(1)))) ++
        """kick-convergence\.sh "\$GITHUB_SHA" "\$GITHUB_REF_NAME" (.*)""".r.findAllMatchIn(all)
          .flatMap(m => "\"([^\"]+)\"".r.findAllMatchIn(m.group(1)).map(_.group(1)))).toSet
    dispatched should contain ("Identity model convergence")
    def awaited(yml: String): Set[String] = {
      val on = RepoFile.block(RepoFile.block(yml, "on"), "workflow_run")
      """workflows:\s*\[([^\]]*)]""".r.findFirstMatchIn(on).map(_.group(1).split(",").map(_.trim).toSet)
        .getOrElse(on.linesIterator.dropWhile(!_.trim.startsWith("workflows:")).drop(1).map(_.trim)
          .takeWhile(_.startsWith("- ")).map(_.stripPrefix("- ").trim).toSet)
    }
    val waiting = byFile.filter { case (_, yml) => RepoFile.block(yml, "on").linesIterator.exists(_.trim == "workflow_run:") }
    waiting.keySet should contain ("report-timings.yml")
    waiting.foreach { case (file, yml) =>
      withClue(s"$file: ")(awaited(yml).intersect(dispatched) shouldBe empty)
    }
  }
}
