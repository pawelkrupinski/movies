package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import ForgejoParse.*

/**
 * ONE RESOLVER FOR UPSTREAM RUNS. A workflow started by another workflow's run (`on: workflow_run`) reads that run's
 * output through one step, `.github/actions/upstream-run` (scripts/ci/upstream_run.py), in its plan job: the run named
 * by hand, else the trigger when it succeeded on main, else the newest successful run on main, said as an annotation.
 * Each consumer used to choose for itself, and each choice went wrong once: the live drift check skipped its only job
 * after a failed recording and showed green (run index 1493); the findings report took a failed recording whole and went
 * red for lack of input (run 3139, after recording 3136). Fitted to WorkflowPlanSpec's plan job: the resolver runs in a
 * root job, and every other job reads `needs.<plan>.outputs.run`.
 */
class UpstreamRunSpec extends AnyFlatSpec with Matchers {

  private val Action = "./.github/actions/upstream-run"

  /** Triggered by another workflow's run, and reading no run of it: what they do instead. */
  private val ReadNoRun = Map(
    "prune-actions-caches.yml" -> "prunes superseded caches after any of its workflows; it reads nothing they produced"
  )

  private def code(file: String) = RepoFile.withoutComments(RepoFile.read(s".github/workflows/$file"))
  private lazy val triggered = RepoFile.workflows().map(_.getName).sorted.filter { file =>
    RepoFile.block(code(file), "on").linesIterator.exists(_.trim == "workflow_run:")
  }
  private def producers(file: String): Seq[String] = {
    val on = RepoFile.block(code(file), "on")
    """workflows:\s*\[([^\]]*)]""".r.findFirstMatchIn(on).map(_.group(1).split(",").map(_.trim).toSeq)
      .getOrElse(RepoFile.block(on, "workflows").linesIterator.drop(1).map(_.trim.stripPrefix("- ").trim).toSeq)
  }
  private def resolverSteps(file: String) = for {
    (name, job) <- RepoFile.jobs(code(file)).toSeq.sortBy(_._1)
    step        <- ForgejoParse.steps(job)
    if step.get("uses").exists(_.trim == Action)
  } yield (name, job, step)

  "a workflow triggered by another's run" should "read it through the one resolver, in a root plan job" in {
    val problems = triggered.filterNot(ReadNoRun.contains).flatMap { file =>
      resolverSteps(file) match {
        case Seq((plan, job, step)) =>
          val id     = step.getOrElse("id", "").trim
          val named  = """workflow:\s*(.+)""".r.findFirstMatchIn(step.getOrElse("with", "")).map(_.group(1).trim)
          val others = RepoFile.jobs(code(file)).toSeq.filter(_._1 != plan)
          Seq(
            Option.when(needs(job).nonEmpty)(s"$file: $plan, which resolves the run, needs others: it must be a root"),
            Option.when(!job.contains(s"run: $${{ steps.$id.outputs.run }}"))(
              s"$file: $plan does not output `run: $${{ steps.$id.outputs.run }}`"),
            Option.when(!named.exists(producers(file).contains))(
              s"$file: the resolver's `workflow:` ${named.getOrElse("(none)")} is not one of ${producers(file).mkString(", ")}"),
            Option.when(!others.exists((_, j) => j.contains(s"needs.$plan.outputs.run")))(
              s"$file: no job reads needs.$plan.outputs.run")
          ).flatten
        case none if none.isEmpty => Seq(s"$file: reads its producing run without $Action (or list it in ReadNoRun)")
        case many                 => Seq(s"$file: ${many.size} resolver steps; one plan job resolves the run")
      }
    }
    withClue("each workflow_run consumer's input, one rule (scripts/ci/upstream_run.py): ")(problems shouldBe empty)
  }

  it should "take the triggering run's fields from nowhere but the resolver" in {
    val readers = (RepoFile.workflows().map(_.getPath) ++ RepoFile.compositeActions())
      .filterNot(_ == ".github/actions/upstream-run/action.yml")
      .filter(path => RepoFile.withoutComments(RepoFile.read(path)).contains("github.event.workflow_run"))
    withClue("a workflow_run's id, conclusion or branch read elsewhere is a second rule: ")(readers shouldBe empty)
  }

  it should "list a workflow as reading no run only while it is still triggered by one" in {
    ReadNoRun.keys.filterNot(triggered.contains) shouldBe empty
    ReadNoRun.keys.filter(f => resolverSteps(f).nonEmpty) shouldBe empty
  }

  "the resolver" should "be scripts/ci/upstream_run.py, with the fallback the workflows rely on" in {
    val action = RepoFile.read(".github/actions/upstream-run/action.yml")
    action should include("python3 scripts/ci/upstream_run.py")
    action should include("github.event.workflow_run.conclusion")
    withClue("the resolver lists runs through scripts/forge-gh, which every plan job's checkout must hold: ") {
      triggered.filterNot(ReadNoRun.contains).flatMap(resolverSteps(_).map(_._2))
        .filterNot(WorkflowScriptsRunnableSpec.checkoutHolds(_, "scripts/forge-gh")) shouldBe empty
    }
  }
}
