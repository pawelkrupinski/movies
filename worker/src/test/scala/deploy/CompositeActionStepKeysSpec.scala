package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * A composite action's steps take fewer keys than a workflow job's — no `timeout-minutes`,
 * for one — and GitHub only finds out when a job loads the action: the whole action fails
 * with `Unexpected value`, after the job has done its real work, and actionlint (which reads
 * workflows, not actions) never sees it. `timeout-minutes` on the hard-cluster ratchet turned
 * every convergence leg red that way on 2026-09-24.
 */
class CompositeActionStepKeysSpec extends AnyFlatSpec with Matchers {
  // https://docs.github.com/en/actions/sharing-automations/creating-actions/metadata-syntax-for-github-actions#runssteps
  private val Allowed = Set("name", "id", "if", "uses", "run", "shell", "with", "env", "working-directory", "continue-on-error")

  private val StepStart = """^(\s*)- ([A-Za-z][\w-]*):.*""".r
  private val Key       = """^(\s*)([A-Za-z][\w-]*):.*""".r

  /** Every step-level key in `yml`, as (key, line number). A step's keys are the one on its
   *  `- ` line and those at the column just past the dash; deeper lines are their values. */
  private def stepKeys(yml: String): Seq[(String, Int)] = {
    var column = -1
    yml.linesIterator.zipWithIndex.flatMap { case (line, index) =>
      line match {
        case StepStart(indent, key) => column = indent.length + 2; Some(key -> (index + 1))
        case Key(indent, key) if indent.length == column => Some(key -> (index + 1))
        case Key(indent, _) if indent.length < column    => column = -1; None
        case _                                           => None
      }
    }.toSeq
  }

  private lazy val compositeActions: Seq[String] = RepoFile.compositeActions()

  "every composite action" should "give its steps only the keys a composite step accepts" in {
    compositeActions should not be empty
    val offending = for {
      path        <- compositeActions
      (key, line) <- stepKeys(RepoFile.read(path)) if !Allowed(key)
    } yield s"$path:$line $key"
    offending shouldBe empty
  }
}
