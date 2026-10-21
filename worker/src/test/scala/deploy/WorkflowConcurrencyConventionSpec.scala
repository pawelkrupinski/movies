package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** THE CONCURRENCY-GROUP CONVENTION, for every workflow in `.github/workflows/` — the one place it is written down.
 *
 *  A group is the workflow's FILE STEM, followed by at most one key naming what makes two runs independent:
 *
 *    - nothing (`identity-capture`): runs share state — an issue, a bot branch or PR, the fixture release, a
 *      cache — so every run of the workflow is in ONE lane.
 *    - `-${{ github.ref }}` (`android`, `ios`, `branch-ci`, `record-scrape-fixtures`): one lane per branch,
 *      for runs that build or record what their branch holds.
 *    - `-${{ inputs.<name> }}` (`identity-measure-${{ inputs.variant }}`, `-${{ inputs.recording }}`,
 *      `-${{ inputs.roster }}`, `-${{ inputs.suite }}`): dispatch-only work whose input names its own unit.
 *    - `identity-model-convergence`'s and `identity-run`'s branch form: main's runs in one lane, every other
 *      branch's in a lane of its own — a measurement pushes a measure/ branch per run (the user's choice, 2026-10-15;
 *      ConvergenceConcurrencyConfigSpec).
 *    - `identity-refit`'s branch form: the scheduled runs in one lane, each branch's dispatches (a thread-count
 *      measurement, a relearn) in a lane of their own (the user's choice, 2026-10-15; IdentityRefitWorkflowSpec).
 *    Never `github.run_id`: Forgejo evaluates `concurrency:` before the run has an id, so it is empty there and
 *    every run shared one lane (2026-10-21).
 *
 *  `cancel-in-progress` is NOT part of the convention: each workflow chooses it for its own reason and says so
 *  beside it (Main's `true` is deliberate — CLAUDE.md "Batch pushes to main"; a recording's or a measurement's
 *  `false` keeps hours of paid-for work from being thrown away).
 *
 *  A reusable workflow (`workflow_call`) declares none: the caller owns the lane, and a group named in the callee
 *  is shared by every call of it in one run, which then cancel each other (country-convergence-leg.yml).
 *
 *  A NON-CANCELLING group (`cancel-in-progress` anything but a literal `true`: `false`, or an expression that can come
 *  out false) SUPERSEDES ITS OLDER QUEUED RUNS, as github.com does. github.com holds at most one running and one pending
 *  run per group, a newer pending run replacing the older; Forgejo 15 queues the group FIFO and unbounded, so every
 *  queued run would still do its full work (infra docs/forgejo-actions.md). So such a workflow's first job is
 *  `supersede` (.github/actions/supersede-check, a no-op on github.com): a run that starts with a newer run of its
 *  group already waiting ends green there, and every other job needs it and `if:`s on its output. The rule is on
 *  workflow-level groups only: main.yml's job-level `publish-*` lanes sit inside a run-level group that cancels, and
 *  Forgejo records no job's evaluated group where the check could read it.
 *
 *  The one name outside the scheme is main.yml's `deploy-${{ github.ref }}`, older than the scheme: renaming it
 *  would buy nothing and leave the landing push's run in a lane the next push no longer cancels. */
class WorkflowConcurrencyConventionSpec extends AnyFlatSpec with Matchers {
  private val NamedOutsideTheScheme = Map("main" -> "deploy-${{ github.ref }}")
  /** Not reusable, and no lane — unreviewed as of 2026-10-15; giving one a lane is a change of its own. */
  private val LaneLess = Set("discover-cinemas", "country-fixture-artifact", "filmweb-diff", "order-independence",
    "renovate", "roster-audit", "security-headers", "scala-steward")

  private val Key = """(-\$\{\{ github\.ref \}\}|-\$\{\{ inputs\.[a-z-]+ \}\}|\$\{\{ github\.ref != 'refs/heads/main' && format\('-\{0\}', github\.ref_name\) \|\| '' \}\}|\$\{\{ github\.event_name != 'schedule' && format\('-\{0\}', github\.ref_name\) \|\| '' \}\})?"""

  private lazy val workflows: Seq[(String, String)] =
    RepoFile.workflows().map(f => f.getName.replaceAll("""\.ya?ml$""", "") -> RepoFile.read(f.getPath))

  private def groupOf(yml: String): Option[String] =
    if (!yml.linesIterator.exists(_ == "concurrency:")) None
    else RepoFile.block(yml, "concurrency").linesIterator.map(_.trim).collectFirst { case s"group: $g" => g }

  "every workflow's concurrency group" should "be its file stem plus at most one independence key" in {
    workflows should not be empty
    val strays = for {
      (stem, yml) <- workflows
      group       <- groupOf(yml)
      if !NamedOutsideTheScheme.get(stem).contains(group) && !group.matches(java.util.regex.Pattern.quote(stem) + Key)
    } yield s"$stem: $group"
    strays shouldBe empty
  }

  private val SupersedeGate = "needs.supersede.outputs.superseded != 'true'"

  /** `cancel-in-progress` anything but a literal `true`: an expression can come out false (ios, on a push). */
  private def nonCancelling(yml: String): Boolean =
    groupOf(yml).isDefined &&
      !RepoFile.block(yml, "concurrency").linesIterator.map(_.trim).contains("cancel-in-progress: true")

  /** The job's OWN `needs`/`if` (its body's indentation), never a step's `if:`. */
  private def gatedOnSupersede(job: String): Boolean = {
    val lines   = RepoFile.withoutComments(job).linesIterator.toVector
    val indent  = lines.drop(1).find(_.trim.nonEmpty).map(_.takeWhile(_ == ' ').length).getOrElse(0)
    def own(key: String): String = {
      val start = lines.indexWhere(l => l.takeWhile(_ == ' ').length == indent && l.trim.startsWith(s"$key:"))
      if (start < 0) ""
      else (lines(start) +: lines.drop(start + 1).takeWhile(l => l.trim.isEmpty || l.takeWhile(_ == ' ').length > indent))
        .mkString("\n")
    }
    val needs = own("needs")
    ("""needs:\s*supersede\s*$""".r.findFirstIn(needs).isDefined || needs.contains("[supersede") ||
      needs.linesIterator.exists(_.trim == "- supersede")) && own("if").contains(SupersedeGate)
  }

  "every non-cancelling group" should "supersede an older queued run as github.com does, gating every job on it" in {
    val nonCancellingWorkflows = workflows.filter { case (_, yml) => nonCancelling(yml) }
    nonCancellingWorkflows should not be empty
    val gaps = nonCancellingWorkflows.flatMap { case (stem, yml) =>
      val jobs = RepoFile.jobs(yml)
      jobs.get("supersede") match {
        case None => Seq(s"$stem: no `supersede` job")
        case Some(job) if !job.contains("uses: ./.github/actions/supersede-check") =>
          Seq(s"$stem: `supersede` does not run .github/actions/supersede-check")
        case _ => jobs.collect { case (name, job) if name != "supersede" && !gatedOnSupersede(job) =>
            s"$stem/$name: needs `supersede` and its `if:` must include `$SupersedeGate`" }
      }
    }
    gaps shouldBe empty
  }

  "the supersede check" should "do nothing on github.com, which supersedes by itself" in {
    val action = RepoFile.read(".github/actions/supersede-check/action.yml")
    action should include("if: github.server_url != 'https://github.com'")
    action should include("/superseded-by")
  }

  "a reusable workflow" should "declare no group, leaving the lane to its caller" in {
    workflows.collect { case (stem, yml) if yml.contains("workflow_call:") && groupOf(yml).isDefined => stem } shouldBe empty
  }

  "every other workflow" should "have a lane, or be named lane-less here" in {
    val laneLess = workflows.collect { case (stem, yml) if !yml.contains("workflow_call:") && groupOf(yml).isEmpty => stem }
    laneLess.toSet shouldBe LaneLess
  }
}
