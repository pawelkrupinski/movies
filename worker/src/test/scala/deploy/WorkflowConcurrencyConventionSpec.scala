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
 *  beside it (a recording's or a measurement's `false` keeps hours of paid-for work from being thrown away). The one
 *  fixed choice is Main's: it never cancels (below).
 *
 *  A reusable workflow (`workflow_call`) declares none: the caller owns the lane, and a group named in the callee
 *  is shared by every call of it in one run, which then cancel each other (country-convergence-leg.yml).
 *
 *  A NON-CANCELLING group (`cancel-in-progress` anything but a literal `true`: `false`, or an expression that can come
 *  out false) relies on the CI server to keep at most one running and one pending run, a newer pending run replacing
 *  the older: github.com does, and so does our patched Forgejo (infra docs/forgejo-actions.md, "We run a patched
 *  Forgejo", live since 2026-10-22). So no workflow carries the retired self-supersede wiring any more: no
 *  `supersede` job, no `needs.supersede` gate, no .github/actions/supersede-check.
 *
 *  MAIN NEVER CANCELS (2026-10-22). It used to (`cancel-in-progress: true`, for GitHub Free's runner budget); on
 *  Forgejo with ~10 agents pushing, 39 of 51 Main runs on 2026-10-21 were cancelled (906 runner-minutes thrown
 *  away, 8 runs completed), so builds and deploys rarely landed. Now the run in flight finishes and publishes and
 *  only the newest waiting push runs next: github.com's one running, one pending.
 *
 *  The one name outside the scheme is main.yml's `deploy-${{ github.ref }}`, older than the scheme: renaming it
 *  would buy nothing but a lane change under a queue. */
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

  /** `cancel-in-progress` anything but a literal `true`: an expression can come out false (ios, on a push). */
  private def nonCancelling(yml: String): Boolean =
    groupOf(yml).isDefined &&
      !RepoFile.block(yml, "concurrency").linesIterator.map(_.trim).contains("cancel-in-progress: true")

  "every non-cancelling group" should "leave superseding an older queued run to the server, with no supersede job" in {
    val nonCancellingWorkflows = workflows.filter { case (_, yml) => nonCancelling(yml) }
    nonCancellingWorkflows should not be empty
    val leftovers = workflows.collect {
      case (stem, yml) if RepoFile.jobs(yml).contains("supersede") => s"$stem: `supersede` job"
      case (stem, yml) if RepoFile.withoutComments(yml).contains("needs.supersede") => s"$stem: `needs.supersede` gate"
      case (stem, yml) if RepoFile.withoutComments(yml).contains("supersede-check") => s"$stem: runs supersede-check"
    }
    leftovers shouldBe empty
    RepoFile.exists(".github/actions/supersede-check") shouldBe false
  }

  /** Main's run in flight finishes and publishes; a newer push queues, and the server drops an older queued one. */
  "Main" should "never cancel its run in flight" in {
    val main = workflows.collectFirst { case ("main", yml) => yml }.getOrElse(fail("no main.yml"))
    nonCancelling(main) shouldBe true
  }

  "a reusable workflow" should "declare no group, leaving the lane to its caller" in {
    workflows.collect { case (stem, yml) if yml.contains("workflow_call:") && groupOf(yml).isDefined => stem } shouldBe empty
  }

  "every other workflow" should "have a lane, or be named lane-less here" in {
    val laneLess = workflows.collect { case (stem, yml) if !yml.contains("workflow_call:") && groupOf(yml).isEmpty => stem }
    laneLess.toSet shouldBe LaneLess
  }
}
