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
 *    - `identity-model-convergence`'s run-id form: main's runs in one lane, every other branch's run in a lane of
 *      its own (the user's choice, 2026-10-15; ConvergenceConcurrencyConfigSpec).
 *
 *  `cancel-in-progress` is NOT part of the convention: each workflow chooses it for its own reason and says so
 *  beside it (Main's `true` is deliberate — CLAUDE.md "Batch pushes to main"; a recording's or a measurement's
 *  `false` keeps hours of paid-for work from being thrown away).
 *
 *  A reusable workflow (`workflow_call`) declares none: the caller owns the lane, and a group named in the callee
 *  is shared by every call of it in one run, which then cancel each other (country-convergence-leg.yml).
 *
 *  The one name outside the scheme is main.yml's `deploy-${{ github.ref }}`, older than the scheme: renaming it
 *  would buy nothing and leave the landing push's run in a lane the next push no longer cancels. */
class WorkflowConcurrencyConventionSpec extends AnyFlatSpec with Matchers {
  private val NamedOutsideTheScheme = Map("main" -> "deploy-${{ github.ref }}")
  /** Not reusable, and no lane — unreviewed as of 2026-10-15; giving one a lane is a change of its own. */
  private val LaneLess = Set("discover-cinemas", "country-fixture-artifact", "filmweb-diff", "order-independence",
    "renovate", "roster-audit", "security-headers", "scala-steward")

  private val Key = """(-\$\{\{ github\.ref \}\}|-\$\{\{ inputs\.[a-z-]+ \}\}|\$\{\{ github\.ref != 'refs/heads/main' && format\('-\{0\}', github\.run_id\) \|\| '' \}\})?"""

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

  "a reusable workflow" should "declare no group, leaving the lane to its caller" in {
    workflows.collect { case (stem, yml) if yml.contains("workflow_call:") && groupOf(yml).isDefined => stem } shouldBe empty
  }

  "every other workflow" should "have a lane, or be named lane-less here" in {
    val laneLess = workflows.collect { case (stem, yml) if !yml.contains("workflow_call:") && groupOf(yml).isEmpty => stem }
    laneLess.toSet shouldBe LaneLess
  }
}
