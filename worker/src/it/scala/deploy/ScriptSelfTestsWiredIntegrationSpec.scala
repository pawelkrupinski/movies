package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * The script self-tests the workflow specs rely on, each run by CI's script-test runner.
 *
 * Each row was one line of a unit wiring spec (`ScriptTests.runInCi(...)`): a workflow spec reads a script's wiring as
 * text and leans on the script's own self-test for its behaviour. Whether the runner runs that self-test is the
 * runner's own discovery (`bash scripts/run-script-tests --list`, a real process), so the rows live here, in `it`,
 * as one table (the owner, 2026-10-27: unit specs spawn no process).
 */
class ScriptSelfTestsWiredIntegrationSpec extends AnyFlatSpec with Matchers {

  /** Each self-test, and the unit wiring spec that leans on it. */
  private val Wired: Seq[(String, String)] = Seq(
    "scripts/ci/prune-actions-caches-test.sh"                -> "ActionsCachePruneWiringSpec: the prune script's own spec",
    "scripts/ci/bot-branch-ready-test.sh"                    -> "BotBranchOnForgejoSpec: the Forgejo publish names the branch to its owner",
    ".github/scripts/convergence-fill-test.sh"               -> "ConvergenceFillWiringSpec: the fill's release script",
    ".github/scripts/convergence-findings-summary-test.sh"   -> "ConvergenceLegWiringSpec: the findings summary never cut by an early-closing head",
    "scripts/ci/stop-stray-sbt-test.sh"                      -> "ConvergenceLegWiringSpec, IdentitySliceOldRefGateSpec: a timed-out step's JVMs ended",
    ".github/scripts/convergence-bisect-step-test.sh"        -> "HermeticConvergenceWiringSpec: each bisect step replays the checked-in step script",
    "scripts/ci/identity_findings_report_test.py"            -> "IdentityFindingsReportWorkflowSpec: the report reads the findings, and fails with no recording",
    "scripts/ci/sbt-sources-key-test.sh"                     -> "SbtCachePathsSpec: the cache key script's own spec",
    "scripts/adopt_ci_budgets_test.py"                       -> "PerformanceBudgetArtifactsSpec: adopt-ci-budgets",
    ".github/scripts/convergence-publish-test.sh"            -> "RecordCorpusInLegWiringSpec: the publish keeps each country's newest five pinned trees",
    "scripts/ci/upstream_run_test.py"                        -> "UpstreamRunSpec: the upstream-run resolver and its fallback")

  "every self-test a workflow spec leans on" should "be run by CI's script-test runner" in {
    val notRun = Wired.collect { case (path, why) if !ScriptTests.runInCi(path) => s"$path ($why)" }
    withClue("CI's runner (scripts/run-script-tests, run in full by ci.yml) does not run:\n" + notRun.mkString("\n") + "\n") {
      notRun shouldBe empty
    }
  }
}
