package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * The identity resolver's whole-corpus measurement on GitHub's runners
 * (`.github/workflows/identity-measure.yml`, dispatched by `scripts/identity-measure-ci.sh`).
 * Each rule is one way it would quietly stop measuring what the local harness measures.
 */
class IdentityMeasureWorkflowSpec extends AnyFlatSpec with Matchers {
  private lazy val workflow = RepoFile.read(".github/workflows/identity-measure.yml")
  private lazy val helper   = RepoFile.read("scripts/identity-measure-ci.sh")
  private lazy val measure  = RepoFile.jobs(workflow)("measure")

  private def triggers: Set[String] =
    RepoFile.block(workflow, "on").linesIterator.drop(1).map(_.trim)
      .collect { case s"$key:" if !key.contains(" ") => key }.filter(t => Set("push", "pull_request", "schedule", "workflow_dispatch", "workflow_call")(t)).toSet

  // One runner per country out of the 20 Main's run needs: only a person asking should spend them.
  "the identity measurement" should "run only when dispatched" in {
    triggers shouldBe Set("workflow_dispatch")
  }

  it should "replay the pinned recorded pair the convergence verdicts use, never record one" in {
    measure should include("uses: ./.github/actions/convergence-setup")
    """mode:\s+record""".r.findFirstIn(workflow) shouldBe empty
  }

  it should "resolve one country per job, as the per-country TitleRuleSet requires" in {
    measure should include("KINOWO_IDENTITY_FULL:           ${{ matrix.code }}")
  }

  it should "skip the robustness re-resolves unless asked" in {
    """robustness:[\s\S]*?default:\s*'off'""".r.findFirstIn(workflow) shouldBe defined
  }

  // A resolver-only variant must reuse the boot; a pipeline change must not.
  it should "key the booted pipeline on the corpus and on every non-resolver source" in {
    val key = RepoFile.step(measure, "Restore the booted pipeline")
    key should include("env.KINOWO_CONVERGENCE_CORPUS_RUN")
    key should include("'!**/services/identity/**'")
    key should include("'**/src/main/**/*.scala'")
  }

  // Only main reaches origin: a variant travels as a patch on a base commit.
  "the helper" should "dispatch the workflow with the worktree's diff as a patch, pushing nothing" in {
    helper should include("gh workflow run \"$workflow\" --ref main")
    helper should include("-f patch=\"$patch\"")
    helper should not include "git push"
  }

  // Five countries download into one directory: a shared name keeps only the last country's log.
  it should "keep each country's log under its own name" in {
    measure should include("run-${{ matrix.code }}.log")
  }

  // It takes one runner per country from the 20 Main's run needs.
  it should "wait for Main to be idle before dispatching" in {
    helper should include("--workflow main.yml")
    measure should include("git apply --binary --index")
  }
}
