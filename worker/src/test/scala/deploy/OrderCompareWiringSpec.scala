package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * THE ORDER COMPARISON IS THE CALLERS' (owner, 2026-11-09). It used to be a job of the shared leg (`order-compare`), so every
 * caller created it -- Identity takes' runs too, skipped, its `name:` template rendered raw (`${{ matrix.phase }}`). Now
 * Identity model convergence and Verify scrape recording run `order-independence-p01|p12 (<country>)` as jobs of their own
 * over the preflight's `order-matrix` (a row per country that has order rows), and Identity takes holds the A/A comparison
 * (`takes-drift-aa`) alone. A job with no `name:` renders its id, plain, when skipped.
 */
class OrderCompareWiringSpec extends AnyFlatSpec with Matchers {
  private val convergence = ".github/workflows/identity-model-convergence.yml"
  private val verify      = ".github/workflows/verify-scrape-recording.yml"
  private val takes       = ".github/workflows/identity-takes.yml"
  private val leg         = ".github/workflows/country-convergence-leg.yml"
  private val pairs       = Seq("order-independence-p01", "order-independence-p12")

  private def jobsOf(file: String) = RepoFile.jobs(RepoFile.read(file))

  "Identity model convergence and Verify scrape recording" should "each compare every pair in a job over the countries with order rows" in {
    Seq(convergence, verify).foreach { file =>
      pairs.foreach { pair =>
        val job = jobsOf(file).getOrElse(pair, fail(s"$file has no `$pair` job"))
        withClue(s"$file $pair: ") {
          job should include("needs: [ preflight, leg ]")
          job should include("matrix: ${{ fromJson(needs.preflight.outputs.order-matrix) }}")
          job should include("needs.preflight.outputs.any-order == 'true'")
          job should include("uses: ./.github/actions/order-compare")
          job should include(s"phase:              $pair")
          job should include(s"leg:    $pair (" + "${{ matrix.country }})")
          RepoFile.withoutComments(job).linesIterator.map(_.trim).exists(_.startsWith("name:")) shouldBe false
        }
      }
    }
  }

  they should "name the fixtures release at workflow level, which convergence-setup reads (run 42560258250 failed every comparison without it)" in {
    Seq(convergence, verify).foreach(file => RepoFile.block(RepoFile.read(file), "env") should include("FIXTURE_RELEASE_TAG: convergence-fixtures"))
  }

  they should "have the preflight plan the order matrix off the leg matrix" in {
    Seq(convergence, verify).foreach { file =>
      val outputs = RepoFile.block(jobsOf(file)("preflight"), "outputs")
      outputs should include("order-matrix: ${{ steps.plan.outputs.order-matrix")
      outputs should include("any-order: ${{ steps.plan.outputs.any-order")
    }
  }

  "Identity model convergence" should "skip p12 when its legs ran one order phase, and wait for the order jobs where it waits for the legs" in {
    jobsOf(convergence)("order-independence-p12") should include("needs.preflight.outputs.order-phases != 'one'")
    jobsOf(convergence)("order-independence-p01") should include("needs.preflight.outputs.reused-from == ''")
    Seq("timings", "request-bisect").foreach { job =>
      pairs.foreach(pair => jobsOf(convergence)(job) should include(pair))
    }
  }

  "Identity takes" should "create no order comparison, and hold the A/A comparison under its old name" in {
    val workflow = RepoFile.read(takes)
    jobsOf(takes).keySet should not contain "order-compare"
    RepoFile.withoutComments(workflow) should not include "order-independence"
    RepoFile.withoutComments(workflow) should not include "order-compare"
    val aa = jobsOf(takes)("takes-drift-aa")
    aa should include("needs.preflight.outputs.harness-aa == 'true'")
    aa should include("pattern: identity-drift-${{ matrix.code }}-takes-drift-aa-pass*")
    aa should include("scripts/ci/drift-replays-compare.sh")
    aa should include("python3 scripts/ci/harness_aa.py judge")
    aa should include("""> "$RUNNER_TEMP/leg-verdict-why"""")
    aa should include("leg:    takes-drift-aa (${{ matrix.country }})")
    RepoFile.withoutComments(aa) should not include "name: ${{"
    jobsOf(takes)("timings") should include("takes-drift-aa")
  }

  "the order-compare action" should "read one pair's passes under the check's name" in {
    val action = RepoFile.read(".github/actions/order-compare/action.yml")
    action should include("pattern: order-pass-${{ inputs.code }}-${{ inputs.phase }}-pass*")
    action should include("""KINOWO_CONVERGENCE_ORDER_PASSES: ${{ inputs.phase == 'order-independence-p01' && '0,1' || '1,2' }}""")
    action should include("KINOWO_CONVERGENCE_ORDER_PASS_IN: ${{ runner.temp }}/order-pass")
    action should include("check_name: ${{ inputs.phase }} (${{ inputs.country }})")
    action should include("uses: ./.github/actions/convergence-findings")
  }

  // A job-level `name:` naming a matrix value renders raw when the job is skipped. Only Verify's `verify (<country>)` leg
  // carries one: its preflight fails rather than leave the matrix empty, so it is never skipped.
  "no workflow that calls the leg" should "hold a skippable job whose name renders raw" in {
    val neverSkipped = Set(verify -> "leg")
    Seq(convergence, verify, takes).foreach { file =>
      jobsOf(file).filterNot((id, _) => neverSkipped(file -> id)).foreach { case (id, job) =>
        val jobLevel = job.linesIterator.filter(l => l.startsWith("        ") && !l.startsWith("         ")).map(_.trim).toSeq
        withClue(s"$file $id: ")(jobLevel.filter(_.startsWith("name:")).filter(_.contains("${{")) shouldBe empty)
      }
    }
    // the leg's own row job is named off its matrix and is created only for a row it runs
    jobsOf(leg).keySet shouldBe Set("convergence")
  }
}
