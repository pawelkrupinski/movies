package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * `identity-model-convergence.yml`: the country convergence suite — every country's legs, whose films
 * the identity model decides, as production's are — dispatched by Main on the pipeline-path gate.
 */
class IdentityModelConvergenceWiringSpec extends AnyFlatSpec with Matchers {
  private lazy val workflow = RepoFile.read(".github/workflows/identity-model-convergence.yml")
  private lazy val leg      = RepoFile.read(".github/workflows/country-convergence-leg.yml")
  private lazy val main     = RepoFile.read(".github/workflows/main.yml")

  "the convergence suite" should "be dispatched by Main's convergence kick, on the pipeline-path gate" in {
    val triggers = RepoFile.block(workflow, "on")
    triggers should include("workflow_dispatch:")
    Seq("schedule:", "push:", "pull_request:", "workflow_run:", "workflow_call:").foreach(triggers should not include _)
    RepoFile.jobs(main)("kick-convergence") should include(
      """kick-convergence.sh "$GITHUB_SHA" "$GITHUB_REF_NAME" "Identity model convergence"""")
    // An edit to this workflow can change its verdict, so it is one of the paths the gate dispatches for.
    RepoFile.read(".github/convergence-paths.txt").linesIterator.map(_.trim).toSeq should contain(
      ".github/workflows/identity-model-convergence.yml")
  }

  private val OrderTag    = "services.movies.OrderIndependence"
  private def byCountry(yaml: String): Map[String, Map[String, String]] =
    RepoFile.matrixRows(yaml).map(row => row("country") -> row).toMap
  private def specOf(alias: String): String = RepoFile.commandAlias(alias).split(" -- ").head

  it should "run every country" in {
    byCountry(workflow).keySet shouldBe Set("poland", "germany", "united-kingdom", "spain", "united-states")
  }

  /** Germany's three lockstep replays were ~5 of the 11.3 minutes of the lane's slowest row (run
   *  37148209974). In a row of their own they run beside the rest of the spec, as the US's do. */
  it should "replay Germany's order-independence in a row of its own, with a budget of its own" in {
    val germany = byCountry(workflow)("germany")
    germany("order") shouldBe "convergenceGermanyOrder"
    germany.keySet should contain allOf ("orderJob", "orderSuite")
    germany("orderJob").toInt should be > (germany("orderSuite").toInt + germany("sampleSuite").toInt)
  }

  /** Every split row the lane runs holds up both ends: the full row EXCLUDES the tag and the order
   *  row runs exactly it, over the same spec. A dropped flag is silent — the claim runs twice, or
   *  stops being checked on that country at all. */
  it should "run each split country's tagged test in its order row and nowhere else in its full one" in {
    val split = RepoFile.matrixRows(workflow).filter(_.contains("order"))
    split.map(_("country")).toSet should contain allOf ("germany", "united-states")
    split.foreach { row =>
      val (full, order) = (RepoFile.commandAlias(row("cmd")), RepoFile.commandAlias(row("order")))
      withClue(s"${row("country")} full row `$full`: ") {
        full should include(s"-l $OrderTag")
        full should not include s"-n $OrderTag"
      }
      withClue(s"${row("country")} order row `$order`: ") {
        order should include(s"-n $OrderTag")
        order should not include s"-l $OrderTag"
      }
      specOf(row("order")) shouldBe specOf(row("cmd"))
    }
  }

  /** The pinned pair is the identity model's own recording, so the lane replays it hermetically —
   *  the mode whose red legs are bisected. */
  it should "replay the pinned pair hermetically" in {
    RepoFile.jobs(workflow)("leg") should include regex """mode:\s+hermetic"""
  }

  /** The overlay mode — replay an old-pipeline pair, fill its gaps live, publish them beside it —
   *  outlived its one caller once the pinned pairs became the identity model's own: a leg either
   *  replays a pair or records one, and nothing publishes an overlay. */
  it should "know no overlay mode: a leg replays a pinned pair or records one" in {
    val setup = RepoFile.read(".github/actions/convergence-setup/action.yml")
    Seq(leg, setup).foreach { yaml =>
      yaml should not include "inputs.mode == 'overlay'"
      yaml should not include "inputs.mode != 'overlay'"
    }
    leg should not include "convergence-overlay-publish"
    RepoFile.exists(".github/actions/convergence-overlay-publish") shouldBe false
  }

  /** The US sample was 79 s in front of the lane's critical path, its convergence row (run
   *  37150307201). In a row of its own it runs beside the suite: the convergence row runs ungated, the
   *  sample row speaks for the sample (its verdict, its red-sample ratchet), and what it records — in a
   *  recording, which runs the same rows — is merged into the convergence row's one publish. */
  it should "run the US sample in a row of its own, merged into the convergence row" in {
    RepoFile.matrixRows(workflow).filter(_.get("sampleRow").contains("true")).map(_("country")) shouldBe
      Seq("united-states")
    RepoFile.jobs(workflow)("leg") should include("sample-row:                    ${{ matrix.sampleRow == true }}")
    val convergence = RepoFile.block(leg, "convergence")
    convergence should include("""inputs.sample-row && ',"sample"' || ''""")
    RepoFile.step(convergence, "Run the ${{ inputs.country }} sample ahead of the suite") should include(
      "if: matrix.phase == 'sample' || (matrix.phase == 'convergence' && !inputs.sample-row && inputs.mode == 'record')\n")
    Seq("Mark the tree before the sample records into it", "Pack the sample's recordings").foreach { name =>
      withClue(s"$name, in every mode's sample row: ")(RepoFile.step(convergence, name) should not include "inputs.mode == 'record'")
    }
    val merge = RepoFile.step(convergence, "Merge the sample row's recordings")
    merge should include("if: always() && inputs.sample-row && matrix.phase == 'convergence'\n")
    withClue("the identity lane renders the sample row as `<country> / sample`: ") {
      merge should include("format('{0} / sample', inputs.country)")
    }
    convergence.indexOf("- name: Merge the sample row's recordings") should be <
      convergence.indexOf("uses: ./.github/actions/convergence-publish")
    withClue("a red sample row ratchets its findings: ") {
      convergence should include("if: always() && (matrix.phase == 'convergence' || matrix.phase == 'sample') && steps.sample.outcome == 'failure'")
    }
  }

  /** One lane: finish the run in flight, keep one newer run pending, and let each newer dispatch replace
   *  that pending one. */
  it should "hold one lane, superseding only the run that waits" in {
    val concurrency = RepoFile.block(workflow, "concurrency")
    concurrency should include("group: identity-model-convergence")
    concurrency should include("cancel-in-progress: false")
  }

  /** A red hermetic leg on main asks the bisect which commit did it — a replay that does not move. */
  it should "request a bisect for a red hermetic leg, and file an issue for a failed scheduled run" in {
    RepoFile.jobs(workflow).keySet shouldBe Set("preflight", "leg", "request-bisect", "report")
    val requests = leg.linesIterator.sliding(2).collect { case Seq(uses, cond) if uses.contains("convergence-bisect-request") => cond }.toSeq
    requests should not be empty
    requests.foreach(_ should include("inputs.mode == 'hermetic'"))
  }

  "the convergence leg" should "set no identity switch: its films are the identity model's, as production's are" in {
    leg should not include "KINOWO_IDENTITY_CUTOVER"
    leg should not include "identity-model:"
  }
}
