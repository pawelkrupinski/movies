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
  private lazy val overlay  = RepoFile.read(".github/actions/convergence-overlay-publish/action.yml")

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

  /** While the pinned pair is one the OLD pipeline recorded, the model's enrichment gaps are filled live
   *  and published as an overlay beside it — so the next dispatch replays more and fetches less. */
  it should "fill and publish the model's gaps as an overlay, never into the recorded pair" in {
    RepoFile.jobs(workflow)("leg") should include regex """mode:\s+overlay"""
    overlay should include("identity-overlay-")
    val commands = overlay.linesIterator.filterNot(_.trim.startsWith("#")).mkString("\n")
    Seq("enrichment-${{ inputs.code }}.tar.", "hermetic-", "gh release delete", "delete-asset").foreach(commands should not include _)
    // ONE overlay publisher per leg, under `always()` and after the sample: the sample runs in the
    // full row's job, so a red sample — which ends that job before the suite — still publishes what
    // it fetched, as the separate sample job's own publish did.
    val publishes = leg.linesIterator.sliding(2).collect {
      case Seq(uses, cond) if uses.contains("uses: ./.github/actions/convergence-overlay-publish") => cond.trim }.toSeq
    publishes shouldBe Seq("if: always() && matrix.phase == 'convergence' && inputs.mode == 'overlay'")
    val convergence = RepoFile.block(leg, "convergence")
    convergence.indexOf("- name: Run the ${{ inputs.country }} sample ahead of the suite") should be <
      convergence.indexOf("uses: ./.github/actions/convergence-overlay-publish")
  }

  /** The US sample was 79 s in front of the lane's critical path, its convergence row (run
   *  37150307201). In a row of its own it runs beside the suite: the convergence row runs ungated, the
   *  sample row speaks for the sample (its verdict, its red-sample ratchet), and its live fills — which
   *  a slice of the corpus can ask and the whole cannot (run 37123700230's two Silent Night slugs) —
   *  are merged into the convergence row's one overlay publish rather than published a second time. */
  it should "run the US sample in a row of its own and publish its fills through the convergence row" in {
    RepoFile.matrixRows(workflow).filter(_.get("sampleRow").contains("true")).map(_("country")) shouldBe
      Seq("united-states")
    RepoFile.jobs(workflow)("leg") should include("sample-row:                    ${{ matrix.sampleRow == true }}")
    val convergence = RepoFile.block(leg, "convergence")
    convergence should include("""inputs.sample-row && ',"sample"' || ''""")
    RepoFile.step(convergence, "Run the ${{ inputs.country }} sample ahead of the suite") should include(
      "if: matrix.phase == 'sample' || (matrix.phase == 'convergence' && !inputs.sample-row)\n")
    Seq("Mark the tree before the sample records into it", "Pack the sample's recordings").foreach { name =>
      withClue(s"$name, in an overlay leg's sample row too: ")(RepoFile.step(convergence, name) should not include "inputs.mode == 'record'")
    }
    val merge = RepoFile.step(convergence, "Merge the sample row's recordings")
    merge should include("if: always() && inputs.sample-row && matrix.phase == 'convergence'\n")
    withClue("the identity lane renders the sample row as `<country> / sample`: ") {
      merge should include("format('{0} / sample', inputs.country)")
    }
    merge should include("""[ "$MODE" = overlay ] && fresh=("$RUNNER_TEMP/overlay-stamp")""")
    convergence.indexOf("- name: Merge the sample row's recordings") should be <
      convergence.indexOf("uses: ./.github/actions/convergence-overlay-publish")
    withClue("a red sample row ratchets its findings: ") {
      convergence should include("if: always() && (matrix.phase == 'convergence' || matrix.phase == 'sample') && steps.sample.outcome == 'failure'")
    }
  }

  it should "never mark a corpus green from an overlay leg" in {
    val conditions = leg.linesIterator.sliding(2).collect { case Seq(uses, cond) if uses.contains("uses: ./.github/actions/convergence-publish") => cond }.toSeq
    conditions should not be empty
    conditions.foreach(cond => cond should (include("inputs.mode == 'record'") or include("inputs.mode != 'overlay'")))
  }

  /** One lane: finish the run in flight, keep one newer run pending, and let each newer dispatch replace
   *  that pending one. */
  it should "hold one lane, superseding only the run that waits" in {
    val concurrency = RepoFile.block(workflow, "concurrency")
    concurrency should include("group: identity-model-convergence")
    concurrency should include("cancel-in-progress: false")
  }

  /** A red hermetic leg on main asks the bisect which commit did it; an overlay leg replays live fills,
   *  so it leaves no request — the bisect needs a replay that does not move. */
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
