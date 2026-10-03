package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * `identity-model-convergence.yml`: the pipeline suite's legs asked of the identity model — a
 * measurement of a model no country serves yet, which Main dispatches beside the pipeline suite on
 * the same gate and the same superseding rules, yet must never be mistaken for, or get in the way of,
 * that suite.
 */
class IdentityModelConvergenceWiringSpec extends AnyFlatSpec with Matchers {
  private lazy val workflow = RepoFile.read(".github/workflows/identity-model-convergence.yml")
  private lazy val suite    = RepoFile.read(".github/workflows/country-convergence.yml")
  private lazy val leg      = RepoFile.read(".github/workflows/country-convergence-leg.yml")
  private lazy val main     = RepoFile.read(".github/workflows/main.yml")
  private lazy val overlay  = RepoFile.read(".github/actions/convergence-overlay-publish/action.yml")

  "the identity model convergence build" should "be dispatched by Main's convergence kick, on the pipeline suite's gate" in {
    val triggers = RepoFile.block(workflow, "on")
    triggers should include("workflow_dispatch:")
    Seq("schedule:", "push:", "pull_request:", "workflow_run:", "workflow_call:").foreach(triggers should not include _)
    RepoFile.jobs(main)("kick-convergence") should include(
      """kick-convergence.sh "$GITHUB_SHA" "$GITHUB_REF_NAME" "Country convergence" "Identity model convergence"""")
    // An edit to this workflow can change its verdict, so it is one of the paths the gate dispatches for.
    RepoFile.read(".github/convergence-paths.txt").linesIterator.map(_.trim).toSeq should contain(
      ".github/workflows/identity-model-convergence.yml")
  }

  /** The fields that say how a country's full spec is split across rows — the only thing the
   *  two lanes may disagree on. */
  private val SplitFields = Set("cmd", "order", "orderJob", "orderSuite")
  private val OrderTag    = "services.movies.OrderIndependence"
  private def specOf(alias: String): String = RepoFile.commandAlias(alias).split(" -- ").head
  private def byCountry(yaml: String): Map[String, Map[String, String]] =
    RepoFile.matrixRows(yaml).map(row => row("country") -> row).toMap

  it should "measure every country the pipeline suite does, on the same budgets and heap" in {
    RepoFile.matrixRows(workflow).map(_ -- SplitFields) shouldBe RepoFile.matrixRows(suite).map(_ -- SplitFields)
    // ...and replay the same spec for each: a split changes which row runs a test, never which tests run.
    val pipeline = byCountry(suite)
    byCountry(workflow).foreach { case (country, row) =>
      withClue(s"$country: ")(specOf(row("cmd")) shouldBe specOf(pipeline(country)("cmd")))
    }
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

  it should "decide its legs' films by the identity model" in {
    RepoFile.jobs(workflow)("leg") should include regex """identity-model:\s+true"""
  }

  /** The pipeline's pair was recorded by the PIPELINE; the model's enrichment gaps are filled live and
   *  published as an overlay beside it — so the next dispatch replays more and fetches less. */
  it should "fill and publish the model's gaps as an overlay, never into the pipeline's pair" in {
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

  it should "never mark the pipeline's corpus green from a new-model leg" in {
    val conditions = leg.linesIterator.sliding(2).collect { case Seq(uses, cond) if uses.contains("uses: ./.github/actions/convergence-publish") => cond }.toSeq
    conditions should not be empty
    conditions.foreach(cond => cond should (include("inputs.mode == 'record'") or include("inputs.mode != 'overlay'")))
  }

  /** Its own lane — neither suite queues behind, or evicts the pending run of, the other — under the
   *  pipeline suite's superseding rules: finish the run in flight, keep one newer run pending, and let
   *  each newer dispatch replace that pending one. */
  it should "hold a lane of its own, superseded the way the pipeline suite's is" in {
    val concurrency = RepoFile.block(workflow, "concurrency")
    concurrency should include("group: identity-model-convergence")
    concurrency should include("cancel-in-progress: false")
    RepoFile.block(suite, "concurrency") should include("cancel-in-progress: false")
    RepoFile.block(suite, "concurrency") should not include "identity-model-convergence"
  }

  it should "request no bisect and file no issue: a red leg is a finding about the model" in {
    RepoFile.jobs(workflow).keySet shouldBe Set("preflight", "leg")
    val requests = leg.linesIterator.sliding(2).collect { case Seq(uses, cond) if uses.contains("convergence-bisect-request") => cond }.toSeq
    requests should not be empty
    requests.foreach(_ should include("!inputs.identity-model"))
  }

  "the convergence leg" should "cut over its own country only when asked, and on every sbt step" in {
    val cutovers = leg.linesIterator.filter(_.contains("KINOWO_IDENTITY_CUTOVER:")).map(_.trim).toSeq
    cutovers should not be empty
    cutovers.distinct shouldBe Seq("KINOWO_IDENTITY_CUTOVER: ${{ inputs.identity-model && inputs.code || '' }}")
    cutovers.size shouldBe leg.linesIterator.count(_.contains("KINOWO_IDENTITY_LOOKUPS:"))
    RepoFile.block(leg, "identity-model") should include("default: false")
  }
}
