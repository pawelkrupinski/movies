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

  /** No row runs the ~100-film sample (2026-10-14): it replays a slice of the corpus every suite replays in full, so it
   *  only ever failed sooner, and once every row fit the lane's 10-minute target it cost more than that. Each row keeps
   *  the one part of it the suite does not assert -- the corpus-shape specs, judged base against head in its takes row
   *  -- and names its sample
   *  alias for the auto-bisect, which still replays it per commit. */
  it should "run no sample, but every country's corpus-shape specs, and name each sample for the bisect" in {
    val rows = RepoFile.matrixRows(workflow)
    rows.filter(_.contains("sampleRow")) shouldBe empty
    rows.foreach { row =>
      withClue(s"${row("country")}: ") {
        row.keySet should contain allOf ("shape", "sample")
        RepoFile.read("build.sbt") should include("addCommandAlias(\"" + row("shape") + "\",")
        RepoFile.read("build.sbt") should include("addCommandAlias(\"" + row("sample") + "\",")
      }
    }
    leg should not include "sample-row"
    RepoFile.withoutComments(leg) should not include "steps.sample"
  }

  /** One lane for main's runs: finish the run in flight, keep one newer run pending, and let each newer dispatch
   *  replace that pending one. A measurement's measure/ branch (scripts/identity-measure) runs in a lane of its own,
   *  replacing nothing and queued behind no other measurement. */
  it should "hold one lane for main's runs, superseding only the run that waits, and one per measurement" in {
    val concurrency = RepoFile.block(workflow, "concurrency")
    concurrency should include(
      "group: identity-model-convergence${{ startsWith(github.ref, 'refs/heads/measure/') && format('-{0}', github.ref_name) || '' }}")
    concurrency should include("cancel-in-progress: false")
  }

  /** A red hermetic leg on main asks the bisect which commit did it — a replay that does not move. */
  it should "request a bisect for a red hermetic leg, and file an issue for a failed scheduled run" in {
    RepoFile.jobs(workflow).keySet shouldBe Set("preflight", "leg", "request-bisect", "report", "timings")
    val requests = leg.linesIterator.sliding(2).collect { case Seq(uses, cond) if uses.contains("convergence-bisect-request") => cond }.toSeq
    requests should not be empty
    requests.foreach(_ should include("inputs.mode == 'hermetic'"))
  }

  "the convergence leg" should "set no identity switch: its films are the identity model's, as production's are" in {
    leg should not include "KINOWO_IDENTITY_CUTOVER"
    leg should not include "identity-model:"
  }
}
