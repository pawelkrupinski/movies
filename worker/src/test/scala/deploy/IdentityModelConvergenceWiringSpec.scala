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
    germany.keySet should contain allOf ("order-job", "order-suite")
    germany("order-job").toInt should be > (germany("order-suite").toInt + germany("sample-suite").toInt)
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
   *  replace that pending one. Any other branch's run has a lane of its own (the group's exact
   *  expression: ConvergenceConcurrencyConfigSpec). */
  it should "hold one lane for main's runs, superseding only the run that waits, and one per measurement" in {
    val concurrency = RepoFile.block(workflow, "concurrency")
    concurrency should include("group: identity-model-convergence")
    concurrency should include("cancel-in-progress: false")
  }

  /** ONE HOP before the legs, not two: the seconds-long `preflight` job, needing nothing, is the only job in front of
   *  them. A second job there was a second wait for a runner in front of every leg -- 12.6 minutes of measure run
   *  968's queue (2026-10-22), for a 3-second check. */
  it should "check its secret in the one job before the legs, so they wait for one runner hop, not two" in {
    val jobs = RepoFile.jobs(workflow)
    jobs("preflight") should include("scripts/ci/preflight.sh TMDB_API_KEY")
    jobs("preflight") should include("TMDB_API_KEY: ${{ secrets.TMDB_API_KEY }}")
    jobs("preflight") should not include "needs:"
    jobs("leg") should include("needs: [ preflight ]\n")
  }

  /** A measurement's measure/ branch is deleted by its own run, last, and only on Forgejo (which cannot cancel a run
   *  through its API, so the laptop's clean-up cannot be trusted to outlive it); never a branch of anyone else. */
  it should "delete a measurement's own measure/ branch as its last job, on Forgejo only" in {
    val drop = RepoFile.jobs(workflow)("drop-measure-branch")
    drop should include("if: ${{ always() && needs.preflight.outputs.drop-branch == 'true' }}")
    RepoFile.block(RepoFile.jobs(workflow)("preflight"), "outputs") should include(
      "drop-branch: ${{ startsWith(github.ref, 'refs/heads/measure/') && github.server_url != 'https://github.com' }}")
    drop should include("BRANCH: ${{ github.ref_name }}")
  }

  /** A red hermetic leg on main asks the bisect which commit did it — a replay that does not move. */
  it should "request a bisect for a red hermetic leg, and carry no scheduled-failure report (it has no schedule)" in {
    RepoFile.jobs(workflow).keySet shouldBe Set("preflight", "leg", "request-bisect", "verdict-complete", "timings", "drop-measure-branch")
    val requests = leg.linesIterator.sliding(2).collect { case Seq(uses, cond) if uses.contains("convergence-bisect-request") => cond }.toSeq
    requests should not be empty
    requests.foreach(_ should include("inputs.mode == 'hermetic'"))
  }

  // NO FALSE GREEN (convergence-failures-2026-10-29 G7): run 1747 went green with its takes rows skipped. On main, after
  // the legs, a job reads the run's own rows and fails it when a takes or takes-drift row did not decide -- unless the
  // preflight proved a reuse, handed on as its `reused-from`.
  it should "fail a main run whose takes or takes-drift rows did not all decide, a proven reuse aside" in {
    val job = RepoFile.jobs(workflow)("verdict-complete")
    job should include("needs: [ preflight, leg ]")
    job should include("if: ${{ !cancelled() && needs.preflight.outputs.on-main == 'true' }}")
    job should include("REUSED_FROM: ${{ needs.preflight.outputs.reused-from }}")
    job should include("convergence_reuse.py complete --run \"$RUN_ID\" --sha \"$GITHUB_SHA\" --reused-from \"$REUSED_FROM\"")
  }

  // THE DETERMINISM CHECK (convergence-failures-2026-10-29 G10): the daily full run replays each drift seed twice and
  // diffs the TSVs (identity-drift-check.sh's DRIFT_TWICE) -- handed on as takes-only is, an output of the preflight.
  it should "replay each drift seed twice on the daily run, handed on to the drift step" in {
    RepoFile.block(workflow, "on") should include("drift-twice:")
    RepoFile.block(RepoFile.jobs(workflow)("preflight"), "outputs") should include("drift-twice: ${{ steps.legs.outputs.drift-twice }}")
    RepoFile.jobs(workflow)("preflight") should include("INPUT_DRIFT_TWICE: ${{ inputs.drift-twice }}")
    RepoFile.jobs(workflow)("leg") should include("drift-twice:                   ${{ needs.preflight.outputs.drift-twice == 'true' }}")
    leg should include("DRIFT_TWICE: ${{ inputs.drift-twice }}")
    RepoFile.read(".github/workflows/convergence-daily.yml") should include("-f drift-twice=true")
  }

  // scripts/identity-drift-ci's pre-land check (convergence-failures-2026-10-29 §7.2): the takes-drift rows alone. The
  // matrix re-parse hands the leg's `with:` the preflight's outputs and nothing else, and Forgejo's dispatch booleans are
  // strings, so the input travels as takes-only does: an output of the preflight, compared with 'true' in the call.
  it should "run only the takes-drift rows on a drift-only dispatch, handed on as takes-only is" in {
    RepoFile.block(workflow, "on") should include("drift-only:")
    RepoFile.block(workflow, "on") should include("drift-seeds:")
    val outputs = RepoFile.block(RepoFile.jobs(workflow)("preflight"), "outputs")
    outputs should include("drift-only: ${{ steps.legs.outputs.drift-only }}")
    outputs should include("drift-seeds: ${{ steps.legs.outputs.drift-seeds }}")
    RepoFile.jobs(workflow)("preflight") should include("INPUT_DRIFT_ONLY: ${{ inputs.drift-only }}")
    val call = RepoFile.jobs(workflow)("leg")
    call should include("drift-only:                    ${{ needs.preflight.outputs.drift-only == 'true' }}")
    call should include("drift-seeds:                   ${{ needs.preflight.outputs.drift-seeds }}")
    val rows = RepoFile.block(leg, "convergence")
    rows should include("""fromJson(inputs.drift-only && (inputs.drift-seeds == '1' && '["takes-drift-1"]'""")
    rows should include("(!(inputs.takes-only || inputs.drift-only) || inputs.takes-countries == ''")
  }

  "the convergence leg" should "set no identity switch: its films are the identity model's, as production's are" in {
    leg should not include "KINOWO_IDENTITY_CUTOVER"
    leg should not include "identity-model:"
  }
}
