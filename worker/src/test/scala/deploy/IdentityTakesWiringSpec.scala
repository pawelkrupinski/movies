package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * `identity-takes.yml`: the takes rows of every country's leg -- the takes check, its base row, the fixed-seed drift rows
 * and the harnesses' A/A rows -- split from Identity model convergence (the owner, 2026-11-08), gating beside it. And the
 * owner's "make whatever kicks off convergence kick off both": convergence's preflight dispatches it for every run, and
 * nothing that asks for a convergence row can start one without the other.
 */
class IdentityTakesWiringSpec extends AnyFlatSpec with Matchers {
  private lazy val workflow    = RepoFile.read(".github/workflows/identity-takes.yml")
  private lazy val convergence = RepoFile.read(".github/workflows/identity-model-convergence.yml")
  private lazy val leg         = RepoFile.read(".github/workflows/country-convergence-leg.yml")

  "Identity takes" should "be started only by a dispatch: convergence's preflight's, or a takes-only or drift-only request" in {
    val triggers = RepoFile.block(workflow, "on")
    triggers should include("workflow_dispatch:")
    Seq("schedule:", "push:", "pull_request:", "workflow_run:", "workflow_call:").foreach(triggers should not include _)
  }

  /** WHATEVER STARTS CONVERGENCE STARTS TAKES: the first step after checkout and telemetry, with no `if:` (a reuse, a
   *  measurement, a hand dispatch alike), on the run's own ref, handing on its run id; and a refused dispatch fails it. */
  it should "be dispatched by every convergence run's preflight, unconditionally, first" in {
    val preflight = RepoFile.jobs(convergence)("preflight")
    val step      = RepoFile.step(preflight, "Dispatch this run's Identity takes run")
    step should include("""run: scripts/ci/dispatch-takes.sh "$GITHUB_REF_NAME" "$GITHUB_RUN_ID"""")
    step should not include "if:"
    step should not include "continue-on-error"
    RepoFile.block(preflight, "permissions") should include("actions: write")
    RepoFile.positionOf(preflight, "Dispatch this run's Identity takes run") should be <
      RepoFile.positionOf(preflight, "What every leg is handed")
    val dispatch = RepoFile.read("scripts/ci/dispatch-takes.sh")
    dispatch should include("dispatch_workflow identity-takes.yml \"$ref\"")
    dispatch should include("-f parent-run=\"$parent\"")
  }

  /** The title names the convergence run, which is how a reader goes from one to the other: convergence_reuse.py's
   *  takes_child (a reuse) and dispatch.sh's find_takes_run (identity-measure --full) read this exact title. */
  it should "carry its convergence run's id in its title, as both of its readers look it up" in {
    workflow should include(
      "run-name: ${{ inputs.parent-run != '' && format('Identity takes (convergence run {0})', inputs.parent-run) || 'Identity takes' }}")
    RepoFile.read("scripts/ci/convergence_reuse.py") should include("TAKES_TITLE = \"Identity takes (convergence run {})\"")
    RepoFile.read("scripts/ci/dispatch.sh") should include("Identity takes (convergence run $parent)")
  }

  /** NO CALLER STARTS ONLY ONE (the owner, 2026-11-08). Convergence takes no takes-only or drift-only input any more, so a
   *  dispatch asking it for takes rows alone is refused (422); and identity-takes.yml is dispatched only by convergence's
   *  preflight (dispatch-takes.sh) and by the requests that ask for no convergence row -- each of them takes-only or
   *  drift-only. A new dispatcher of either workflow fails here until it is one of these. */
  it should "be dispatched by no caller that starts convergence's rows without it" in {
    Seq("takes-only:", "drift-only:", "drift-seeds:", "takes-countries:", "drift-explore:").foreach(
      RepoFile.block(convergence, "on") should not include _)
    val sources = (RepoFile.listed("scripts")(f => f.isFile) ++ RepoFile.listed("scripts/ci")(f => f.isFile) ++
      RepoFile.listed(".github/scripts")(f => f.isFile) ++ RepoFile.workflows() ++
      RepoFile.listed(".claude/skills")(_.isDirectory).flatMap(d => Option(d.listFiles()).toSeq.flatten.filter(_.isFile)))
      .map(_.getPath).filterNot(p => p.endsWith("_test.py") || p.endsWith("-test.sh") || p.endsWith(".md"))
    val dispatches = """(gh workflow run|dispatch_workflow|identity_dispatch_measure)\s+("?\$?\{?)([\w.-]+)""".r
    def dispatchedBy(path: String): Set[String] = {
      val text = RepoFile.withoutComments(RepoFile.read(path))
      val named = dispatches.findAllMatchIn(text).map(_.group(3)).toSet
      // a dispatch through a variable names the workflow where the variable is set
      named.flatMap {
        case "workflow" => """(?m)^\s*(?:local\s+)?workflow=([\w.-]+)""".r.findAllMatchIn(text).map(_.group(1)).toSet
        case "convergence_workflow" => """(?m)^convergence_workflow=([\w.-]+)""".r.findAllMatchIn(text).map(_.group(1)).toSet
        case other => Set(other)
      }
    }
    val takesDispatchers = sources.filter(p => dispatchedBy(p).contains("identity-takes.yml")).toSet
    takesDispatchers shouldBe Set("scripts/ci/dispatch-takes.sh", "scripts/identity-measure", "scripts/identity-drift-ci",
      "scripts/ci/drift-explorer-publish.sh")
    (takesDispatchers - "scripts/ci/dispatch-takes.sh").foreach { path =>
      withClue(s"$path dispatches identity-takes.yml itself, so it must ask for takes rows alone: ") {
        RepoFile.read(path) should (include("takes-only=true") or include("drift-only=true"))
      }
    }
    withClue("--full asks for convergence's rows: it dispatches convergence, whose preflight starts the takes run: ") {
      RepoFile.read("scripts/identity-measure") should include("""identity_dispatch_measure "$convergence_workflow"""")
      RepoFile.read("scripts/identity-measure") should include("""find_takes_run "$convergence_run"""")
    }
  }

  it should "hold a lane of its own, main's runs superseding only the run that waits, one per measurement" in {
    val concurrency = RepoFile.block(workflow, "concurrency")
    concurrency should include(
      "group: identity-takes${{ github.ref != 'refs/heads/main' && format('-{0}', github.ref_name) || '' }}")
    concurrency should include("cancel-in-progress: false")
  }

  /** The same leg as convergence's, handed only the takes rows: no order or next-day command (so it creates no order comparison), the takes row on; and convergence's leg the rest, its takes row off. */
  it should "run the convergence leg with its takes rows only, as convergence runs it with every other row" in {
    val call = RepoFile.jobs(workflow)("leg")
    call should include("uses: ./.github/workflows/country-convergence-leg.yml")
    call should include regex """order-command:\s+''"""
    call should include regex """next-day-command:\s+''"""
    call should not include "takes-row:"
    RepoFile.jobs(convergence)("leg") should include regex """takes-row:\s+false"""
    RepoFile.step(RepoFile.jobs(workflow)("preflight"), "Which legs does this run start?") should include("CONVERGENCE_SUITE: takes")
    RepoFile.step(RepoFile.jobs(convergence)("preflight"), "Which legs does this run start?") should include(
      "CONVERGENCE_SUITE: convergence")
  }

  it should "judge a reuse of its own rows, of the takes run the named convergence run dispatched" in {
    val preflight = RepoFile.jobs(workflow)("preflight")
    RepoFile.step(preflight, "Does the named run's takes verdict hold for this commit?") should include(
      "convergence_reuse.py verify --suite takes --source \"$REUSE_RUN\"")
    RepoFile.step(preflight, "This run's reuse facts") should include("convergence_reuse.py facts --suite takes")
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

  // THE DRIFT HARNESS'S A/A ROW (review 2026-11-06 P5, scripts/ci/harness_aa.py): decided once, in the preflight, for
  // main's full suite alone, and handed on as drift-twice is -- a preflight output compared with 'true' in the call.
  it should "add the harness's A/A row only on a push to main that changed the harness, decided in the preflight" in {
    val preflight = RepoFile.jobs(workflow)("preflight")
    RepoFile.block(preflight, "outputs") should include("harness-aa: ${{ steps.aa.outputs.harness-aa || 'false' }}")
    val decide = RepoFile.step(preflight, "Did this push change the drift harness (its A/A row)?")
    decide should include("gh run list --workflow identity-takes.yml --branch main")
    decide should include("github.ref == 'refs/heads/main'")
    decide should include("inputs.drift-only != true")
    decide should include("python3 scripts/ci/harness_aa.py decide --head \"$GITHUB_SHA\" --since \"$since\"")
    RepoFile.jobs(workflow)("leg") should include("harness-aa:                    ${{ needs.preflight.outputs.harness-aa == 'true' }}")
    leg should include("inputs.harness-aa && ',\"takes-drift-aa-pass0\",\"takes-drift-aa-pass1\",\"takes-aa\"'")
  }

  // scripts/identity-drift-ci's pre-land check (convergence-failures-2026-10-29 §7.2): the takes-drift rows alone. The
  // matrix re-parse hands the leg's `with:` the preflight's outputs and nothing else, and a dispatch's booleans may be
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
    // ROTATING SEEDS (scripts/ci/drift_seeds.py): each row's seed, the run's rotation or the dispatch's pins
    outputs should include("drift-seed-1: ${{ steps.legs.outputs.drift-seed-1 }}")
    outputs should include("drift-seed-2: ${{ steps.legs.outputs.drift-seed-2 }}")
    call should include("drift-seed-1:                  ${{ needs.preflight.outputs.drift-seed-1 }}")
    call should include("drift-seed-2:                  ${{ needs.preflight.outputs.drift-seed-2 }}")
    val rows = RepoFile.block(leg, "convergence")
    rows should include("""fromJson(inputs.phases || inputs.drift-only && (inputs.drift-seed-1 == '' && inputs.drift-seed-2 != '' && '["takes-drift-2"]'""")
    rows should include("(!(inputs.takes-only || inputs.drift-only) || inputs.takes-countries == ''")
  }

}
