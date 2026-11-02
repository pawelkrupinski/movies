package deploy

import models.Country
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * The split between convergence legs that render a VERDICT and the legs that RECORD what
 * verdicts are rendered from.
 *
 * Verdict legs (dispatched by Main, nightly, by hand on a branch) are hermetic: they replay
 * a pinned corpus + enrichment-tree pair and never touch the network, so a red leg means
 * the code changed — not that Cinemeta answered 504, Metacritic timed out, or OMDb ran out
 * of free quota. Only `Record scrape fixtures` records, and it pins the pair the verdict
 * legs replay. Each rule below is one way that split silently collapses.
 */
class HermeticConvergenceWiringSpec extends AnyFlatSpec with Matchers {
  private lazy val leg      = RepoFile.read(".github/workflows/country-convergence-leg.yml")
  private lazy val recorder = RepoFile.read(".github/workflows/record-scrape-fixtures.yml")
  private lazy val setup    = RepoFile.read(".github/actions/convergence-setup/action.yml")
  private lazy val publish  = RepoFile.read(".github/actions/convergence-publish/action.yml")
  private lazy val verdictCallers = Seq(RepoFile.read(".github/workflows/identity-model-convergence.yml"))
  /** What one subcommand of the scripts the setup and publish actions run does. */
  private def setupFunction(name: String)   = RepoFile.shellFunction(".github/scripts/convergence-setup.sh", name)
  private def publishFunction(name: String) = RepoFile.shellFunction(".github/scripts/convergence-publish.sh", name)

  private def directives(yaml: String): String =
    yaml.linesIterator.filterNot(_.trim.startsWith("#")).mkString("\n")

  // Every verdict caller replays the pinned pair: since `Record scrape fixtures` pins pairs the identity
  // model recorded (run 37216285654 on), no caller fills gaps live, so every red leg is
  // one a bisect can replay.
  "a verdict leg" should "be hermetic, and no verdict caller ask for anything else" in {
    """mode:[\s\S]*?default:\s*hermetic""".r.findFirstIn(leg) shouldBe defined
    // The `mode:` KEY, not any key ending in it (a checkout's `sparse-checkout-cone-mode:`).
    verdictCallers.foreach(caller =>
      directives(caller).linesIterator.map(_.trim).filter(_.startsWith("mode:")).map(_.split("\\s+").last).toSet should be(Set("hermetic")))
  }

  it should "hand the mode to the suite under the name the wiring reads" in {
    val hermetic = s"${tools.ArchiveReplayWiring.HermeticVar}: $${{ inputs.mode == 'hermetic' }}"
    RepoFile.step(RepoFile.block(leg, "convergence"), SuiteStep) should include(hermetic)
  }

  it should "never publish a tree — only a recording leg has anything to add" in {
    // The leg's one publish runs in every mode (a recording's sample and suite share it);
    // the action itself is what refuses to write a hermetic leg's tree.
    RepoFile.step(publish, "Pack the enrichment fixtures this leg recorded") should include("inputs.mode == 'record'")
    RepoFile.step(publish, "Publish the tree to the rolling release") should include("inputs.mode == 'record'")
  }

  // Falling back to live filling when no pair is pinned would reopen exactly the flake
  // this exists to close, and do it silently.
  it should "fail, not fall back to the network, when no recorded pair is pinned" in {
    val resolveStep = RepoFile.step(setup, "Resolve the recorded pair to replay")
    resolveStep should not include "continue-on-error"
    resolveStep should include(".github/scripts/convergence-setup.sh pair")
    val resolve = setupFunction("pair")
    resolve should include("exit 1")
    resolve should include("hermetic-$code.txt")
    RepoFile.step(publish, "Pin this recording as the pair hermetic legs replay") should include(
      """convergence-publish.sh pin "$CODE"""")
    publishFunction("pin_pair") should include("hermetic-$code.txt")
  }

  // ...nor when the pinned pair has EXPIRED. The rolling release keeps the two newest trees, so an
  // older pin (the retired identity-measure-ci.sh's RECORDING) finds nothing; the step said `::error::` and
  // exited 0, and a 2026-09-29 PL measurement "succeeded" with 0 of 9,981 listings matched.
  it should "fail, not replay nothing, when the pinned pair's corpus or tree is gone" in {
    def exitsAfterErrors(name: String, text: String, exits: String*): Unit = {
      val lines = text.linesIterator.map(_.trim).filter(_.nonEmpty).toSeq
      val afterErrors = lines.zip(lines.drop(1)).collect { case (line, next) if line.startsWith("echo \"::error::") => next }
      withClue(s"$name: ")(afterErrors.toSet shouldBe exits.toSet)
    }
    val corpusStep = RepoFile.step(setup, "Restore the recorded scrape corpus")
    corpusStep should include(".github/scripts/convergence-setup.sh corpus")
    exitsAfterErrors("the corpus", setupFunction("corpus"), "exit 1")
    // ...and the exit must END the leg: a step that continues on error swallowed it, and run
    // 36968897208 replayed an expired pin's empty tree into five legs of "not recorded" 404s.
    corpusStep.linesIterator.map(_.trim).filter(_.startsWith("continue-on-error:")).foreach(_ shouldBe "continue-on-error: ${{ inputs.mode == 'record' }}")
    // The tree is fetched in the background (restore-enrichment-tree.sh) and its verdict collected
    // where it is moved in: a missing pin exits 3 there, which only a RECORDING may pass; a release
    // that could not be read at all exits 4, which no leg may pass.
    exitsAfterErrors("the tree", RepoFile.read(".github/scripts/restore-enrichment-tree.sh"), "exit 3", "exit 4")
    val unpackStep = RepoFile.step(setup, "Unpack whichever fixtures are present")
    unpackStep should not include "continue-on-error"
    unpackStep should include(".github/scripts/convergence-setup.sh unpack")
    val unpack = setupFunction("unpack")
    unpack should include("""if [ "$tree_status" -ne 0 ] && ! { [ "$tree_status" -eq 3 ] && [ "$mode" = "record" ]; }; then""")
    unpack should include("""exit "$tree_status"""")
  }

  // The sample, the full leg and the bisect of one run must replay ONE pair, or a recorder
  // pinning a new pair mid-run would split a leg across two recordings. One job resolves it
  // once, in its setup, and every step after it — sample, suite, bisect request — reads that (the bisect's
  // with the row's own pre-suite fill after it: ConvergenceFillWiringSpec).
  it should "replay the pair its sample replayed" in {
    val convergence = RepoFile.block(leg, "convergence")
    convergence should include("- uses: ./.github/actions/convergence-setup\n              id: setup")
    convergence should include("pair:           ${{ format('{0} {1}', steps.setup.outputs.hermetic-pair, ")
    withClue("the pair comes from this job's own setup, not another job's: ") {
      convergence should not include "needs.sample"
    }
    RepoFile.read(".github/workflows/convergence-bisect.yml") should include("hermetic-pair: ${{ fromJson(needs.plan.outputs.by-country)[matrix.country].pair }}")
  }

  "the recorder" should "record every country's tree over the corpus it just captured" in {
    val enrichment = RepoFile.block(recorder, "enrichment")
    enrichment should include("uses: ./.github/workflows/country-convergence-leg.yml")
    enrichment should include("mode:           record")
    // Its own run, named inside the leg: a `with:` is baked before the run has an id (run 408).
    enrichment should not include "corpus-run:"
    RepoFile.read(".github/workflows/country-convergence-leg.yml") should include(
      "corpus-run:    ${{ inputs.corpus-run || (inputs.mode == 'record' && github.run_id || '') }}")
    val codes = """code:\s*(\w+)""".r.findAllMatchIn(RepoFile.block(enrichment, "matrix")).map(_.group(1)).toSet
    codes shouldBe Country.all.map(_.code).toSet
  }

  private val SuiteStep  = "Run the ${{ inputs.country }} ${{ matrix.phase }} suite"

  // No row runs the ~100-film sample (2026-10-14): one job, its suite the only sbt verdict in it -- and since 2026-11-02
  // the order comparison after it, reading the one-pass rows' corpora (no sample there either).
  it should "run each country's suite in one job of rows, and no sample in any" in {
    RepoFile.jobs(leg).keySet shouldBe Set("convergence", "order-compare")
    RepoFile.block(leg, "order-compare") should not include "sample-command"

    val convergence = RepoFile.block(leg, "convergence")
    convergence should not include "sample ahead of the suite"
    RepoFile.withoutComments(convergence) should not include "inputs.sample-command }} 2>&1"
  }

  // The full suite's report names only the full suite's tests.
  it should "report only its own suite's tests" in {
    RepoFile.step(RepoFile.block(leg, "convergence"), SuiteStep) should include("rm -rf target/test-reports/unit")
  }

  // The full job holds the corpus capture's budget too, so its ceiling must clear every step ceiling -- a cancelled job
  // runs its publish only inside a short grace window -- and GitHub's 360.
  it should "give each recording job a ceiling over its corpus capture and its suite together" in {
    val convergence = RepoFile.block(leg, "convergence")
    val corpus = Seq("Tunnel to prod Mongo", "Record ${{ inputs.country }}'s corpus").map { name =>
      RepoFile.step(convergence, name).linesIterator.map(_.trim)
        .collectFirst { case s"timeout-minutes: $n" => n.toInt }
        .getOrElse(fail(s"the `$name` step has no ceiling of its own"))
    }.sum
    RepoFile.matrixRows(RepoFile.block(recorder, "enrichment")).foreach { row =>
      withClue(s"${row("country")}: ") {
        row("job").toInt - corpus - row("suite").toInt should be >= 10
        row("job").toInt should be <= 360
      }
    }
  }

  // Hours of paid-for live fills: a manual re-record must queue behind a nightly one.
  it should "not cancel a recording in progress" in {
    RepoFile.block(recorder, "concurrency") should include("cancel-in-progress: false")
  }

  // Only a leg that got through the boot has recorded every request the corpus makes; the
  // spec prints the line, the publish reads it (CountryConvergenceBehaviour.BootComplete).
  it should "pin a pair only from a full recording leg whose boot completed" in {
    val pinStep  = RepoFile.step(publish, "Pin this recording as the pair hermetic legs replay")
    val treeStep = RepoFile.step(publish, "Publish the tree to the rolling release")
    pinStep should include("inputs.mode == 'record' && inputs.complete == 'true'")
    treeStep should include("COMPLETE: ${{ inputs.complete }}")
    treeStep should include("""convergence-publish.sh tree "$CODE" "$COMPLETE" "$LOG"""")
    // The decision is taken where the pinned tree is uploaded, and handed to the marker's step.
    publishFunction("publish_tree") should include("""boot complete""")
    publishFunction("publish_tree") should include("""[ "$complete" = "true" ]""")
    publishFunction("pin_pair") should include("""pinned="${KINOWO_PINNED_TREE:-}"""")
    RepoFile.block(leg, "convergence") should include("complete: ${{ inputs.mode == 'record' && steps.suite.outcome != 'cancelled' }}")
  }

  // `gh release upload` sends its files side by side, and one stream to the release ran ~22 MB/s:
  // uploading the pinned copy after the working tree was a second, sequential 18 s of the UK
  // recording's critical path (run 37071880312). One call carries both; the marker stays last.
  "the publish action" should "upload the pinned tree in the same call as the working tree, not after it" in {
    val tree = publishFunction("publish_tree")
    val pin  = publishFunction("pin_pair")
    // `${pin[@]+…}`: an empty pin under `set -u` on macOS bash 3.2 is otherwise an unbound variable.
    tree should include(""""$RELEASE" upload "$TAG" "$ARCHIVE" ${pin[@]+"${pin[@]}"} --clobber""")
    withClue("the pin step must not upload the tree a second time: ") {
      pin should not include ".tar.zst"
      pin should not include "enrichment-upload/"
    }
    pin.indexOf("hermetic-$code.txt") should be >= 0
  }

  // One request step where the two jobs each carried one: it tells the bisect whether the SAMPLE
  // was red (then the bad commit is known bad without a first replay) from the sample step itself.
  // GitHub runs a `shell: bash` step as `bash -eo pipefail`: a bare `script; code=$?` ends the step at the script's
  // non-zero exit, so a check that could not run (2) turned the row red, and a red one (1) never posted its status.
  // And a check that could not run (2) is no verdict, so never a pass: run 38533191835 named a base no leg could fetch and
  // every leg passed. It posts `error` and fails the row.
  "the identity takes check" should "read its script's exit code past bash -e, post every outcome, and fail on 2 as on 1" in {
    val step = leg.linesIterator.dropWhile(!_.contains("name: Check which listings the code moved to another film"))
      .takeWhile(l => !l.trim.startsWith("# A RECORDING whose sample")).mkString("\n")
    step should include("""code=0; scripts/ci/identity-takes-check.sh "${{ inputs.code }}" "$RUNNER_TEMP/identity-takes" || code=$?""")
    step should include("""case $code in 0) state=success ;; 1) state=failure ;; *) state=error ;; esac""")
    step should include("""exit "$code"""")
    step should not include ("exit 0 ;;")
  }

  // A row of its own, beside the suite: it reads the pinned pair and none of the suite's output, and after the suite it was
  // 4-9 minutes of every convergence row (run 38568096186). Its red asks no bisect and seeds no hard clusters (both read the
  // suite), and its row runs no suite and reports none.
  it should "run in a `takes` row of every hermetic leg, beside the suite, and nowhere else" in {
    val convergence = RepoFile.block(leg, "convergence")
    convergence should include("inputs.mode == 'hermetic' && inputs.takes-row && ',\"takes\",\"takes-base\",\"takes-drift-1\",\"takes-drift-2\"' || ''")
    convergence should include("if: ${{ !cancelled() && inputs.mode == 'hermetic' && matrix.phase == 'takes' }}")
    convergence should include("if: \"!startsWith(matrix.phase, 'takes')\"\n              shell: bash")
    convergence should include("- name: Publish convergence test report\n              if: always() && !startsWith(matrix.phase, 'takes')")
  }

  "the auto-bisect" should "be requested only by a red hermetic pipeline leg on main, naming its sample to replay" in {
    val convergence = RepoFile.block(leg, "convergence")
    "uses: ./.github/actions/convergence-bisect-request".r.findAllIn(leg).size shouldBe 1
    convergence should include("uses: ./.github/actions/convergence-bisect-request\n" +
      "              if: failure() && steps.setup.outcome == 'success' && inputs.mode == 'hermetic' && github.ref == 'refs/heads/main' && " +
      "!startsWith(matrix.phase, 'takes')")
    convergence should include("sample-command: ${{ inputs.sample-command }}")
  }

  // The ratchet reads the red suite's findings, from every row but the takes check's.
  it should "ratchet a red suite's findings" in {
    RepoFile.block(leg, "convergence") should include("uses: ./.github/actions/hard-clusters-ratchet\n" +
      "              if: failure() && !startsWith(matrix.phase, 'takes')")
  }

  // Outside the leg: inside, it would hold the suite's one-run lane for its whole budget.
  it should "run in a run of its own, only on main, inside a ceiling above its budget" in {
    val bisect = RepoFile.read(".github/workflows/convergence-bisect.yml")
    RepoFile.block(bisect, "on") should include("workflow_dispatch:")
    RepoFile.block(bisect, "plan") should include("""if [ "$branch" != main ]; then""")
    val ceiling = """timeout-minutes:\s*(\d+)""".r.findFirstMatchIn(RepoFile.block(bisect, "bisect"))
      .map(_.group(1).toInt).getOrElse(fail("no ceiling"))
    val budget = """BUDGET_MINUTES:\s*(\d+)""".r.findFirstMatchIn(bisect).map(_.group(1).toInt)
      .getOrElse(fail("no budget"))
    ceiling should be > budget
    RepoFile.withoutComments(bisect) should include("cp .github/scripts/convergence-bisect.sh")
    bisect should include("MAX_STEPS:       3")
    withClue("each step replays the checked-in (and tested) step script, copied out of the tree bisect rewrites: ") {
      RepoFile.withoutComments(bisect) should include("""export STEP_COMMAND="$tools/convergence-bisect-step.sh"""")
      RepoFile.withoutComments(bisect) should not include "<<'STEP'"
    }
    withClue("the leg must not carry a bisect job of its own: ") {
      leg.linesIterator.map(_.trim).toList should not contain "bisect:"
    }
  }
}
