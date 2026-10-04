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

  private def directives(yaml: String): String =
    yaml.linesIterator.filterNot(_.trim.startsWith("#")).mkString("\n")

  // Every verdict caller replays the pinned pair: since `Record scrape fixtures` pins pairs the identity
  // model recorded (run 37216285654 on), no caller fills gaps live as an overlay, so every red leg is
  // one a bisect can replay.
  "a verdict leg" should "be hermetic, and no verdict caller ask for anything else" in {
    """mode:[\s\S]*?default:\s*hermetic""".r.findFirstIn(leg) shouldBe defined
    // The `mode:` KEY, not any key ending in it (a checkout's `sparse-checkout-cone-mode:`).
    verdictCallers.foreach(caller =>
      directives(caller).linesIterator.map(_.trim).filter(_.startsWith("mode:")).map(_.split("\\s+").last).toSet should be(Set("hermetic")))
  }

  it should "hand the mode to BOTH suite steps under the name the wiring reads" in {
    val hermetic = s"${tools.ArchiveReplayWiring.HermeticVar}: $${{ inputs.mode == 'hermetic' }}"
    val convergence = RepoFile.block(leg, "convergence")
    RepoFile.step(convergence, SampleStep) should include(hermetic)
    RepoFile.step(convergence, SuiteStep) should include(hermetic)
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
    val resolve = RepoFile.step(setup, "Resolve the recorded pair to replay")
    resolve should not include "continue-on-error"
    resolve should include("exit 1")
    resolve should include(s"hermetic-$${{ inputs.code }}.txt")
    RepoFile.step(publish, "Pin this recording as the pair hermetic legs replay") should include("hermetic-$code.txt")
  }

  // ...nor when the pinned pair has EXPIRED. The rolling release keeps the two newest trees, so an
  // older pin (`identity-measure-ci.sh`'s RECORDING) finds nothing; the step said `::error::` and
  // exited 0, and a 2026-09-29 PL measurement "succeeded" with 0 of 9,981 listings matched.
  it should "fail, not replay nothing, when the pinned pair's corpus or tree is gone" in {
    def exitsAfterErrors(name: String, text: String, exits: String*): Unit = {
      val lines = text.linesIterator.map(_.trim).filter(_.nonEmpty).toSeq
      val afterErrors = lines.zip(lines.drop(1)).collect { case (line, next) if line.startsWith("echo \"::error::") => next }
      withClue(s"$name: ")(afterErrors.toSet shouldBe exits.toSet)
    }
    val corpus = RepoFile.step(setup, "Restore the recorded scrape corpus")
    exitsAfterErrors("the corpus", corpus, "exit 1")
    // ...and the exit must END the leg: a step that continues on error swallowed it, and run
    // 36968897208 replayed an expired pin's empty tree into five legs of "not recorded" 404s.
    corpus.linesIterator.map(_.trim).filter(_.startsWith("continue-on-error:")).foreach(_ shouldBe "continue-on-error: ${{ inputs.mode == 'record' }}")
    // The tree is fetched in the background (restore-enrichment-tree.sh) and its verdict collected
    // where it is moved in: a missing pin exits 3 there, which only a RECORDING may pass; a release
    // that could not be read at all exits 4, which no leg may pass.
    exitsAfterErrors("the tree", RepoFile.read(".github/scripts/restore-enrichment-tree.sh"), "exit 3", "exit 4")
    val unpack = RepoFile.step(setup, "Unpack whichever fixtures are present")
    unpack should not include "continue-on-error"
    unpack should include("""if [ "$tree_status" -ne 0 ] && ! { [ "$tree_status" -eq 3 ] && [ "$MODE" = "record" ]; }; then""")
    unpack should include("""exit "$tree_status"""")
  }

  // The sample, the full leg and the bisect of one run must replay ONE pair, or a recorder
  // pinning a new pair mid-run would split a leg across two recordings. One job resolves it
  // once, in its setup, and every step after it — sample, suite, bisect request — reads that.
  it should "replay the pair its sample replayed" in {
    val convergence = RepoFile.block(leg, "convergence")
    convergence should include("- uses: ./.github/actions/convergence-setup\n              id: setup")
    convergence should include("pair:           ${{ steps.setup.outputs.hermetic-pair }}")
    withClue("the pair comes from this job's own setup, not another job's: ") {
      convergence should not include "needs.sample"
    }
    RepoFile.read(".github/workflows/convergence-bisect.yml") should include("hermetic-pair: ${{ matrix.request.pair }}")
  }

  "the recorder" should "record every country's tree over the corpus it just captured" in {
    val enrichment = RepoFile.block(recorder, "enrichment")
    enrichment should include("uses: ./.github/workflows/country-convergence-leg.yml")
    enrichment should include("mode:           record")
    enrichment should include("corpus-run:     ${{ github.run_id }}")
    val codes = """code:\s*(\w+)""".r.findAllMatchIn(RepoFile.block(enrichment, "matrix")).map(_.group(1)).toSet
    codes shouldBe Country.all.map(_.code).toSet
  }

  // A recording's sample used to be a job of its own AHEAD of the full leg: its own checkout and
  // setup (~70 s), its own pack and upload of the whole working tree (90 s for the US), and only
  // then the full leg's setup restoring that same upload — ~4 minutes of every recording's
  // critical path (run 36909637796) to hand a tree from one runner to the next. Run in the full
  // leg's job, first, over the tree on disk, the full leg replays exactly what the sample recorded,
  // as it did through the release, and one publish carries both. The hermetic and overlay legs
  // followed: their sample job cost ~1 minute of checkout and setup ahead of every full leg.
  private val SampleStep = "Run the ${{ inputs.country }} sample ahead of the suite"
  private val SuiteStep  = "Run the ${{ inputs.country }} ${{ matrix.phase }} suite"

  it should "run each country's sample in the full leg's job, ahead of its suite, not as a job ahead of it" in {
    RepoFile.jobs(leg).keySet shouldBe Set("convergence")
    val convergence = RepoFile.block(leg, "convergence")
    val sample = RepoFile.step(convergence, SampleStep)
    sample should include("sbt -J-Xmx${{ inputs.heap }} ${{ inputs.sample-command }} 2>&1 | tee convergence-sample.log")
    sample should include("timeout-minutes: ${{ inputs.sample-suite-timeout-minutes }}")
    sample should include(s"${tools.ArchiveReplayWiring.HermeticVar}: $${{ inputs.mode == 'hermetic' }}")
    sample should include("KINOWO_IDENTITY_LOOKUPS: ${{ inputs.identity-lookups }}")
    sample should include("KINOWO_CONVERGENCE_ENRICHMENT_FIXTURES: enrichment-${{ inputs.code }}")
    RepoFile.positionOf(convergence, SampleStep) should be < RepoFile.positionOf(convergence, s"- name: $SuiteStep")
  }

  // A recording runs its full leg whatever the sample said — but a red sample must still turn the
  // leg red, AFTER the publish has kept what both recorded, and its findings still feed the ratchet.
  it should "record the full corpus past a red sample, then report the sample red" in {
    val convergence = RepoFile.block(leg, "convergence")
    RepoFile.step(convergence, SampleStep) should include("continue-on-error: ${{ inputs.mode == 'record' }}")
    val verdict = RepoFile.step(convergence, "Fail the leg on the sample's verdict")
    verdict should include("if: always() && inputs.mode == 'record' && steps.sample.outcome == 'failure'")
    RepoFile.positionOf(convergence, "uses: ./.github/actions/convergence-publish") should be <
      RepoFile.positionOf(convergence, "- name: Fail the leg on the sample's verdict")
    convergence should include("log:   convergence-sample.log")
    withClue("the full suite's report must name only the full suite's tests: ") {
      RepoFile.step(convergence, SuiteStep) should include("rm -rf target/test-reports/unit")
    }
  }

  // The full job now holds the corpus capture's and the sample's budgets too, so its ceiling must
  // clear every step ceiling — a cancelled job runs its publish only inside a short grace window —
  // and GitHub's 360.
  it should "give each recording job a ceiling over its corpus capture, its sample and its suite together" in {
    val convergence = RepoFile.block(leg, "convergence")
    val corpus = Seq("Tunnel to prod Mongo", "Record ${{ inputs.country }}'s corpus").map { name =>
      RepoFile.step(convergence, name).linesIterator.map(_.trim)
        .collectFirst { case s"timeout-minutes: $n" => n.toInt }
        .getOrElse(fail(s"the `$name` step has no ceiling of its own"))
    }.sum
    RepoFile.matrixRows(RepoFile.block(recorder, "enrichment")).foreach { row =>
      withClue(s"${row("country")}: ") {
        row("job").toInt - corpus - row("suite").toInt - row("sampleSuite").toInt should be >= 10
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
    val pin  = RepoFile.step(publish, "Pin this recording as the pair hermetic legs replay")
    val tree = RepoFile.step(publish, "Publish the tree to the rolling release")
    pin should include("inputs.mode == 'record' && inputs.complete == 'true'")
    // The decision is taken where the pinned tree is uploaded, and handed to the marker's step.
    tree should include("""boot complete""")
    tree should include("""[ "${{ inputs.complete }}" = "true" ]""")
    pin should include("""pinned="${KINOWO_PINNED_TREE:-}"""")
    RepoFile.block(leg, "convergence") should include("complete: ${{ inputs.mode == 'record' && steps.suite.outcome != 'cancelled' }}")
  }

  // `gh release upload` sends its files side by side, and one stream to the release ran ~22 MB/s:
  // uploading the pinned copy after the working tree was a second, sequential 18 s of the UK
  // recording's critical path (run 37071880312). One call carries both; the marker stays last.
  "the publish action" should "upload the pinned tree in the same call as the working tree, not after it" in {
    val tree = RepoFile.step(publish, "Publish the tree to the rolling release")
    val pin  = RepoFile.step(publish, "Pin this recording as the pair hermetic legs replay")
    tree should include(""""$RELEASE" upload "$TAG" "$ARCHIVE" "${pin[@]}" --clobber""")
    withClue("the pin step must not upload the tree a second time: ") {
      pin should not include ".tar.zst"
      pin should not include "enrichment-upload/"
    }
    pin.indexOf("hermetic-$code.txt") should be >= 0
  }

  // One request step where the two jobs each carried one: it tells the bisect whether the SAMPLE
  // was red (then the bad commit is known bad without a first replay) from the sample step itself.
  "the auto-bisect" should "be requested only by a red hermetic pipeline leg on main, saying whether the sample was red" in {
    val convergence = RepoFile.block(leg, "convergence")
    "uses: ./.github/actions/convergence-bisect-request".r.findAllIn(leg).size shouldBe 1
    convergence should include("uses: ./.github/actions/convergence-bisect-request\n" +
      "              if: failure() && inputs.mode == 'hermetic' && github.ref == 'refs/heads/main'")
    convergence should include("sample-failed:  ${{ steps.sample.outcome == 'failure' }}")
  }

  // The ratchet reads ONE log per run: the sample's when the sample failed (in the old shape the
  // full leg never ran then), the suite's otherwise — and a sample artifact from one row only.
  it should "ratchet a red sample's findings and a red suite's, never the suite's for a red sample" in {
    val convergence = RepoFile.block(leg, "convergence")
    convergence should include("uses: ./.github/actions/hard-clusters-ratchet\n" +
      "              if: failure() && steps.sample.outcome != 'failure' && matrix.phase != 'sample'")
    convergence should include("uses: ./.github/actions/hard-clusters-ratchet\n" +
      "              if: always() && (matrix.phase == 'convergence' || matrix.phase == 'sample') && steps.sample.outcome == 'failure'\n" +
      "              with:\n                  code:  ${{ inputs.code }}\n                  log:   convergence-sample.log")
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
    withClue("the leg must not carry a bisect job of its own: ") {
      leg.linesIterator.map(_.trim).toList should not contain "bisect:"
    }
  }
}
