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
  private lazy val verdictCallers = Seq(RepoFile.read(".github/workflows/country-convergence.yml"))

  private def directives(yaml: String): String =
    yaml.linesIterator.filterNot(_.trim.startsWith("#")).mkString("\n")

  "a verdict leg" should "be hermetic unless its caller says otherwise, and no verdict caller does" in {
    """mode:[\s\S]*?default:\s*hermetic""".r.findFirstIn(leg) shouldBe defined
    // The `mode:` KEY, not any key ending in it (a checkout's `sparse-checkout-cone-mode:`).
    verdictCallers.foreach(caller => directives(caller).linesIterator.map(_.trim).filter(_.startsWith("mode:")).toSeq shouldBe empty)
  }

  it should "hand the mode to BOTH suite steps under the name the wiring reads" in {
    val hermetic = s"${tools.ArchiveReplayWiring.HermeticVar}: $${{ inputs.mode == 'hermetic' }}"
    RepoFile.block(leg, "sample") should include(hermetic)
    RepoFile.block(leg, "convergence") should include(hermetic)
  }

  it should "never publish a tree — only a recording leg has anything to add" in {
    // The sample JOB runs only hermetic or overlay legs; a recording's sample runs inside the
    // full leg's job and is published by its one publish.
    RepoFile.block(leg, "sample") should not include "uses: ./.github/actions/convergence-publish"
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
    Seq("Restore the recorded scrape corpus", "Restore the enrichment tree and remembered answers").foreach { name =>
      val lines = RepoFile.step(setup, name).linesIterator.map(_.trim).filter(_.nonEmpty).toSeq
      val afterErrors = lines.zip(lines.drop(1)).collect { case (line, next) if line.startsWith("echo \"::error::") => next }
      withClue(s"$name: ")(afterErrors should (not be empty and contain only "exit 1"))
    }
  }

  // The sample, the full leg and the bisect of one run must replay ONE pair, or a recorder
  // pinning a new pair mid-run would split a leg across two recordings.
  it should "replay the pair its sample replayed" in {
    RepoFile.block(leg, "sample") should include("pair: ${{ steps.setup.outputs.hermetic-pair }}")
    RepoFile.block(leg, "convergence") should include("hermetic-pair: ${{ needs.sample.outputs.pair }}")
    RepoFile.block(leg, "convergence") should include("pair:           ${{ needs.sample.outputs.pair }}")
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
  // as it did through the release, and one publish carries both.
  private val SampleStep = "Run the ${{ inputs.country }} sample over the tree this leg records"

  it should "run each country's sample in the full leg's job, ahead of its suite, not as a job ahead of it" in {
    RepoFile.block(leg, "sample") should include("if: inputs.mode != 'record'")
    val convergence = RepoFile.block(leg, "convergence")
    val sample = RepoFile.step(convergence, SampleStep)
    sample should include("if: inputs.mode == 'record' && matrix.phase == 'convergence'")
    sample should include("sbt -J-Xmx${{ inputs.heap }} ${{ inputs.sample-command }} 2>&1 | tee convergence-sample.log")
    sample should include("timeout-minutes: ${{ inputs.sample-suite-timeout-minutes }}")
    sample should include(s"${tools.ArchiveReplayWiring.HermeticVar}: $${{ inputs.mode == 'hermetic' }}")
    sample should include("KINOWO_IDENTITY_LOOKUPS: ${{ inputs.identity-lookups }}")
    sample should include("KINOWO_CONVERGENCE_ENRICHMENT_FIXTURES: enrichment-${{ inputs.code }}")
    convergence.indexOf(SampleStep) should be < convergence.indexOf("- name: Run the ${{ inputs.country }} ${{ matrix.phase }} suite")
  }

  // A recording runs its full leg whatever the sample said — but a red sample must still turn the
  // leg red, AFTER the publish has kept what both recorded, and its findings still feed the ratchet.
  it should "record the full corpus past a red sample, then report the sample red" in {
    val convergence = RepoFile.block(leg, "convergence")
    RepoFile.step(convergence, SampleStep) should include("continue-on-error: true")
    val verdict = RepoFile.step(convergence, "Fail the leg on the sample's verdict")
    verdict should include("if: always() && steps.sample.outcome == 'failure'")
    convergence.indexOf("uses: ./.github/actions/convergence-publish") should be < convergence.indexOf("- name: Fail the leg on the sample's verdict")
    convergence should include("log:   convergence-sample.log")
    withClue("the full suite's report must name only the full suite's tests: ") {
      RepoFile.step(convergence, "Run the ${{ inputs.country }} ${{ matrix.phase }} suite") should include("rm -rf target/test-reports/unit")
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
    val pin = RepoFile.step(publish, "Pin this recording as the pair hermetic legs replay")
    pin should include("inputs.mode == 'record' && inputs.complete == 'true'")
    pin should include("""boot complete""")
    RepoFile.block(leg, "convergence") should include("complete: ${{ inputs.mode == 'record' && steps.suite.outcome != 'cancelled' }}")
  }

  "the auto-bisect" should "be requested only by a red hermetic pipeline leg on main, from both jobs" in {
    Seq("sample", "convergence").foreach { job =>
      withClue(s"$job: ") {
        RepoFile.block(leg, job) should include("uses: ./.github/actions/convergence-bisect-request\n" +
          "              if: failure() && inputs.mode == 'hermetic' && !inputs.identity-model && github.ref == 'refs/heads/main'")
      }
    }
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
    bisect should include(".github/scripts/convergence-bisect.sh")
    bisect should include("MAX_STEPS:       3")
    withClue("the leg must not carry a bisect job of its own: ") {
      leg.linesIterator.map(_.trim).toList should not contain "bisect:"
    }
  }
}
