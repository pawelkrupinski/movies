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
  private lazy val verdictCallers = Seq(
    RepoFile.read(".github/workflows/country-convergence.yml"),
    RepoFile.read(".github/workflows/us-convergence.yml"))

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
    RepoFile.block(leg, "sample") should include("if: always() && inputs.mode == 'record'")
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

  "the auto-bisect" should "be requested only by a red hermetic leg on main, from both jobs" in {
    Seq("sample", "convergence").foreach { job =>
      withClue(s"$job: ") {
        RepoFile.block(leg, job) should include("uses: ./.github/actions/convergence-bisect-request\n" +
          "              if: failure() && inputs.mode == 'hermetic' && github.ref == 'refs/heads/main'")
      }
    }
  }

  // Outside the leg: inside, it would hold the suite's one-run lane for its whole budget.
  it should "run after the suite's run, only when it failed on main, inside a ceiling above its budget" in {
    val bisect = RepoFile.read(".github/workflows/convergence-bisect.yml")
    bisect should include("workflows: [Country convergence, US convergence]")
    RepoFile.block(bisect, "plan") should include(
      "github.event.workflow_run.conclusion == 'failure' && github.event.workflow_run.head_branch == 'main'")
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
