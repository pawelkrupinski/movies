package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Every hermetic convergence leg publishes what its tree lacked, and the next one fetches it: the
 * suite's refetch list goes up beside the pinned pair, a `fill` job beside the suite fetches the
 * previous leg's within its minutes and publishes what it got, and every leg set up after that
 * replays the pinned tree with the pair's fills over it (docs/design/convergence-fixture-fill.md).
 * Each rule below is one way that loop silently stops, or starts costing a verdict its determinism.
 */
class ConvergenceFillWiringSpec extends AnyFlatSpec with Matchers {
  private lazy val leg     = RepoFile.read(".github/workflows/country-convergence-leg.yml")
  private lazy val setup   = RepoFile.read(".github/actions/convergence-setup/action.yml")
  private lazy val publish = RepoFile.read(".github/actions/convergence-publish/action.yml")
  private lazy val lane    = RepoFile.read(".github/workflows/identity-model-convergence.yml")
  private lazy val fill    = RepoFile.read(".github/actions/convergence-fill/action.yml")
  private lazy val fillJob = RepoFile.jobs(lane)("fill")

  private val MainOnly = "github.ref == 'refs/heads/main'"

  "a hermetic convergence row" should "publish the gaps its tree could not answer, for the next leg's fill" in {
    val convergence = RepoFile.jobs(leg)("convergence")
    convergence should include(
      "refetch-list: ${{ inputs.mode == 'hermetic' && format('test/resources/fixtures/enrichment-{0}.refetch.tsv', inputs.code) || '' }}")
    // ...the file the suite writes it to.
    tools.MissingFixtures.refetchListBeside(java.nio.file.Paths.get("test/resources/fixtures/enrichment-us")).toString shouldBe
      "test/resources/fixtures/enrichment-us.refetch.tsv"
  }

  it should "never fetch anything itself: the fill runs on a runner of its own, beside the legs" in {
    leg should not include "FillMissingFixtures"
    leg should not include "convergence-fill"
    RepoFile.withoutComments(fill) should include("scripts.FillMissingFixtures")
    fillJob should include("uses: ./.github/actions/convergence-fill")
    withClue("waiting for the legs would add the fill's minutes to the lane's: ")(fillJob should include("needs: preflight"))
    fillJob should include("code: [pl, de, uk, es, us]")
    withClue("a fill gates nothing: ")(fillJob should include("continue-on-error: true"))
  }

  "the fill" should "stop starting requests at its minute, inside the lane's 9-minute rows" in {
    fill should include("""echo "FILL_UNTIL=$(( $(date +%s) + ${{ inputs.minutes }} * 60 ))" >> "$GITHUB_ENV"""")
    RepoFile.step(fill, "Fetch them until the fill's minutes run out") should include("$FILL_UNTIL")
    """(?m)^\s+minutes:\s*(\d+)""".r.findFirstMatchIn(fillJob).map(_.group(1).toInt).getOrElse(99) should be <= 7
  }

  it should "replay nothing and compile only the fixtures, without becoming the build cache's entry" in {
    fill should include("replay: 'false'")
    setup should include("restore-only: ${{ inputs.replay != 'true' }}")
    Seq("Start MongoDB (single-node replica set, for change streams)", "Fetch the enrichment tree in the background",
        "Restore the recorded scrape corpus", "Unpack whichever fixtures are present", "Wait for MongoDB, started in the background above")
      .foreach(name => withClue(s"$name: ")(RepoFile.step(setup, name) should include("if: inputs.replay == 'true'")))
  }

  it should "publish what it fetched through the gated action, as a fill of the pair" in {
    fill should include("uses: ./.github/actions/convergence-publish")
    fill should include("fill-archive: fill-upload-source/fill.tar.zst")
  }

  // A `--clobber` deletes the asset before re-uploading it: a leg restoring in that moment finds nothing.
  "a fill and a refetch list" should "go up from main only, under a name nobody else writes" in {
    Seq("Publish the gaps this leg's tree could not answer", "Publish what this leg's fill fetched").foreach { name =>
      val step = RepoFile.step(publish, name)
      withClue(s"$name: ") {
        step should include(MainOnly)
        step should include("inputs.mode == 'hermetic'")
        step should include("$KINOWO_CONVERGENCE_PIN_CORPUS_RUN-$GITHUB_RUN_ID")
        RepoFile.withoutComments(step) should not include "--clobber"
      }
    }
  }

  "a hermetic leg's pair" should "name its fills once, at setup, and carry them to a bisect" in {
    val resolve = RepoFile.step(setup, "Resolve the recorded pair to replay")
    resolve should include("read -r corpus recorded tree fills <<< \"$pair\"")
    resolve should include("convergence-fill.sh fills")
    resolve should include("""pair="$corpus $recorded $tree${fills:+ $fills}"""")
    resolve should include("KINOWO_CONVERGENCE_FILL_ASSETS=")
    RepoFile.read(".github/scripts/restore-enrichment-tree.sh") should include("""convergence-fill.sh" unpack "$stage" "${fills[@]}"""")
  }

  "a new pin" should "prune the fills and lists of the pairs it prunes" in {
    RepoFile.step(publish, "Pin this recording as the pair hermetic legs replay") should include("^(fill|refetch)-$code-")
  }

  "the fill's release script" should "be run by CI's shell specs" in {
    RepoFile.read(".github/workflows/ci.yml") should include("bash .github/scripts/convergence-fill-test.sh")
  }
}
