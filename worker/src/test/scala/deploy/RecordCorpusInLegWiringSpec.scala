package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Each country's corpus is recorded by that country's RECORDING leg, as the first thing its job
 * does — not by five `scrapes (<country>)` jobs that every leg had to wait for.
 *
 * `needs:` joins jobs, not matrix rows, so the old shape started no leg until the slowest
 * country's capture was uploaded, and paid five more runners, checkouts and JDK/sbt setups to
 * hand tarballs over through artifacts. Folded into the leg, the capture reuses the leg's setup.
 *
 * The move must not widen what the prod-Mongo route is exposed to. The scrape job ran nothing
 * after the read but a tar and two uploads; the leg runs project code for hours with a
 * write-scoped token and the TMDB/OMDb keys. So the tunnel's key reaches only the step that
 * opens it, and the tunnel is closed — key deleted, credential blanked — straight after the read.
 */
class RecordCorpusInLegWiringSpec extends AnyFlatSpec with Matchers {
  private lazy val legFile     = RepoFile.read(".github/workflows/country-convergence-leg.yml")
  private lazy val convergence = RepoFile.block(legFile, "convergence")
  private lazy val recorder    = RepoFile.read(".github/workflows/record-scrape-fixtures.yml")
  private lazy val setup       = RepoFile.read(".github/actions/convergence-setup/action.yml")

  private val Tunnel  = "Tunnel to prod Mongo"
  private val Record  = "Record ${{ inputs.country }}'s corpus"
  private val Close   = "Close the tunnel to prod Mongo"
  private val Pack    = "Pack ${{ inputs.country }}'s corpus"
  private val Sample  = "Run the ${{ inputs.country }} sample ahead of the suite"
  private val InRecordingRow = "inputs.mode == 'record' && matrix.phase == 'convergence'"

  private def at(marker: String): Int = {
    val index = convergence.indexOf(marker)
    withClue(s"`$marker` in the convergence job: ")(index should be >= 0)
    index
  }

  "the recorder" should "have no scrape jobs ahead of its legs, which every leg would wait for" in {
    RepoFile.jobs(recorder).keySet shouldBe Set("preflight", "enrichment", "report")
    RepoFile.block(recorder, "enrichment") should include("needs: preflight\n")
  }

  // The corpus is recorded by the convergence row only; a second row would open a second tunnel
  // and race the first for the same artifact name and cache key.
  it should "pass its legs no order-independence row, which would have no corpus of its own" in {
    RepoFile.block(recorder, "enrichment") should not include "order-command"
  }

  "a recording leg" should "record, close the tunnel and upload its corpus before the sample replays it" in {
    val order = Seq("uses: ./.github/actions/convergence-setup", s"- name: $Tunnel", s"- name: $Record",
      s"- name: $Close", s"- name: $Pack", "uses: actions/cache/save@", "uses: actions/upload-artifact@", s"- name: $Sample")
    order.map(at) shouldBe order.map(at).sorted
  }

  // e2e depends on worker's fixtures, never its specs, so no leg's build cache holds worker's
  // test-classes current: recording from `worker/Test` recompiled up to all 744 of them first.
  it should "record from the Fixtures configuration, which every leg's build cache holds compiled" in {
    RepoFile.step(convergence, Record) should include("""run: sbt "worker/Fixtures/runMain scripts.RecordCorpusFixture ${{ inputs.code }}"""")
    RepoFile.exists("worker/src/fixtures/scala/scripts/RecordCorpusFixture.scala") shouldBe true
    RepoFile.exists("worker/src/test/scala/scripts/RecordCorpusFixture.scala") shouldBe false
  }

  it should "run the corpus steps in a recording's convergence row only, and close the tunnel whatever happened" in {
    Seq(Tunnel, Record, Pack).foreach { name =>
      withClue(s"$name: ")(RepoFile.step(convergence, name) should include(s"if: $InRecordingRow\n"))
    }
    RepoFile.step(convergence, Close) should include(s"if: always() && $InRecordingRow\n")
    RepoFile.step(convergence, Close) should include("run: scripts/ci/close-mongo-tunnel.sh 27018")
    withClue("a failed read must stop the leg, not record a tree over yesterday's corpus: ") {
      RepoFile.step(convergence, Record) should not include "continue-on-error"
    }
  }

  // The scrape job's posture: the ssh key in the tunnel step's env and nowhere else.
  it should "hand the tunnel key to the step that opens the tunnel and to no other" in {
    val tunnel = RepoFile.step(convergence, Tunnel)
    val keyRefs = """secrets\.MONGO_CI_SSH_\w+""".r
    keyRefs.findAllIn(tunnel).toSet shouldBe Set("secrets.MONGO_CI_SSH_KEY", "secrets.MONGO_CI_SSH_HOST_KEY")
    keyRefs.findAllIn(legFile).size shouldBe keyRefs.findAllIn(tunnel).size
    """secrets\.\w+""".r.findAllIn(tunnel).toSet shouldBe Set("secrets.MONGO_CI_SSH_KEY", "secrets.MONGO_CI_SSH_HOST_KEY")
    withClue("the recorder maps the key for its preflight only: ") {
      keyRefs.findAllIn(RepoFile.jobs(recorder).removed("preflight").values.mkString("\n")) shouldBe empty
    }
  }

  // The leg's own throwaway Mongo holds 27017; the tunnel must stay off it.
  it should "open and close the tunnel on 27018, clear of the leg's own database on 27017" in {
    RepoFile.step(convergence, Tunnel) should include("run: scripts/ci/wait-for-mongo-tunnel.sh 27018")
    RepoFile.step(convergence, Sample) should include("MONGODB_URI:      mongodb://127.0.0.1:27017/")
  }

  // Hermetic legs, the provenance diff, identity-decorations and hard-clusters.sh all read these.
  it should "publish the corpus under the names its readers already read" in {
    RepoFile.step(convergence, Pack) should include("tar -czf \"scrape-archive/scrapes-${{ inputs.code }}.tar.gz\"")
    Seq("cinema-scrapes-${{ inputs.code }}.json.gz", "prod-coverage-${{ inputs.code }}.json",
        "cinema-scrapes-${{ inputs.code }}-sample.json.gz", "prod-coverage-${{ inputs.code }}-sample.json").foreach { file =>
      RepoFile.step(convergence, Pack) should include(s"\"test/resources/fixtures/corpus/$file\"")
    }
    convergence should include("path: scrape-archive\n                  key: scrape-fixtures-${{ inputs.code }}-${{ github.run_id }}")
    convergence should include("name: scrape-fixtures-${{ inputs.code }}\n                  path: scrape-archive/scrapes-${{ inputs.code }}.tar.gz")
    convergence should include("retention-days: 14")
  }

  it should "not try to download a corpus from its own run, which no job uploads before it" in {
    setup should not include "uses: actions/download-artifact"
  }
}
