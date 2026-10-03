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
  private val Restore = "Restore the corpus the convergence row records"
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

  // The corpus is recorded by the convergence row only: a second tunnel would race the first for
  // the same artifact name and cache key, and read a DIFFERENT corpus a moment later. A
  // recording's order row (the UK's, whose replays were ~6 of its 14 minutes behind a boot they
  // never touch — run 37105119296) replays the convergence row's own upload instead.
  it should "split only the UK's replays into a row of their own" in {
    RepoFile.block(recorder, "enrichment") should include("order-command:  ${{ matrix.order || '' }}")
    val ordered = recorder.linesIterator.filter(_.contains("order: ")).toSeq
    ordered.map(_.contains("code: uk,")) shouldBe Seq(true)
    ordered.head should include("cmd: convergenceUkWithoutOrder, order: convergenceUkOrder,")
  }

  it should "hand a recording's order row the corpus its convergence row records, through the run's artifact" in {
    val restore = RepoFile.step(convergence, Restore)
    restore should include("if: inputs.mode == 'record' && matrix.phase != 'convergence'\n")
    restore should include("""scripts/ci/wait-for-run-artifact.sh "scrape-fixtures-${{ inputs.code }}" "(${{ inputs.country }}) / convergence"""")
    restore should include("""gh run download "$GITHUB_RUN_ID" --name "scrape-fixtures-${{ inputs.code }}" --dir scrape-archive""")
    withClue("restored before anything replays it: ")(at(s"- name: $Restore") should be < at(s"- name: $Sample"))
  }

  // Gap-filled, as an overlay leg is: no boot of its own to have fetched what its passes ask, so
  // what the restored tree lacks is fetched ONCE between them (SharedLiveAnswers), and its sample
  // — which gates nothing in a recording and whose recordings no row would publish — skipped.
  it should "replay a recording's order row gap-filled, without a sample of its own" in {
    RepoFile.step(convergence, Sample) should include("if: inputs.mode != 'record' || matrix.phase == 'sample' || (matrix.phase == 'convergence' && !inputs.sample-row)\n")
    RepoFile.step(convergence, "Run the ${{ inputs.country }} ${{ matrix.phase }} suite") should include(
      "KINOWO_CONVERGENCE_FILL_ONLY: ${{ inputs.mode == 'overlay' || (inputs.mode == 'record' && matrix.phase == 'order-independence') }}")
  }

  "a recording leg" should "record, close the tunnel and upload its corpus before the sample replays it" in {
    val order = Seq("uses: ./.github/actions/convergence-setup", s"- name: $Tunnel", s"- name: $Record",
      s"- name: $Close", s"- name: $Pack", "uses: actions/cache/save@", "uses: actions/upload-artifact@", s"- name: $Sample")
    order.map(at) shouldBe order.map(at).sorted
  }

  // e2e depends on worker's fixtures, never its specs, so no leg's build cache holds worker's
  // test-classes current: recording from `worker/Test` recompiled up to all 744 of them first.
  it should "record from the Fixtures configuration, which every leg's build cache holds compiled" in {
    RepoFile.step(convergence, Record) should include("scripts/ci/sbt-server.sh classpath worker/Fixtures/fullClasspath")
    RepoFile.step(convergence, Record) should include("""java "${options[@]}" -cp "$classpath" scripts.RecordCorpusFixture ${{ inputs.code }}""")
    RepoFile.exists("worker/src/fixtures/scala/scripts/RecordCorpusFixture.scala") shouldBe true
    RepoFile.exists("worker/src/test/scala/scripts/RecordCorpusFixture.scala") shouldBe false
  }

  // The US recording's sample, beside its boot rather than ahead of it (a minute off the run's longest
  // leg, 37105119296): a row of its own over the same corpus and tree, whose recordings the convergence
  // row merges before it publishes — a hermetic sample leg replays the pair that publish pins.
  it should "run the US recording's sample in a row of its own, and publish what it records" in {
    val ordered = recorder.linesIterator.filter(_.contains("sampleRow: true")).toSeq
    ordered.map(_.contains("code: us,")) shouldBe Seq(true)
    RepoFile.block(recorder, "enrichment") should include("sample-row:     ${{ matrix.sampleRow == true }}")
    RepoFile.step(convergence, "Mark the tree before the sample records into it") should include(
      "if: inputs.mode == 'record' && matrix.phase == 'sample'\n")
    RepoFile.step(convergence, "Pack the sample's recordings") should include(
      ".github/scripts/sample-recordings.sh pack \"test/resources/fixtures/enrichment-${{ inputs.code }}\" \"$RUNNER_TEMP/sample-stamp\"")
    val merge = RepoFile.step(convergence, "Merge the sample row's recordings")
    merge should include("if: always() && inputs.mode == 'record' && inputs.sample-row && matrix.phase == 'convergence'\n")
    merge should include(""".github/scripts/sample-recordings.sh merge "sample-download/enrichment-sample-${{ inputs.code }}.tar.zst"""")
    withClue("merged before the tree is packed and published: ") {
      at("- name: Merge the sample row's recordings") should be < at("uses: ./.github/actions/convergence-publish")
    }
    withClue("and the sample row runs no suite: ")(
      RepoFile.step(convergence, "Run the ${{ inputs.country }} ${{ matrix.phase }} suite") should include("if: matrix.phase != 'sample'\n"))
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

  /** Matrix rows are queued in the order they are listed, so with runners short the LAST row waits
   *  longest; the US, the longest leg and so the critical path, waited 1.7 min as the last row
   *  (run 37111868620). */
  "the recorder's matrix" should "list the longest leg, the United States, first" in {
    val countries = """(?m)^\s+- \{ country: ([a-z-]+),""".r.findAllMatchIn(recorder).map(_.group(1)).toList
    countries should not be empty
    countries.head shouldBe "united-states"
  }

  /** sbt's server portfile lives in `project/target`, which the leg's build cache saves; one saved
   *  while a leg's server ran sent a later leg's client to a dead server, and Germany's corpus step
   *  hung its whole 10 minutes (run 37112719910). */
  "a leg's build cache" should "never carry sbt's server portfile into another leg" in {
    setup should include("              project/target\n              !project/target/active.json\n")
  }

  /** Recordings land every ~15 min; keeping only the newest two pinned trees per country evicted a
   *  pair before an identity measure dispatched on it could restore it (measure 37114004953). */
  "the publish" should "keep each country's newest five pinned trees" in {
    val publish = RepoFile.read(".github/actions/convergence-publish/action.yml")
    publish should include("| sort -r | tail -n +6 | cut -f2")
    RepoFile.read(".github/scripts/restore-enrichment-tree.sh") should include("(it keeps the newest five)")
  }
}
